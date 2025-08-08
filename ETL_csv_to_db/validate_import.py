#!/usr/bin/env python3
"""
DataCo Import Validation Script
===============================
Script validation để kiểm tra SQL import trước khi chạy.

Author: Senior Database Expert
"""

import re
import pandas as pd
from collections import Counter
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class ImportValidator:
    """Validator cho DataCo import SQL"""
    
    def __init__(self, sql_file: str, csv_file: str):
        self.sql_file = sql_file
        self.csv_file = csv_file
        self.df = None
        
    def load_csv_data(self) -> bool:
        """Load CSV data để so sánh"""
        try:
            self.df = pd.read_csv(self.csv_file, encoding='latin1')
            logger.info(f"✅ Loaded CSV: {len(self.df):,} rows")
            return True
        except Exception as e:
            logger.error(f"❌ Error loading CSV: {e}")
            return False
    
    def validate_sql_syntax(self) -> bool:
        """Basic SQL syntax validation với AUTO_INCREMENT checks"""
        try:
            logger.info("🔍 Validating SQL syntax...")
            
            with open(self.sql_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Check basic syntax issues
            issues = []
            
            # Check for unmatched quotes
            single_quotes = content.count("'") - content.count("\\'")
            if single_quotes % 2 != 0:
                issues.append("Unmatched single quotes detected")
            
            # Check for common SQL issues
            if 'INSERT' not in content:
                issues.append("No INSERT statements found")
            
            # Check for AUTO_INCREMENT conflicts
            auto_increment_tables = ['status', 'roles', 'warehouses', 'vehicles', 'users', 'orders', 'order_items', 'addresses', 'payments', 'deliveries']
            for table in auto_increment_tables:
                # Check if inserting into id column directly (not external_id)
                pattern = f"INSERT.*INTO {table}.*\\(.*\\bid\\b.*,"
                if re.search(pattern, content, re.IGNORECASE):
                    # Kiểm tra xem có phải external_id không
                    external_pattern = f"INSERT.*INTO {table}.*\\(.*external_id.*,"
                    if not re.search(external_pattern, content, re.IGNORECASE):
                        issues.append(f"Table '{table}' có AUTO_INCREMENT, không nên insert vào id column")
            
            # Check for proper statement endings
            insert_count = content.count('INSERT')
            semicolon_count = content.count(';')
            
            if insert_count > semicolon_count:
                issues.append(f"Missing semicolons: {insert_count} INSERTs vs {semicolon_count} semicolons")
            
            if issues:
                logger.warning("⚠️  SQL Syntax Issues:")
                for issue in issues:
                    logger.warning(f"   - {issue}")
                return False
            
            logger.info("✅ SQL syntax validation passed")
            return True
            
        except Exception as e:
            logger.error(f"❌ SQL validation error: {e}")
            return False
    
    def validate_data_counts(self) -> bool:
        """Validate data counts between CSV and SQL"""
        try:
            logger.info("📊 Validating data counts...")
            
            # CSV counts
            csv_stats = {
                'total_rows': len(self.df),
                'unique_orders': self.df['Order Id'].nunique(),
                'unique_products': self.df['Product Card Id'].nunique(),
                'unique_categories': self.df['Category Id'].nunique(),
                'unique_customers': self.df['Customer Id'].nunique(),
                'unique_departments': self.df['Department Id'].nunique()
            }
            
            # Read SQL file và count INSERT statements
            with open(self.sql_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Extract INSERT counts from SQL
            sql_stats = {}
            
            # Count order_items (should match total CSV rows)
            order_items_match = re.search(r'INSERT IGNORE INTO order_items.*?VALUES\s+(.*?);', content, re.DOTALL)
            if order_items_match:
                values_str = order_items_match.group(1)
                order_items_count = values_str.count('(')
                sql_stats['order_items'] = order_items_count
            
            # Count products
            products_match = re.search(r'INSERT IGNORE INTO products.*?VALUES\s+(.*?);', content, re.DOTALL)
            if products_match:
                values_str = products_match.group(1)
                products_count = values_str.count('(')
                sql_stats['products'] = products_count
            
            # Count categories
            categories_match = re.search(r'INSERT IGNORE INTO categories.*?VALUES\s+(.*?);', content, re.DOTALL)
            if categories_match:
                values_str = categories_match.group(1)
                categories_count = values_str.count('(')
                sql_stats['categories'] = categories_count
            
            # Count orders
            orders_match = re.search(r'INSERT IGNORE INTO orders.*?VALUES\s+(.*?);', content, re.DOTALL)
            if orders_match:
                values_str = orders_match.group(1)
                orders_count = values_str.count('(')
                sql_stats['orders'] = orders_count
            
            # Validation report
            logger.info("📈 Data Count Validation Report:")
            logger.info(f"   CSV Total Rows: {csv_stats['total_rows']:,}")
            logger.info(f"   SQL Order Items: {sql_stats.get('order_items', 0):,}")
            
            logger.info(f"   CSV Unique Orders: {csv_stats['unique_orders']:,}")
            logger.info(f"   SQL Orders: {sql_stats.get('orders', 0):,}")
            
            logger.info(f"   CSV Unique Products: {csv_stats['unique_products']:,}")
            logger.info(f"   SQL Products: {sql_stats.get('products', 0):,}")
            
            logger.info(f"   CSV Unique Categories: {csv_stats['unique_categories']:,}")
            logger.info(f"   SQL Categories: {sql_stats.get('categories', 0):,}")
            
            # Validation checks
            validation_passed = True
            
            if sql_stats.get('order_items', 0) != csv_stats['total_rows']:
                logger.warning(f"⚠️  Order items count mismatch!")
                validation_passed = False
            
            if sql_stats.get('orders', 0) != csv_stats['unique_orders']:
                logger.warning(f"⚠️  Orders count mismatch!")
                validation_passed = False
            
            if sql_stats.get('products', 0) != csv_stats['unique_products']:
                logger.warning(f"⚠️  Products count mismatch!")
                validation_passed = False
            
            if sql_stats.get('categories', 0) != csv_stats['unique_categories']:
                logger.warning(f"⚠️  Categories count mismatch!")
                validation_passed = False
            
            if validation_passed:
                logger.info("✅ Data count validation passed")
            
            return validation_passed
            
        except Exception as e:
            logger.error(f"❌ Data count validation error: {e}")
            return False
    
    def validate_business_rules(self) -> bool:
        """Validate business rules"""
        try:
            logger.info("📋 Validating business rules...")
            
            validation_passed = True
            
            # Check for negative prices
            negative_prices = (self.df['Product Price'] < 0).sum()
            if negative_prices > 0:
                logger.warning(f"⚠️  Found {negative_prices} products with negative prices")
                validation_passed = False
            
            # Check for zero quantities
            zero_quantities = (self.df['Order Item Quantity'] <= 0).sum()
            if zero_quantities > 0:
                logger.warning(f"⚠️  Found {zero_quantities} items with zero/negative quantities")
                validation_passed = False
            
            # Check for missing critical data
            critical_fields = ['Order Id', 'Product Card Id', 'Category Id', 'Customer Id']
            for field in critical_fields:
                null_count = self.df[field].isnull().sum()
                if null_count > 0:
                    logger.warning(f"⚠️  {field} has {null_count} null values")
                    validation_passed = False
            
            # Check date consistency
            date_issues = 0
            try:
                order_dates = pd.to_datetime(self.df['order date (DateOrders)'], errors='coerce')
                shipping_dates = pd.to_datetime(self.df['shipping date (DateOrders)'], errors='coerce')
                
                # Shipping date should be >= order date
                invalid_dates = (shipping_dates < order_dates).sum()
                if invalid_dates > 0:
                    logger.warning(f"⚠️  Found {invalid_dates} records where shipping date < order date")
                    date_issues += invalid_dates
            except:
                logger.warning("⚠️  Could not validate date consistency")
                validation_passed = False
            
            if validation_passed and date_issues == 0:
                logger.info("✅ Business rules validation passed")
            
            return validation_passed
            
        except Exception as e:
            logger.error(f"❌ Business rules validation error: {e}")
            return False
    
    def generate_validation_report(self) -> str:
        """Generate comprehensive validation report"""
        report = f"""
# DataCo Import Validation Report
Generated: {pd.Timestamp.now().strftime('%Y-%m-%d %H:%M:%S')}

## Dataset Overview
- **Total Records**: {len(self.df):,}
- **Unique Orders**: {self.df['Order Id'].nunique():,}
- **Unique Products**: {self.df['Product Card Id'].nunique():,}
- **Unique Categories**: {self.df['Category Id'].nunique():,}
- **Unique Customers**: {self.df['Customer Id'].nunique():,}
- **Date Range**: {self.df['order date (DateOrders)'].min()} to {self.df['order date (DateOrders)'].max()}

## Data Quality Metrics
- **Missing Product Descriptions**: {self.df['Product Description'].isnull().sum():,}
- **Missing Customer Emails**: {(self.df['Customer Email'] == 'XXXXXXXXX').sum():,}
- **Zero Prices**: {(self.df['Product Price'] == 0).sum():,}
- **Negative Sales**: {(self.df['Sales'] < 0).sum():,}

## Top Categories
{self.df['Category Name'].value_counts().head(10).to_string()}

## Top Products
{self.df['Product Name'].value_counts().head(10).to_string()}

## Recommendations
1. ✅ Verify all foreign key relationships before import
2. ✅ Run import in test environment first
3. ✅ Monitor for constraint violations during import
4. ✅ Create backup before import
5. ✅ Validate record counts after import
        """
        
        return report
    
    def run_full_validation(self) -> bool:
        """Run complete validation suite"""
        logger.info("🚀 Starting DataCo Import Validation...")
        
        try:
            # Load data
            if not self.load_csv_data():
                return False
            
            # Run validations
            validations = [
                self.validate_sql_syntax(),
                self.validate_data_counts(), 
                self.validate_business_rules()
            ]
            
            # Generate report
            report = self.generate_validation_report()
            with open('validation_report.md', 'w', encoding='utf-8') as f:
                f.write(report)
            
            all_passed = all(validations)
            
            if all_passed:
                logger.info("🎉 All validations passed! Ready for import.")
            else:
                logger.warning("⚠️  Some validations failed. Review before import.")
            
            logger.info("📄 Validation report saved: validation_report.md")
            
            return all_passed
            
        except Exception as e:
            logger.error(f"💥 Validation failed: {e}")
            return False

if __name__ == "__main__":
    validator = ImportValidator(
        sql_file='dataco_complete_import.sql',
        csv_file='DataCoSupplyChainDataset.csv'
    )
    
    success = validator.run_full_validation()
    
    if success:
        print("✅ Validation thành công! Sẵn sàng import.")
    else:
        print("❌ Validation có vấn đề. Xem lại trước khi import.")