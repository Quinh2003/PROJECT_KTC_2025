#!/usr/bin/env python3
"""
Advanced DataCo ETL Pipeline - Complete Implementation
=====================================================
Full enterprise ETL pipeline với transaction data generation.

Author: Senior Database Architect (20 years experience)
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import logging
from typing import Dict, List, Tuple, Optional
import math
import re

# Setup logging
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('../../logs/enterprise_pipeline.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class AdvancedDataCoPipeline:
    """
    Advanced ETL Pipeline với full transaction processing
    """
    
    def __init__(self, csv_file: str):
        self.csv_file = csv_file
        self.df = None
        self.batch_size = 1000
        
        # Mapping configurations theo DataCo_Database_Mapping.md
        self.shipping_mode_mapping = {
            'Standard Class': 'STANDARD',
            'First Class': 'FIRST_CLASS',
            'Second Class': 'SECOND_CLASS', 
            'Same Day': 'SAME_DAY'
        }
        
        self.payment_type_mapping = {
            'DEBIT': 'DEBIT',
            'TRANSFER': 'TRANSFER',
            'CASH': 'CASH'
        }
        
        # Product Status mapping theo guide
        self.product_status_mapping = {
            0: 'ACTIVE',
            1: 'INACTIVE'
        }
        
        self.order_status_mapping = {
            'COMPLETE': 1,      # COMPLETED
            'CLOSED': 1,        # COMPLETED  
            'PENDING': 2,       # PENDING
            'PROCESSING': 2,    # PENDING
            'CANCELLED': 3      # CANCELLED
        }

    def load_and_prepare_data(self) -> bool:
        """Load và prepare data với advanced processing"""
        try:
            logger.info("🔄 Loading dataset...")
            self.df = pd.read_csv(self.csv_file, encoding='latin1')
            
            # Data cleaning và preparation
            logger.info("🧹 Advanced data cleaning...")
            
            # Handle missing values strategically
            self.df['Product Description'] = self.df['Product Description'].fillna(self.df['Product Name'])
            self.df['Customer Email'] = self.df['Customer Email'].fillna('noemail@dataco.com')
            self.df['Order Zipcode'] = self.df['Order Zipcode'].fillna('00000')
            self.df['Customer Zipcode'] = self.df['Customer Zipcode'].fillna('00000')
            
            # Date conversions với error handling
            self.df['order_date_clean'] = pd.to_datetime(self.df['order date (DateOrders)'], errors='coerce')
            self.df['shipping_date_clean'] = pd.to_datetime(self.df['shipping date (DateOrders)'], errors='coerce')
            
            # Business rule validations
            self.df = self.df[self.df['Product Price'] > 0]
            self.df = self.df[self.df['Order Item Quantity'] > 0]
            self.df = self.df[self.df['Sales'] > 0]
            
            logger.info(f"✅ Data loaded: {len(self.df):,} rows ready for processing")
            return True
            
        except Exception as e:
            logger.error(f"❌ Error loading data: {e}")
            return False

    def clean_string(self, value: str, max_length: int = None) -> str:
        """Clean string values cho SQL"""
        if pd.isna(value) or value == '':
            return ''
        
        # Convert to string và clean
        clean_val = str(value).strip()
        clean_val = clean_val.replace("'", "''")  # Escape single quotes
        clean_val = re.sub(r'[^\x00-\x7F]+', '', clean_val)  # Remove non-ASCII
        
        if max_length and len(clean_val) > max_length:
            clean_val = clean_val[:max_length]
            
        return clean_val

    def format_datetime(self, dt_value) -> str:
        """Format datetime cho MySQL"""
        if pd.isna(dt_value):
            return 'NOW()'
        
        if isinstance(dt_value, str):
            try:
                dt_value = pd.to_datetime(dt_value)
            except:
                return 'NOW()'
        
        return f"'{dt_value.strftime('%Y-%m-%d %H:%M:%S')}'"

    def generate_users_sql(self) -> str:
        """Generate users SQL từ customer data với AUTO_INCREMENT"""
        logger.info("👥 Generating users SQL...")
        
        # Get unique customers
        customers = self.df[[
            'Customer Id', 'Customer Fname', 'Customer Lname', 'Customer Email'
        ]].drop_duplicates(subset=['Customer Id'])
        
        user_values = []
        # System user không có external_id
        user_values.append("(0, 'system', 'system@dataco.com', 'System User', 'hashed_password', 1, 5, NOW())")
        
        for _, row in customers.iterrows():
            external_id = int(row['Customer Id'])
            username = f"customer_{external_id}"
            email = self.clean_string(row['Customer Email'], 255)
            if email == 'XXXXXXXXX' or email == '':
                email = f"customer_{external_id}@dataco.com"
            
            fname = self.clean_string(row['Customer Fname'], 100)
            lname = self.clean_string(row['Customer Lname'], 100)
            full_name = f"{fname} {lname}".strip()
            if not full_name:
                full_name = f"Customer {external_id}"
            
            user_values.append(
                f"({external_id}, '{username}', '{email}', '{self.clean_string(full_name, 255)}', "
                f"'hashed_password', 1, 5, NOW())"
            )
        
        if user_values:
            return f"""
INSERT IGNORE INTO users (external_id, username, email, full_name, password, role_id, status_id, created_at) VALUES
{', '.join(user_values)};
"""
        return ""

    def generate_orders_sql(self) -> str:
        """Generate orders SQL theo mapping guide"""
        logger.info("📦 Generating orders SQL...")
        
        # Get unique orders với các trường được chỉ định trong mapping guide
        orders = self.df[[
            'Order Id', 'Benefit per order', 'Order Profit Per Order', 'Sales',
            'order_date_clean', 'Order Status', 'Customer Id', 'Department Id', 'Customer Segment'
        ]].drop_duplicates(subset=['Order Id'])
        
        order_values = []
        for _, row in orders.iterrows():
            external_id = int(row['Order Id'])
            # Mapping theo guide: Benefit per order → benefit_per_order
            benefit_per_order = float(row['Benefit per order']) if pd.notna(row['Benefit per order']) else 0.0
            # Mapping theo guide: Order Profit Per Order → order_profit_per_order  
            order_profit_per_order = float(row['Order Profit Per Order']) if pd.notna(row['Order Profit Per Order']) else 0.0
            # Mapping theo guide: Sales → total_amount
            total_amount = float(row['Sales']) if pd.notna(row['Sales']) else 0.0
            # Mapping theo guide: order date (DateOrders) → created_at
            created_at = self.format_datetime(row['order_date_clean'])
            
            status_id = self.order_status_mapping.get(str(row['Order Status']), 1)
            customer_external_id = int(row['Customer Id'])
            store_external_id = int(row['Department Id'])
            
            # Customer Segment → notes (theo mapping guide)
            notes = self.clean_string(row['Customer Segment'], 500) if pd.notna(row['Customer Segment']) else ''
            
            # Chỉ insert các trường được chỉ định trong mapping guide
            order_values.append(
                f"({external_id}, {benefit_per_order}, {order_profit_per_order}, {total_amount}, "
                f"{created_at}, (SELECT id FROM users WHERE external_id = {customer_external_id}), "
                f"(SELECT id FROM stores WHERE external_id = {store_external_id}), '{notes}', NOW())"
            )
        
        if order_values:
            return f"""
INSERT IGNORE INTO orders (external_id, benefit_per_order, order_profit_per_order, total_amount, created_at, created_by, store_id, notes, updated_at) VALUES
{', '.join(order_values)};
"""
        return ""

    def generate_addresses_sql(self) -> str:
        """Generate addresses SQL theo mapping guide"""
        logger.info("📍 Generating addresses SQL...")
        
        # Get unique order addresses với fields từ mapping guide
        addresses = self.df[[
            'Order Id', 'Order City', 'Order Country', 'Order State', 'Order Region',
            'Latitude', 'Longitude', 'Customer Fname', 'Customer Email', 
            'Customer Street', 'Order Zipcode', 'Customer City', 'Customer Country', 
            'Customer State', 'Customer Zipcode'
        ]].drop_duplicates(subset=['Order Id'])
        
        address_values = []
        for _, row in addresses.iterrows():
            order_external_id = int(row['Order Id'])
            
            # Mapping theo guide: Latitude → latitude, Longitude → longitude
            latitude = float(row['Latitude']) if pd.notna(row['Latitude']) else 'NULL'
            longitude = float(row['Longitude']) if pd.notna(row['Longitude']) else 'NULL'
            
            # Mapping theo guide: Order City → city (ưu tiên Order City trước Customer City)
            city = self.clean_string(row['Order City'], 100) if pd.notna(row['Order City']) else self.clean_string(row['Customer City'], 100)
            # Mapping theo guide: Order Country → country  
            country = self.clean_string(row['Order Country'], 100) if pd.notna(row['Order Country']) else self.clean_string(row['Customer Country'], 100)
            # Mapping theo guide: Order State → state
            state = self.clean_string(row['Order State'], 100) if pd.notna(row['Order State']) else self.clean_string(row['Customer State'], 100)
            # Mapping theo guide: Order Region → region
            region = self.clean_string(row['Order Region'], 100)
            
            # Mapping theo guide: Customer Fname → contact_name
            contact_name = self.clean_string(row['Customer Fname'], 255)
            # Mapping theo guide: Customer Email → contact_email
            contact_email = self.clean_string(row['Customer Email'], 255)
            if contact_email == 'XXXXXXXXX' or not contact_email:
                contact_email = 'noemail@dataco.com'
            
            # Mapping theo guide: Customer Street → address
            address = self.clean_string(row['Customer Street'], 500)
            if not address:
                address = f"Address for Order {order_external_id}"
            
            # Mapping theo guide: Order Zipcode → postal_code (ưu tiên Order Zipcode)
            postal_code = str(row['Order Zipcode']) if pd.notna(row['Order Zipcode']) else str(row['Customer Zipcode']) if pd.notna(row['Customer Zipcode']) else '00000'
            
            lat_val = f"{latitude}" if latitude != 'NULL' else 'NULL'
            lng_val = f"{longitude}" if longitude != 'NULL' else 'NULL'
            
            # Chỉ insert các trường được chỉ định trong mapping guide
            address_values.append(
                f"({lat_val}, {lng_val}, NOW(), (SELECT id FROM orders WHERE external_id = {order_external_id}), "
                f"'{postal_code}', '{city}', '{country}', '{region}', '{state}', '{address}', "
                f"'{contact_email}', '{contact_name}', 'DELIVERY', NOW())"
            )
        
        if address_values:
            return f"""
INSERT IGNORE INTO addresses (latitude, longitude, created_at, order_id, postal_code, city, country, region, state, address, contact_email, contact_name, address_type, updated_at) VALUES
{', '.join(address_values)};
"""
        return ""

    def generate_order_items_sql(self) -> str:
        """Generate order_items SQL theo mapping guide"""
        logger.info("📋 Generating order_items SQL...")
        
        order_items = self.df[[
            'Order Item Id', 'Order Item Quantity', 'Order Item Product Price',
            'Order Id', 'Product Card Id'
        ]]
        
        # Process in batches for memory efficiency
        item_values = []
        for _, row in order_items.iterrows():
            item_external_id = int(row['Order Item Id'])
            # Mapping theo guide: Order Item Quantity → quantity
            quantity = int(row['Order Item Quantity'])
            # Mapping theo guide: Order Item Product Price → unit_price
            unit_price = float(row['Order Item Product Price'])
            order_external_id = int(row['Order Id'])
            product_external_id = int(row['Product Card Id'])
            
            # Chỉ insert các trường được chỉ định trong mapping guide
            item_values.append(
                f"({item_external_id}, {quantity}, {unit_price}, NOW(), "
                f"(SELECT id FROM orders WHERE external_id = {order_external_id}), "
                f"(SELECT id FROM products WHERE external_id = {product_external_id}), NOW())"
            )
        
        if item_values:
            # Split into batches
            batch_sql = []
            for i in range(0, len(item_values), self.batch_size):
                batch = item_values[i:i+self.batch_size]
                batch_sql.append(f"""
INSERT IGNORE INTO order_items (external_id, quantity, unit_price, created_at, order_id, product_id, updated_at) VALUES
{', '.join(batch)};
""")
            return '\n'.join(batch_sql)
        return ""

    def generate_payments_sql(self) -> str:
        """Generate payments SQL theo mapping guide"""
        logger.info("💳 Generating payments SQL...")
        
        payments = self.df[[
            'Order Id', 'Type', 'Sales', 'Customer Id'
        ]].drop_duplicates(subset=['Order Id'])
        
        payment_values = []
        payment_counter = 1
        
        for _, row in payments.iterrows():
            amount = float(row['Sales'])
            # Mapping theo guide: Type → payment_method với ánh xạ giá trị chính xác
            payment_method = self.payment_type_mapping.get(str(row['Type']), 'CASH')
            order_external_id = int(row['Order Id'])
            customer_external_id = int(row['Customer Id'])
            
            # Chỉ insert trường payment_method được chỉ định trong mapping guide
            payment_values.append(
                f"({amount}, 4, NOW(), (SELECT id FROM users WHERE external_id = {customer_external_id}), "
                f"(SELECT id FROM orders WHERE external_id = {order_external_id}), NOW(), "
                f"'Transaction for Order {order_external_id}', 'TXN_{payment_counter:08d}', '{payment_method}')"
            )
            payment_counter += 1
        
        if payment_values:
            return f"""
INSERT IGNORE INTO payments (amount, status_id, created_at, created_by, order_id, updated_at, notes, transaction_id, payment_method) VALUES
{', '.join(payment_values)};
"""
        return ""

    def generate_deliveries_sql(self) -> str:
        """Generate deliveries SQL theo mapping guide"""
        logger.info("🚚 Generating deliveries SQL...")
        
        deliveries = self.df[[
            'Order Id', 'Late_delivery_risk', 'shipping_date_clean', 'Shipping Mode',
            'order_date_clean', 'Days for shipping (real)'
        ]].drop_duplicates(subset=['Order Id'])
        
        delivery_values = []
        
        for _, row in deliveries.iterrows():
            order_external_id = int(row['Order Id'])
            # Mapping theo guide: Late_delivery_risk → late_delivery_risk
            late_delivery_risk = int(row['Late_delivery_risk']) if pd.notna(row['Late_delivery_risk']) else 0
            
            # Mapping theo guide: shipping date (DateOrders) → actual_delivery_time
            actual_delivery_time = self.format_datetime(row['shipping_date_clean'])
            order_date = self.format_datetime(row['order_date_clean'])
            
            # Mapping theo guide: Shipping Mode → service_type với ánh xạ giá trị chính xác
            service_type = self.shipping_mode_mapping.get(str(row['Shipping Mode']), 'STANDARD')
            
            # Calculate pickup date (order_date + 1 day)
            try:
                if pd.notna(row['order_date_clean']):
                    pickup_dt = row['order_date_clean'] + timedelta(days=1)
                    pickup_date = f"'{pickup_dt.strftime('%Y-%m-%d %H:%M:%S')}'"
                else:
                    pickup_date = 'NOW()'
            except:
                pickup_date = 'NOW()'
            
            # Chỉ insert các trường được chỉ định trong mapping guide
            delivery_values.append(
                f"({late_delivery_risk}, {actual_delivery_time}, NOW(), "
                f"{order_date}, (SELECT id FROM orders WHERE external_id = {order_external_id}), {pickup_date}, NOW(), 1, "
                f"'Delivery for Order {order_external_id}', '{service_type}', 'ROAD')"
            )
        
        if delivery_values:
            return f"""
INSERT IGNORE INTO deliveries (late_delivery_risk, actual_delivery_time, created_at, order_date, order_id, pickup_date, updated_at, vehicle_id, delivery_notes, service_type, transport_mode) VALUES
{', '.join(delivery_values)};
"""
        return ""

    def generate_complete_sql(self) -> bool:
        """Generate complete SQL file"""
        try:
            logger.info("🏗️  Generating complete SQL file...")
            
            # Generate all SQL sections
            sql_sections = []
            
            # 1. Master data (from previous pipeline)
            sql_sections.append("-- ===== MASTER DATA =====")
            
            # Status - loại bỏ ID vì có AUTO_INCREMENT
            sql_sections.append("""
INSERT IGNORE INTO status (name, description, type, created_at) VALUES
('COMPLETED', 'Order completed successfully', 'ORDER', NOW()),
('PENDING', 'Order pending processing', 'ORDER', NOW()),
('CANCELLED', 'Order cancelled', 'ORDER', NOW()),
('PAID', 'Payment completed', 'PAYMENT', NOW()),
('ACTIVE', 'User active', 'USER', NOW()),
('AVAILABLE', 'Vehicle available', 'VEHICLE', NOW());
""")
            
            # Roles - loại bỏ ID vì có AUTO_INCREMENT
            sql_sections.append("""
INSERT IGNORE INTO roles (role_name, description, is_active, created_at) VALUES
('CUSTOMER', 'Default customer role', 1, NOW());
""")
            
            # Warehouses - loại bỏ ID vì có AUTO_INCREMENT
            sql_sections.append("""
INSERT IGNORE INTO warehouses (name, address, capacity_m3, is_active, created_at, created_by) VALUES
('Main Warehouse', 'Default Warehouse Address', 10000.00, 1, NOW(), (SELECT id FROM users WHERE external_id = 0));
""")
            
            # Vehicles - loại bỏ ID vì có AUTO_INCREMENT, và sử dụng status name thay vì hardcode ID
            sql_sections.append("""
INSERT IGNORE INTO vehicles (license_plate, vehicle_type, capacity_weight_kg, capacity_volume_m3, status_id, created_at) VALUES
('DEFAULT-001', 'TRUCK', 5000.00, 50.00, (SELECT id FROM status WHERE name = 'AVAILABLE'), NOW());
""")
            
            # 2. Categories - chỉ insert field được chỉ định trong mapping guide  
            categories = self.df[['Category Id', 'Category Name']].drop_duplicates()
            cat_values = []
            for _, row in categories.iterrows():
                external_id = int(row['Category Id'])
                # Mapping theo guide: Category Name → name
                name = self.clean_string(row['Category Name'], 255)
                cat_values.append(f"({external_id}, '{name}', NOW())")
            
            if cat_values:
                sql_sections.append(f"""
INSERT IGNORE INTO categories (external_id, name, created_at) VALUES
{', '.join(cat_values)};
""")
            
            # 3. Stores - chỉ insert field được chỉ định trong mapping guide
            stores = self.df[['Department Id', 'Department Name']].drop_duplicates()
            store_values = []
            for _, row in stores.iterrows():
                external_id = int(row['Department Id'])
                # Mapping theo guide: Department Name → store_name
                store_name = self.clean_string(row['Department Name'], 255)
                store_values.append(f"({external_id}, '{store_name}', NOW())")
            
            if store_values:
                sql_sections.append(f"""
INSERT IGNORE INTO stores (external_id, store_name, created_at) VALUES
{', '.join(store_values)};
""")
            
            # 4. Products - chỉ insert các trường trong mapping guide
            products = self.df[[
                'Product Card Id', 'Product Name', 'Product Description', 
                'Product Price', 'Product Status', 'Product Image', 'Product Category Id'
            ]].drop_duplicates(subset=['Product Card Id'])
            
            product_values = []
            for _, row in products.iterrows():
                external_id = int(row['Product Card Id'])
                name = self.clean_string(row['Product Name'], 255)
                description = self.clean_string(row['Product Description'], 1000)
                unit_price = float(row['Product Price'])  # Product Price → unit_price
                # Product Status mapping: 0 → ACTIVE, 1 → INACTIVE
                product_status = self.product_status_mapping.get(int(row['Product Status']), 'ACTIVE')
                product_image = self.clean_string(row['Product Image'], 500)
                category_external_id = int(row['Product Category Id'])
                
                # Chỉ insert các trường được chỉ định trong mapping guide
                product_values.append(
                    f"({external_id}, '{name}', '{description}', {unit_price}, '{product_status}', "
                    f"'{product_image}', (SELECT id FROM categories WHERE external_id = {category_external_id}), NOW())"
                )
            
            if product_values:
                sql_sections.append(f"""
INSERT IGNORE INTO products 
(external_id, name, description, unit_price, product_status, product_image, category_id, created_at) VALUES
{', '.join(product_values)};
""")
            
            # 5. Transaction data
            sql_sections.append("\n-- ===== TRANSACTION DATA =====")
            
            sql_sections.append(self.generate_users_sql())
            sql_sections.append(self.generate_orders_sql())
            sql_sections.append(self.generate_addresses_sql())
            sql_sections.append(self.generate_order_items_sql())
            sql_sections.append(self.generate_payments_sql())
            sql_sections.append(self.generate_deliveries_sql())
            
            # Write to file
            output_file = 'dataco_complete_import.sql'
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write("-- DataCo Supply Chain Complete Import SQL\n")
                f.write("-- Generated by Advanced ETL Pipeline\n")
                f.write(f"-- Created: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                f.write(f"-- Total Records: {len(self.df):,}\n\n")
                f.write("SET FOREIGN_KEY_CHECKS = 0;\n")
                f.write("SET SQL_MODE = 'NO_AUTO_VALUE_ON_ZERO';\n\n")
                
                for section in sql_sections:
                    f.write(section)
                    f.write("\n")
                
                f.write("\nSET FOREIGN_KEY_CHECKS = 1;\n")
                f.write("-- Import completed successfully!\n")
            
            logger.info(f"✅ Complete SQL generated: {output_file}")
            logger.info(f"📈 Stats: {len(self.df):,} records processed")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Error generating SQL: {e}")
            return False

    def run_complete_pipeline(self) -> bool:
        """Run complete advanced pipeline"""
        logger.info("🚀 Starting Advanced DataCo ETL Pipeline...")
        
        try:
            # Load and prepare data
            if not self.load_and_prepare_data():
                return False
            
            # Generate complete SQL
            if not self.generate_complete_sql():
                return False
            
            logger.info("🎉 Advanced pipeline completed successfully!")
            return True
            
        except Exception as e:
            logger.error(f"💥 Pipeline failed: {e}")
            return False

if __name__ == "__main__":
    pipeline = AdvancedDataCoPipeline('DataCoSupplyChainDataset.csv')
    
    success = pipeline.run_complete_pipeline()
    
    if success:
        print("✅ Advanced ETL Pipeline thành công!")
        print("📁 File output: dataco_complete_import.sql")
    else:
        print("❌ Advanced ETL Pipeline thất bại!")