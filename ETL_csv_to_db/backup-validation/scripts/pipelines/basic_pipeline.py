#!/usr/bin/env python3
"""
DataCo Supply Chain Data Pipeline
==================================
Enterprise-grade ETL pipeline để import dữ liệu DataCo vào database logistics.

Author: Database Expert (20 years experience)
Created: 2025
"""

import pandas as pd
import numpy as np
import mysql.connector
from datetime import datetime
import logging
from typing import Dict, List, Tuple, Optional
import os
from pathlib import Path
import json

# Cấu hình logging chuyên nghiệp
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('../../logs/basic_pipeline.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class DataCoPipeline:
    """
    Chuyên nghiệp ETL Pipeline cho DataCo Supply Chain Dataset
    """
    
    def __init__(self, csv_file: str, db_config: Dict[str, str]):
        """
        Khởi tạo pipeline với cấu hình database
        
        Args:
            csv_file: Đường dẫn đến file CSV
            db_config: Cấu hình kết nối database
        """
        self.csv_file = csv_file
        self.db_config = db_config
        self.df = None
        self.connection = None
        
        # Mapping configurations từ kinh nghiệm 20 năm
        self.shipping_mode_mapping = {
            'Standard Class': 'STANDARD',
            'First Class': 'EXPRESS', 
            'Second Class': 'PRIORITY',
            'Same Day': 'EXPRESS'
        }
        
        self.payment_type_mapping = {
            'DEBIT': 'DEBIT',
            'TRANSFER': 'BANK_TRANSFER',
            'CASH': 'CASH'
        }
        
        # Product Status mapping theo DataCo_Database_Mapping.md
        self.product_status_mapping = {
            0: 'ACTIVE',     # 0 → ACTIVE theo mapping guide
            1: 'INACTIVE'    # 1 → INACTIVE theo mapping guide
        }
        
        # Stats tracking
        self.stats = {
            'total_rows': 0,
            'processed_rows': 0,
            'errors': 0,
            'skipped_rows': 0
        }

    def connect_database(self) -> bool:
        """
        Kết nối đến database với error handling chuyên nghiệp
        """
        try:
            self.connection = mysql.connector.connect(**self.db_config)
            logger.info("✅ Kết nối database thành công")
            return True
        except mysql.connector.Error as e:
            logger.error(f"❌ Lỗi kết nối database: {e}")
            return False

    def load_dataset(self) -> bool:
        """
        Load dataset với encoding handling và error recovery
        """
        try:
            logger.info(f"📁 Đang load dataset: {self.csv_file}")
            
            # Kinh nghiệm: Luôn kiểm tra file tồn tại trước
            if not Path(self.csv_file).exists():
                raise FileNotFoundError(f"File không tồn tại: {self.csv_file}")
            
            # Load với encoding latin1 như yêu cầu
            self.df = pd.read_csv(self.csv_file, encoding='latin1')
            
            # Validation cơ bản
            if self.df.empty:
                raise ValueError("Dataset rỗng!")
                
            self.stats['total_rows'] = len(self.df)
            logger.info(f"✅ Load thành công {self.stats['total_rows']:,} rows")
            
            # Log thông tin dataset
            logger.info(f"📊 Dataset info:")
            logger.info(f"   - Columns: {len(self.df.columns)}")
            logger.info(f"   - Memory usage: {self.df.memory_usage(deep=True).sum() / 1024**2:.2f} MB")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Lỗi load dataset: {e}")
            return False

    def clean_and_validate_data(self) -> bool:
        """
        Data cleaning và validation với kinh nghiệm 20 năm
        """
        try:
            logger.info("🧹 Bắt đầu cleaning và validation dữ liệu...")
            
            original_rows = len(self.df)
            
            # 1. Xử lý missing values theo business logic
            logger.info("🔍 Xử lý missing values...")
            
            # Product Description có thể NULL -> thay thế bằng Product Name
            self.df['Product Description'] = self.df['Product Description'].fillna(
                self.df['Product Name']
            )
            
            # Customer Email missing -> tạo email dummy
            self.df['Customer Email'] = self.df['Customer Email'].fillna('noemail@dummy.com')
            
            # 2. Validation dữ liệu quan trọng
            logger.info("✅ Validation dữ liệu...")
            
            # Kiểm tra required fields
            required_fields = [
                'Order Id', 'Product Card Id', 'Category Id', 
                'Order Item Id', 'Customer Id'
            ]
            
            for field in required_fields:
                null_count = self.df[field].isnull().sum()
                if null_count > 0:
                    logger.warning(f"⚠️  {field} có {null_count} null values")
            
            # 3. Data type conversion
            logger.info("🔄 Converting data types...")
            
            # Convert dates với error handling
            date_columns = ['order date (DateOrders)', 'shipping date (DateOrders)']
            for col in date_columns:
                self.df[col] = pd.to_datetime(self.df[col], errors='coerce')
            
            # 4. Remove duplicates nếu có
            duplicates = self.df.duplicated(subset=['Order Item Id']).sum()
            if duplicates > 0:
                logger.warning(f"🔍 Tìm thấy {duplicates} duplicates, đang xóa...")
                self.df = self.df.drop_duplicates(subset=['Order Item Id'])
            
            # 5. Business rules validation
            logger.info("📋 Áp dụng business rules...")
            
            # Giá phải > 0
            invalid_prices = (self.df['Product Price'] <= 0).sum()
            if invalid_prices > 0:
                logger.warning(f"⚠️  {invalid_prices} sản phẩm có giá <= 0")
                self.df = self.df[self.df['Product Price'] > 0]
            
            # Quantity phải > 0
            invalid_qty = (self.df['Order Item Quantity'] <= 0).sum()
            if invalid_qty > 0:
                logger.warning(f"⚠️  {invalid_qty} items có quantity <= 0")
                self.df = self.df[self.df['Order Item Quantity'] > 0]
            
            cleaned_rows = len(self.df)
            removed_rows = original_rows - cleaned_rows
            
            logger.info(f"✅ Data cleaning hoàn thành:")
            logger.info(f"   - Rows gốc: {original_rows:,}")
            logger.info(f"   - Rows sau cleaning: {cleaned_rows:,}")
            logger.info(f"   - Rows bị loại bỏ: {removed_rows:,}")
            
            self.stats['processed_rows'] = cleaned_rows
            self.stats['skipped_rows'] = removed_rows
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Lỗi trong quá trình cleaning: {e}")
            return False

    def create_master_data_sql(self) -> List[str]:
        """
        Tạo SQL cho master data (categories, stores, default data)
        """
        sql_statements = []
        
        # 1. Create default statuses - sử dụng AUTO_INCREMENT
        logger.info("📝 Tạo default status data...")
        status_sql = """
        INSERT IGNORE INTO status (name, description, type, created_at) VALUES
        ('COMPLETED', 'Order completed successfully', 'ORDER', NOW()),
        ('PENDING', 'Order pending processing', 'ORDER', NOW()),
        ('CANCELLED', 'Order cancelled', 'ORDER', NOW()),
        ('PAID', 'Payment completed', 'PAYMENT', NOW()),
        ('ACTIVE', 'User active', 'USER', NOW()),
        ('AVAILABLE', 'Vehicle available', 'VEHICLE', NOW());
        """
        sql_statements.append(status_sql)
        
        # 2. Create default role - sử dụng AUTO_INCREMENT
        role_sql = """
        INSERT IGNORE INTO roles (role_name, description, is_active, created_at) VALUES
        ('CUSTOMER', 'Default customer role', 1, NOW());
        """
        sql_statements.append(role_sql)
        
        # 3. Create default warehouse - sử dụng AUTO_INCREMENT
        warehouse_sql = """
        INSERT IGNORE INTO warehouses (name, address, capacity_m3, is_active, created_at, created_by) VALUES
        ('Main Warehouse', 'Default Warehouse Address', 10000.00, 1, NOW(), 
         (SELECT id FROM users WHERE username = 'system'));
        """
        sql_statements.append(warehouse_sql)
        
        # 4. Create default vehicle - sử dụng AUTO_INCREMENT
        vehicle_sql = """
        INSERT IGNORE INTO vehicles (license_plate, vehicle_type, capacity_weight_kg, capacity_volume_m3, status_id, created_at) VALUES
        ('DEFAULT-001', 'TRUCK', 5000.00, 50.00, 
         (SELECT id FROM status WHERE name = 'AVAILABLE'), NOW());
        """
        sql_statements.append(vehicle_sql)
        
        # 5. Categories từ dataset - theo mapping guide
        logger.info("📝 Tạo categories SQL...")
        categories = self.df[['Category Id', 'Category Name']].drop_duplicates()
        
        category_values = []
        for _, row in categories.iterrows():
            external_id = int(row['Category Id'])
            # Mapping theo guide: Category Name → name
            name = str(row['Category Name']).replace("'", "''")
            category_values.append(f"({external_id}, '{name}', NOW())")
        
        if category_values:
            categories_sql = f"""
            INSERT IGNORE INTO categories (external_id, name, created_at) VALUES
            {', '.join(category_values)};
            """
            sql_statements.append(categories_sql)
        
        # 6. Stores/Departments từ dataset - theo mapping guide
        logger.info("📝 Tạo stores SQL...")
        stores = self.df[['Department Id', 'Department Name']].drop_duplicates()
        
        store_values = []
        for _, row in stores.iterrows():
            external_id = int(row['Department Id'])
            # Mapping theo guide: Department Name → store_name
            store_name = str(row['Department Name']).replace("'", "''")
            store_values.append(f"({external_id}, '{store_name}', NOW())")
        
        if store_values:
            stores_sql = f"""
            INSERT IGNORE INTO stores (external_id, store_name, created_at) VALUES
            {', '.join(store_values)};
            """
            sql_statements.append(stores_sql)
        
        return sql_statements

    def create_products_sql(self) -> str:
        """
        Tạo SQL cho products theo mapping guide
        """
        logger.info("📝 Tạo products SQL...")
        
        products = self.df[[
            'Product Card Id', 'Product Name', 'Product Description', 
            'Product Price', 'Product Status', 'Product Image', 'Product Category Id'
        ]].drop_duplicates(subset=['Product Card Id'])
        
        product_values = []
        for _, row in products.iterrows():
            external_id = int(row['Product Card Id'])
            # Mapping theo guide: Product Name → name
            name = str(row['Product Name']).strip().replace("'", "''")
            # Mapping theo guide: Product Description → description
            description = str(row['Product Description']).replace("'", "''") if pd.notna(row['Product Description']) else name
            # Mapping theo guide: Product Price → unit_price
            unit_price = float(row['Product Price'])
            # Mapping theo guide: Product Status → product_status (0 → ACTIVE, 1 → INACTIVE)
            product_status = self.product_status_mapping.get(int(row['Product Status']), 'ACTIVE')
            # Mapping theo guide: Product Image → product_image
            product_image = str(row['Product Image']).replace("'", "''") if pd.notna(row['Product Image']) else ''
            category_external_id = int(row['Product Category Id'])
            
            # Chỉ insert các trường được chỉ định trong mapping guide
            product_values.append(
                f"({external_id}, '{name}', '{description}', {unit_price}, '{product_status}', "
                f"'{product_image}', (SELECT id FROM categories WHERE external_id = {category_external_id}), NOW())"
            )
        
        if product_values:
            return f"""
            INSERT IGNORE INTO products 
            (external_id, name, description, unit_price, product_status, product_image, category_id, created_at) 
            VALUES {', '.join(product_values)};
            """
        return ""

    def generate_all_sql(self) -> bool:
        """
        Generate tất cả SQL statements theo thứ tự dependency
        """
        try:
            logger.info("🏗️  Bắt đầu generate SQL statements...")
            
            all_sql = []
            
            # 1. Master data
            master_sqls = self.create_master_data_sql()
            all_sql.extend(master_sqls)
            
            # 2. Products
            products_sql = self.create_products_sql()
            if products_sql:
                all_sql.append(products_sql)
            
            # 3. Users (customers) - tạo user mặc định với AUTO_INCREMENT và lookup
            users_sql = """
            INSERT IGNORE INTO users (username, email, full_name, password, role_id, status_id, created_at) VALUES
            ('system', 'system@dataco.com', 'System User', 'hashed_password', 
             (SELECT id FROM roles WHERE role_name = 'CUSTOMER'), 
             (SELECT id FROM status WHERE name = 'ACTIVE'), NOW());
            """
            all_sql.append(users_sql)
            
            # 4. Orders, order_items, addresses, payments, deliveries
            # Sẽ implement trong phần tiếp theo
            
            # Ghi ra file
            output_file = 'dataco_import.sql'
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write("-- DataCo Supply Chain Data Import SQL\n")
                f.write("-- Generated by Professional ETL Pipeline\n")
                f.write(f"-- Created: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
                f.write("SET FOREIGN_KEY_CHECKS = 0;\n\n")
                
                for i, sql in enumerate(all_sql, 1):
                    f.write(f"-- Statement {i}\n")
                    f.write(sql)
                    f.write("\n\n")
                
                f.write("SET FOREIGN_KEY_CHECKS = 1;\n")
            
            logger.info(f"✅ SQL được tạo thành công: {output_file}")
            logger.info(f"📊 Tổng cộng {len(all_sql)} SQL statements")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Lỗi generate SQL: {e}")
            return False

    def run_pipeline(self) -> bool:
        """
        Chạy toàn bộ pipeline ETL
        """
        logger.info("🚀 Bắt đầu DataCo ETL Pipeline...")
        
        try:
            # Step 1: Load dataset
            if not self.load_dataset():
                return False
            
            # Step 2: Clean và validate
            if not self.clean_and_validate_data():
                return False
            
            # Step 3: Generate SQL
            if not self.generate_all_sql():
                return False
            
            # Step 4: Kết nối DB và execute (optional)
            # if self.connect_database():
            #     self.execute_sql_statements()
            
            # Final report
            logger.info("🎉 Pipeline hoàn thành thành công!")
            logger.info(f"📈 Thống kê:")
            for key, value in self.stats.items():
                logger.info(f"   - {key}: {value:,}")
            
            return True
            
        except Exception as e:
            logger.error(f"💥 Pipeline thất bại: {e}")
            return False

# Configuration
DB_CONFIG = {
    'host': 'localhost',
    'user': 'root',
    'password': 'somethings',
    'database': 'fastroute',
    'charset': 'utf8mb4'
}

if __name__ == "__main__":
    # Chạy pipeline
    pipeline = DataCoPipeline(
        csv_file='DataCoSupplyChainDataset.csv',
        db_config=DB_CONFIG
    )
    
    success = pipeline.run_pipeline()
    
    if success:
        print("✅ ETL Pipeline thành công!")
    else:
        print("❌ ETL Pipeline thất bại!")