#!/usr/bin/env python3
"""
Configuration file cho DataCo ETL Pipeline
==========================================
Cấu hình database và các tham số pipeline.
"""

import os
from typing import Dict, Any

# Database Configuration
DATABASE_CONFIG = {
    'host': os.getenv('DB_HOST', 'localhost'),
    'port': int(os.getenv('DB_PORT', 3306)),
    'user': os.getenv('DB_USER', 'fastroute_user'),
    'password': os.getenv('DB_PASSWORD', 'fastroute_password'),
    'database': os.getenv('DB_NAME', 'fasteroute'),
    'charset': 'utf8mb4',
    'autocommit': True,
    'raise_on_warnings': True
}

# File paths
CSV_FILE = 'DataCoSupplyChainDataset.csv'
OUTPUT_SQL_FILE = 'dataco_import.sql'
LOG_FILE = 'data_pipeline.log'

# Pipeline settings
PIPELINE_CONFIG = {
    'batch_size': 1000,  # Số records xử lý mỗi batch
    'max_retries': 3,    # Số lần retry khi lỗi
    'timeout_seconds': 300,  # Timeout cho mỗi operation
    'enable_validation': True,
    'create_backup': True
}

# Business Rules
BUSINESS_RULES = {
    'min_price': 0.01,
    'max_price': 999999.99,
    'min_quantity': 1,
    'max_quantity': 10000,
    'required_fields': [
        'Order Id', 'Product Card Id', 'Category Id',
        'Order Item Id', 'Customer Id'
    ]
}

# Data Quality thresholds
DATA_QUALITY = {
    'max_null_percentage': 5.0,     # % null values cho phép
    'max_duplicate_percentage': 1.0, # % duplicates cho phép
    'min_data_completeness': 95.0    # % completeness yêu cầu
}