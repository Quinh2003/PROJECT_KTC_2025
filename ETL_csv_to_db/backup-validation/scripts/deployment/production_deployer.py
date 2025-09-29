#!/usr/bin/env python3
"""
DataCo Deployment Script
========================
Script cuối cùng để deploy import vào database thực tế.

Usage: python3 deploy_import.py [--dry-run] [--batch-size=1000]
"""

import argparse
import mysql.connector
import logging
import time
from pathlib import Path
import sys
import re

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class DataCoDeployer:
    """Production deployment cho DataCo import"""
    
    def __init__(self, db_config: dict, sql_file: str, dry_run: bool = False):
        self.db_config = db_config
        self.sql_file = sql_file
        self.dry_run = dry_run
        self.connection = None
        self.cursor = None
        
    def connect_database(self) -> bool:
        """Kết nối database với production settings"""
        try:
            self.connection = mysql.connector.connect(
                **self.db_config,
                autocommit=False,  # Transaction mode
                raise_on_warnings=True
            )
            self.cursor = self.connection.cursor()
            
            # Set session variables for import
            session_vars = [
                "SET SESSION foreign_key_checks = 0",
                "SET SESSION unique_checks = 0", 
                "SET SESSION sql_mode = 'NO_AUTO_VALUE_ON_ZERO'",
                "SET SESSION autocommit = 0"
            ]
            
            for var in session_vars:
                self.cursor.execute(var)
            
            logger.info("✅ Database connected successfully")
            return True
            
        except mysql.connector.Error as e:
            logger.error(f"❌ Database connection failed: {e}")
            return False
    
    def verify_schema(self) -> bool:
        """Kiểm tra schema có đúng không"""
        try:
            logger.info("🔍 Verifying database schema...")
            
            required_tables = [
                'categories', 'stores', 'products', 'users', 'roles',
                'orders', 'order_items', 'addresses', 'payments', 
                'deliveries', 'status', 'warehouses', 'vehicles'
            ]
            
            self.cursor.execute("SHOW TABLES")
            existing_tables = [table[0] for table in self.cursor.fetchall()]
            
            missing_tables = set(required_tables) - set(existing_tables)
            if missing_tables:
                logger.error(f"❌ Missing tables: {missing_tables}")
                return False
            
            logger.info("✅ All required tables exist")
            return True
            
        except Exception as e:
            logger.error(f"❌ Schema verification failed: {e}")
            return False
    
    def check_existing_data(self) -> dict:
        """Kiểm tra dữ liệu đã có trong database"""
        try:
            logger.info("📊 Checking existing data...")
            
            tables_to_check = ['categories', 'products', 'orders', 'order_items']
            existing_counts = {}
            
            for table in tables_to_check:
                self.cursor.execute(f"SELECT COUNT(*) FROM {table}")
                count = self.cursor.fetchone()[0]
                existing_counts[table] = count
                
                if count > 0:
                    logger.warning(f"⚠️  Table {table} already has {count} records")
            
            return existing_counts
            
        except Exception as e:
            logger.error(f"❌ Data check failed: {e}")
            return {}
    
    def execute_import(self) -> bool:
        """Execute SQL import với transaction management"""
        try:
            if self.dry_run:
                logger.info("🧪 DRY RUN MODE - No data will be imported")
                return self.validate_sql_file()
            
            logger.info("🚀 Starting database import...")
            
            # Read SQL file
            with open(self.sql_file, 'r', encoding='utf-8') as f:
                sql_content = f.read()
            
            # Split into individual statements
            statements = [stmt.strip() for stmt in sql_content.split(';') if stmt.strip()]
            
            logger.info(f"📋 Found {len(statements)} SQL statements")
            
            # Execute in transaction
            try:
                self.connection.start_transaction()
                
                success_count = 0
                for i, statement in enumerate(statements, 1):
                    if statement.upper().startswith('INSERT'):
                        try:
                            self.cursor.execute(statement)
                            affected = self.cursor.rowcount
                            success_count += 1
                            
                            if i % 10 == 0:  # Progress update every 10 statements
                                logger.info(f"✅ Executed {i}/{len(statements)} statements, {affected} rows affected")
                                
                        except mysql.connector.Error as e:
                            logger.warning(f"⚠️  Statement {i} failed: {e}")
                            # Continue with next statement
                
                # Commit transaction
                self.connection.commit()
                logger.info(f"✅ Import completed successfully! {success_count}/{len(statements)} statements executed")
                
                return True
                
            except Exception as e:
                # Rollback on error
                self.connection.rollback()
                logger.error(f"❌ Import failed, rolled back: {e}")
                return False
            
        except Exception as e:
            logger.error(f"❌ Import execution failed: {e}")
            return False
    
    def validate_sql_file(self) -> bool:
        """Validate SQL file với AUTO_INCREMENT checks"""
        try:
            logger.info("🔍 Validating SQL file...")
            
            if not Path(self.sql_file).exists():
                logger.error(f"❌ SQL file not found: {self.sql_file}")
                return False
            
            file_size = Path(self.sql_file).stat().st_size / (1024*1024)  # MB
            logger.info(f"📄 SQL file size: {file_size:.2f} MB")
            
            with open(self.sql_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            insert_count = content.count('INSERT')
            logger.info(f"📋 Found {insert_count} INSERT statements")
            
            if insert_count == 0:
                logger.error("❌ No INSERT statements found")
                return False
            
            # Check for AUTO_INCREMENT conflicts
            logger.info("🔍 Checking for AUTO_INCREMENT conflicts...")
            auto_increment_tables = ['status', 'roles', 'warehouses', 'vehicles', 
                                   'users', 'orders', 'order_items', 'addresses', 
                                   'payments', 'deliveries']
            
            conflicts = []
            for table in auto_increment_tables:
                # Check if inserting into id column directly (trừ external_id)
                pattern = f"INSERT.*INTO {table}.*\\(.*\\bid\\b.*,"
                if re.search(pattern, content, re.IGNORECASE):
                    # Double check it's not external_id
                    pattern_external = f"INSERT.*INTO {table}.*\\(.*external_id.*,"
                    if not re.search(pattern_external, content, re.IGNORECASE):
                        conflicts.append(f"Table '{table}' has AUTO_INCREMENT, should not insert into 'id' column")
            
            if conflicts:
                logger.error("❌ AUTO_INCREMENT conflicts found:")
                for conflict in conflicts:
                    logger.error(f"   - {conflict}")
                return False
            
            logger.info("✅ SQL file validation passed (including AUTO_INCREMENT checks)")
            return True
            
        except Exception as e:
            logger.error(f"❌ SQL validation failed: {e}")
            return False
    
    def post_import_verification(self) -> bool:
        """Verify import results"""
        try:
            logger.info("🔍 Post-import verification...")
            
            verification_queries = [
                ("Total Categories", "SELECT COUNT(*) FROM categories"),
                ("Total Products", "SELECT COUNT(*) FROM products"),
                ("Total Orders", "SELECT COUNT(*) FROM orders"),
                ("Total Order Items", "SELECT COUNT(*) FROM order_items"),
                ("Total Users", "SELECT COUNT(*) FROM users"),
                ("Total Addresses", "SELECT COUNT(*) FROM addresses"),
                ("Total Payments", "SELECT COUNT(*) FROM payments"),
                ("Total Deliveries", "SELECT COUNT(*) FROM deliveries")
            ]
            
            logger.info("📊 Final record counts:")
            for description, query in verification_queries:
                self.cursor.execute(query)
                count = self.cursor.fetchone()[0]
                logger.info(f"   {description}: {count:,}")
            
            # Sample data check
            self.cursor.execute("SELECT name FROM products LIMIT 5")
            products = [row[0] for row in self.cursor.fetchall()]
            logger.info(f"📦 Sample products: {products}")
            
            logger.info("✅ Post-import verification completed")
            return True
            
        except Exception as e:
            logger.error(f"❌ Verification failed: {e}")
            return False
    
    def cleanup_session(self):
        """Reset session variables"""
        try:
            if self.cursor:
                cleanup_vars = [
                    "SET SESSION foreign_key_checks = 1",
                    "SET SESSION unique_checks = 1",
                    "SET SESSION autocommit = 1"
                ]
                
                for var in cleanup_vars:
                    self.cursor.execute(var)
                
                self.cursor.close()
            
            if self.connection:
                self.connection.close()
                
            logger.info("🧹 Session cleaned up")
            
        except Exception as e:
            logger.warning(f"⚠️  Cleanup warning: {e}")
    
    def deploy(self) -> bool:
        """Main deployment process"""
        logger.info("🚀 DataCo Deployment Starting...")
        
        try:
            # Step 1: Connect
            if not self.connect_database():
                return False
            
            # Step 2: Verify schema
            if not self.verify_schema():
                return False
            
            # Step 3: Check existing data
            existing_data = self.check_existing_data()
            if any(count > 0 for count in existing_data.values()):
                response = input("⚠️  Database has existing data. Continue? (y/N): ")
                if response.lower() != 'y':
                    logger.info("🛑 Deployment cancelled by user")
                    return False
            
            # Step 4: Execute import
            if not self.execute_import():
                return False
            
            # Step 5: Verify results (if not dry run)
            if not self.dry_run:
                if not self.post_import_verification():
                    return False
            
            logger.info("🎉 Deployment completed successfully!")
            return True
            
        except KeyboardInterrupt:
            logger.warning("⚠️  Deployment interrupted by user")
            return False
        except Exception as e:
            logger.error(f"💥 Deployment failed: {e}")
            return False
        finally:
            self.cleanup_session()

def main():
    parser = argparse.ArgumentParser(description='DataCo Database Deployment')
    parser.add_argument('--dry-run', action='store_true', help='Validate only, no actual import')
    parser.add_argument('--host', default='localhost', help='Database host')
    parser.add_argument('--user', default='fastroute_user', help='Database user') 
    parser.add_argument('--password', default='fastroute_password', help='Database password')
    parser.add_argument('--database', default='fasteroute', help='Database name')
    parser.add_argument('--sql-file', default='dataco_complete_import.sql', help='SQL file to import')
    
    args = parser.parse_args()
    
    # Get password if not provided as argument
    if not args.password or args.password == 'fastroute_password':
        import getpass
        args.password = getpass.getpass("Database password (default: fastroute_password): ") or 'fastroute_password'
    
    # Database configuration
    db_config = {
        'host': args.host,
        'user': args.user,
        'password': args.password,
        'database': args.database,
        'charset': 'utf8mb4'
    }
    
    # Create deployer
    deployer = DataCoDeployer(
        db_config=db_config,
        sql_file=args.sql_file,
        dry_run=args.dry_run
    )
    
    # Run deployment
    success = deployer.deploy()
    
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()