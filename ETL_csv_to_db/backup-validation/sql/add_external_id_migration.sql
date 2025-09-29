-- Add external_id columns to existing tables
-- This script modifies the FastRoute database to support external ID mapping
-- from DataCo CSV data while keeping auto-increment primary keys

USE fastroute;

-- Add external_id column to categories table
ALTER TABLE categories 
ADD COLUMN external_id BIGINT UNIQUE AFTER id;

-- Add external_id column to stores table  
ALTER TABLE stores
ADD COLUMN external_id BIGINT UNIQUE AFTER id;

-- Add external_id column to products table
ALTER TABLE products
ADD COLUMN external_id BIGINT UNIQUE AFTER id;

-- Add external_id column to users table
ALTER TABLE users
ADD COLUMN external_id BIGINT UNIQUE AFTER id;

-- Add external_id column to orders table
ALTER TABLE orders
ADD COLUMN external_id BIGINT UNIQUE AFTER id;

-- Add external_id column to order_items table
ALTER TABLE order_items
ADD COLUMN external_id BIGINT UNIQUE AFTER id;

-- Add indexes for performance
CREATE INDEX idx_categories_external_id ON categories(external_id);
CREATE INDEX idx_stores_external_id ON stores(external_id);
CREATE INDEX idx_products_external_id ON products(external_id);
CREATE INDEX idx_users_external_id ON users(external_id);
CREATE INDEX idx_orders_external_id ON orders(external_id);
CREATE INDEX idx_order_items_external_id ON order_items(external_id);

-- Show completion message
SELECT 'External ID columns added successfully!' as status;
