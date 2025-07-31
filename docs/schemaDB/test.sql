-- ===================================================
-- KTC LOGISTICS SYSTEM - SQL TABLES
-- Generated for KTC Logistics 2025 Project (3PL/4PL)
-- Date: August 01, 2025
-- Database: MySQL
-- Ordered by workflow: from order placement to delivery
-- Updated:
--   - Renamed LOGISTICS_MANAGER to MANAGER in users.role
--   - Added capacity and max_weight to warehouses
--   - Removed is_active and hazardous from products
--   - Removed pickup_method from orders
--   - Removed PREPARING from orders.status
--   - Merged failure_reason and failure_details into failure_notes in orders
--   - Merged is_optimized and ai_suggested into ai_optimized in routes
--   - Merged READY and ASSIGNED into READY in orders.status
--   - Replaced IN_PROGRESS with ON_DELIVERY in orders.status
--   - Merged delivery_trips and delivery_tracking into single delivery_tracking table
--   - Updated all status fields to use status table
--   - Replaced milestone and location with stop_point (now location)
--   - Removed driver_id from delivery_tracking, kept only vehicle_id
--   - Merged driver_note and notes into notes in delivery_tracking
--   - Added recipient_name and recipient_phone to orders
--   - Added driver_id to orders
--   - Restored route_id to link with routes
--   - Updated order_items with actual_distance_km for real route distance
--   - Renamed trip_id to tracking_id
--   - Updated activity_logs to use status table for user actions only
--   - Renamed name to type in status table
--   - Removed target_type and target_id from activity_logs
--   - Added role_id to activity_logs
--   - Merged details and notes into metadata in activity_logs
--   - Replaced ip_address with device_info in activity_logs
--   - Added session_id and action_timestamp to activity_logs
--   - Renamed entity_type to type in status table
--   - Renamed status_types to status
--   - Renamed status_code to code
--   - Merged password_reset_tokens and refresh_tokens into tokens
--   - Added roles table with permissions
--   - Removed terrain_type from routes, added to waypoints JSON with completed_at
--   - Added delivery_latitude, delivery_longitude to orders
-- Supports payment, customer role, products, order value, customer order creation, AI, and 3D Map
-- ===================================================

-- 1. STATUS TABLE
-- Centralized management of status values
-- ===================================================
CREATE TABLE status (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    type VARCHAR(50) NOT NULL,
    code VARCHAR(50) NOT NULL,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

    UNIQUE INDEX idx_status_code (type, code)
);

-- 2. ROLES TABLE
-- Manages reusable roles with permissions for users and other entities
-- ===================================================
CREATE TABLE roles (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    role_name VARCHAR(50) NOT NULL,
    permission JSON COMMENT 'Array of permissions, e.g. ["create_order", "view_order", "manage_vehicle"]',
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

    UNIQUE INDEX idx_roles_name (role_name)
);

-- 3. USERS TABLE
-- Manages users (customers, drivers, managers) for authentication and roles
-- ===================================================
CREATE TABLE users (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    password VARCHAR(255) NOT NULL,
    role_id BIGINT NOT NULL,
    phone VARCHAR(50),
    address TEXT,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    notes TEXT COMMENT 'Additional notes about the user',

    FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE RESTRICT,

    INDEX idx_users_email (email),
    INDEX idx_users_role (role_id),
    INDEX idx_users_active (is_active)
);

-- 4. TOKENS TABLE
-- Manages both password reset and refresh tokens
-- ===================================================
CREATE TABLE tokens (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    token VARCHAR(255) UNIQUE NOT NULL,
    expiry TIMESTAMP NOT NULL,
    status_id BIGINT NOT NULL,
    token_type ENUM('PASSWORD_RESET', 'REFRESH') NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    user_id BIGINT,
    notes TEXT COMMENT 'Notes about the token',

    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (status_id) REFERENCES status(id) ON DELETE RESTRICT,

    INDEX idx_tokens_token (token),
    INDEX idx_tokens_user (user_id),
    INDEX idx_tokens_expiry (expiry)
);

-- 5. WAREHOUSES TABLE
-- Stores warehouse information for inventory management
-- ===================================================
CREATE TABLE warehouses (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    address TEXT NOT NULL,
    capacity DECIMAL(10, 2) COMMENT 'Dung tích chứa (m³)',
    max_weight DECIMAL(10, 2) COMMENT 'Tải trọng tối đa (tấn)',
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by BIGINT,
    notes TEXT COMMENT 'Additional notes about the warehouse',

    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,
    INDEX idx_warehouses_name (name),
    INDEX idx_warehouses_active (is_active)
);

-- 6. PRODUCTS TABLE
-- Stores product information with inventory support
-- ===================================================
CREATE TABLE products (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    product_code VARCHAR(50) UNIQUE NOT NULL,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    unit_price DECIMAL(10, 2) NOT NULL COMMENT 'Price per unit in VND',
    weight DECIMAL(10, 2) COMMENT 'Weight in kg',
    fragile BOOLEAN DEFAULT FALSE COMMENT 'Is the product fragile?',
    stock_quantity INT NOT NULL DEFAULT 0 COMMENT 'Current stock in inventory',
    warehouse_id BIGINT COMMENT 'Default warehouse for the product',
    temporary BOOLEAN DEFAULT FALSE COMMENT 'True if created by customer',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by BIGINT,
    notes TEXT COMMENT 'Additional notes about the product',

    FOREIGN KEY (warehouse_id) REFERENCES warehouses(id) ON DELETE SET NULL,
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,
    INDEX idx_products_code (product_code),
    INDEX idx_products_temporary (temporary),
    INDEX idx_products_warehouse (warehouse_id)
);

-- 7. INVENTORY TRANSACTIONS TABLE
-- Tracks inventory movements (in/out) across warehouses
-- ===================================================
CREATE TABLE inventory_transactions (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    product_id BIGINT NOT NULL,
    warehouse_id BIGINT NOT NULL,
    status_id BIGINT NOT NULL,
    quantity INT NOT NULL,
    transaction_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    order_id BIGINT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by BIGINT,
    notes TEXT COMMENT 'Notes about the transaction',

    FOREIGN KEY (product_id) REFERENCES products(id) ON DELETE CASCADE,
    FOREIGN KEY (warehouse_id) REFERENCES warehouses(id) ON DELETE CASCADE,
    FOREIGN KEY (order_id) REFERENCES orders(id) ON DELETE SET NULL,
    FOREIGN KEY (status_id) REFERENCES status(id) ON DELETE RESTRICT,
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,

    INDEX idx_inventory_transactions_product (product_id),
    INDEX idx_inventory_transactions_warehouse (warehouse_id),
    INDEX idx_inventory_transactions_status (status_id),
    INDEX idx_inventory_transactions_order (order_id)
);

-- 8. ORDERS TABLE
-- Stores static order information (customer, addresses, total amount)
-- ===================================================
CREATE TABLE orders (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    order_code VARCHAR(100) UNIQUE NOT NULL,
    status_id BIGINT NOT NULL,
    pickup_address VARCHAR(500) COMMENT 'Text address for pickup',
    pickup_latitude DOUBLE COMMENT 'Latitude for pickup (AI Route Optimization)',
    pickup_longitude DOUBLE COMMENT 'Longitude for pickup (AI Route Optimization)',
    delivery_address VARCHAR(500) NOT NULL COMMENT 'Text address for delivery',
    delivery_latitude DOUBLE COMMENT 'Latitude for delivery (AI Route Optimization)',
    delivery_longitude DOUBLE COMMENT 'Longitude for delivery (AI Route Optimization)',
    recipient_name VARCHAR(255) NOT NULL COMMENT 'Recipient''s name',
    recipient_phone VARCHAR(50) NOT NULL COMMENT 'Recipient''s phone number',
    description TEXT,
    total_amount DECIMAL(10, 2) COMMENT 'Total order value in VND (sum of order_items.subtotal + shipping_fee)',
    shipping_fee DECIMAL(10, 2) DEFAULT 0.00 COMMENT 'Shipping fee in VND (calculated from order_items)',
    transport_mode ENUM('STANDARD', 'EXPRESS', 'PRIORITY') DEFAULT 'STANDARD' COMMENT 'Affects shipping fee',
    scheduled_time TIMESTAMP,
    actual_delivery_time TIMESTAMP,
    notes TEXT COMMENT 'Additional notes about the order',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by BIGINT,
    customer_id BIGINT,
    vehicle_id BIGINT,
    driver_id BIGINT,
    tracking_id BIGINT,
    route_id BIGINT,

    FOREIGN KEY (status_id) REFERENCES status(id) ON DELETE RESTRICT,
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,
    FOREIGN KEY (customer_id) REFERENCES users(id) ON DELETE SET NULL,
    FOREIGN KEY (vehicle_id) REFERENCES vehicles(id) ON DELETE SET NULL,
    FOREIGN KEY (driver_id) REFERENCES users(id) ON DELETE SET NULL,
    FOREIGN KEY (tracking_id) REFERENCES delivery_tracking(id) ON DELETE SET NULL,
    FOREIGN KEY (route_id) REFERENCES routes(id) ON DELETE SET NULL,

    INDEX idx_orders_code (order_code),
    INDEX idx_orders_status (status_id),
    INDEX idx_orders_customer (customer_id),
    INDEX idx_orders_vehicle (vehicle_id),
    INDEX idx_orders_driver (driver_id),
    INDEX idx_orders_created_by (created_by),
    INDEX idx_orders_scheduled_time (scheduled_time),
    INDEX idx_orders_tracking (tracking_id),
    INDEX idx_orders_route (route_id)
);

-- 9. ORDER ITEMS TABLE
-- Links products to orders with shipping fee calculation
-- ===================================================
CREATE TABLE order_items (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    order_id BIGINT NOT NULL,
    product_id BIGINT NOT NULL,
    quantity INT NOT NULL,
    unit_price DECIMAL(10, 2) NOT NULL COMMENT 'Price at time of order',
    subtotal DECIMAL(10, 2) NOT NULL COMMENT 'quantity * unit_price',
    weight_kg DECIMAL(10, 2) COMMENT 'Total weight of this item (quantity * product.weight)',
    actual_distance_km DECIMAL(10, 2) COMMENT 'Actual route distance in km from routes.waypoints',
    shipping_fee DECIMAL(10, 2) COMMENT 'Calculated as: 50000 + (1000 * actual_distance_km) + (500 * weight_kg) * transport_mode_multiplier',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    notes TEXT COMMENT 'Additional notes about the item',

    FOREIGN KEY (order_id) REFERENCES orders(id) ON DELETE CASCADE,
    FOREIGN KEY (product_id) REFERENCES products(id) ON DELETE RESTRICT,

    INDEX idx_order_items_order (order_id),
    INDEX idx_order_items_product (product_id)
);

-- 10. ROUTES TABLE
-- Stores optimized routes with waypoints JSON for AI Route Optimization
-- ===================================================
CREATE TABLE routes (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    waypoints JSON NOT NULL COMMENT 'JSON array of waypoints, e.g. [{"address": "123 Hanoi", "latitude": 21.0, "longitude": 105.8, "terrain_type": "URBAN", "distance_to_next": 2.5, "completed_at": "2025-08-01 00:11:00"}, ...]',
    estimated_distance DECIMAL(10, 2) COMMENT 'Total distance in km',
    estimated_duration INT COMMENT 'Duration in minutes',
    estimated_cost DECIMAL(10, 2) COMMENT 'Estimated shipping cost in VND',
    ai_optimized BOOLEAN DEFAULT FALSE COMMENT 'Được tối ưu bởi AI',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by BIGINT,
    notes TEXT COMMENT 'Additional notes about the route',

    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,
    INDEX idx_routes_name (name),
    INDEX idx_routes_created_by (created_by)
);

-- 11. VEHICLES TABLE
-- Stores vehicles for transportation
-- ===================================================
CREATE TABLE vehicles (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    license_plate VARCHAR(20) UNIQUE NOT NULL,
    vehicle_type VARCHAR(100),
    capacity DECIMAL(10, 2) COMMENT 'Tải trọng (tấn)',
    status_id BIGINT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    notes TEXT COMMENT 'Additional notes about the vehicle',

    FOREIGN KEY (status_id) REFERENCES status(id) ON DELETE RESTRICT,

    INDEX idx_vehicles_license_plate (license_plate),
    INDEX idx_vehicles_status (status_id),
    INDEX idx_vehicles_type (vehicle_type)
);

-- 12. DELIVERY TRACKING TABLE
-- Manages tracking per trip (vehicle) with locations and notes
-- ===================================================
CREATE TABLE delivery_tracking (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    vehicle_id BIGINT NOT NULL,
    status_id BIGINT NOT NULL,
    latitude DOUBLE COMMENT 'Current latitude of trip',
    longitude DOUBLE COMMENT 'Current longitude of trip',
    timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    location VARCHAR(255) COMMENT 'Location identifier and description (e.g., A: Warehouse A, B: Station 1)',
    arrived_at TIMESTAMP COMMENT 'Time of arrival at location',
    notes TEXT COMMENT 'Notes including driver issues and trip details',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

    FOREIGN KEY (vehicle_id) REFERENCES vehicles(id) ON DELETE CASCADE,
    FOREIGN KEY (status_id) REFERENCES status(id) ON DELETE RESTRICT,

    INDEX idx_delivery_tracking_vehicle (vehicle_id),
    INDEX idx_delivery_tracking_status (status_id),
    INDEX idx_delivery_tracking_location (location),
    INDEX idx_delivery_tracking_timestamp (timestamp)
);

-- 13. DELIVERY PROOFS TABLE
-- Stores proof of delivery (photos, signatures)
-- ===================================================
CREATE TABLE delivery_proofs (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    proof_type ENUM('PHOTO', 'SIGNATURE', 'RECEIPT') NOT NULL,
    file_path VARCHAR(500),
    file_name VARCHAR(255),
    recipient_name VARCHAR(255),
    recipient_signature TEXT COMMENT 'Base64 encoded signature',
    captured_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    order_id BIGINT NOT NULL,
    uploaded_by BIGINT,
    notes TEXT COMMENT 'Additional notes about the proof',

    FOREIGN KEY (order_id) REFERENCES orders(id) ON DELETE CASCADE,
    FOREIGN KEY (uploaded_by) REFERENCES users(id) ON DELETE SET NULL,

    INDEX idx_delivery_proofs_order (order_id),
    INDEX idx_delivery_proofs_type (proof_type),
    INDEX idx_delivery_proofs_uploaded_by (uploaded_by)
);

-- 14. PAYMENTS TABLE
-- Manages order payments, no refunds
-- ===================================================
CREATE TABLE payments (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    order_id BIGINT NOT NULL,
    amount DECIMAL(10, 2) NOT NULL COMMENT 'Payment amount in VND',
    payment_method ENUM('CASH', 'CREDIT_CARD', 'BANK_TRANSFER', 'STRIPE') NOT NULL,
    status_id BIGINT NOT NULL,
    transaction_id VARCHAR(255) COMMENT 'Transaction ID from Stripe',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by BIGINT,
    notes TEXT COMMENT 'Additional notes about the payment',

    FOREIGN KEY (order_id) REFERENCES orders(id) ON DELETE CASCADE,
    FOREIGN KEY (status_id) REFERENCES status(id) ON DELETE RESTRICT,
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,

    INDEX idx_payments_order (order_id),
    INDEX idx_payments_status (status_id),
    INDEX idx_payments_transaction (transaction_id)
);

-- 15. ACTIVITY LOGS TABLE
-- Tracks user-initiated activities for analysis based on role
-- ===================================================
CREATE TABLE activity_logs (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    actor_id BIGINT COMMENT 'ID of user or system actor',
    role_id BIGINT NOT NULL,
    status_id BIGINT NOT NULL,
    action_timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    device_info JSON COMMENT 'Device details, e.g. {"ip": "192.168.1.1", "browser": "Chrome", "os": "Windows"}',
    metadata JSON COMMENT 'Combined details and notes, e.g. {"details": {"failure_notes": "Broken package"}, "notes": "Manual entry"}',

    FOREIGN KEY (status_id) REFERENCES status(id) ON DELETE RESTRICT,
    FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE RESTRICT,

    INDEX idx_activity_logs_status (status_id),
    INDEX idx_activity_logs_role (role_id),
    INDEX idx_activity_logs_session (session_id),
    INDEX idx_activity_logs_timestamp (action_timestamp)
);