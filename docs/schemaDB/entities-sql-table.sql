-- ===================================================
-- KTC LOGISTICS SYSTEM - SQL TABLES FROM ENTITIES
-- Generated from Spring Boot JPA Entities
-- Date: July 29, 2025
-- ===================================================

-- 1. USERS TABLE
-- ===================================================
CREATE TABLE users (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    password VARCHAR(255) NOT NULL,
    role ENUM('ADMIN', 'DISPATCHER', 'DRIVER', 'FLEET_MANAGER', 'OPERATIONS_MANAGER') NOT NULL,
    phone VARCHAR(50),
    address TEXT,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    INDEX idx_users_email (email),
    INDEX idx_users_role (role),
    INDEX idx_users_active (is_active)
);

-- ===================================================
-- 2. CUSTOMERS TABLE
-- ===================================================
CREATE TABLE customers (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE,
    phone VARCHAR(50) NOT NULL,
    address TEXT,
    company VARCHAR(255),
    contact_person VARCHAR(255),
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    INDEX idx_customers_email (email),
    INDEX idx_customers_phone (phone),
    INDEX idx_customers_active (is_active)
);

-- ===================================================
-- 3. VEHICLES TABLE
-- ===================================================
CREATE TABLE vehicles (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    license_plate VARCHAR(20) UNIQUE NOT NULL,
    vehicle_type VARCHAR(100),
    capacity DOUBLE COMMENT 'Tải trọng (tấn)',
    fuel_type VARCHAR(50),
    status ENUM('AVAILABLE', 'IN_USE', 'MAINTENANCE', 'INACTIVE', 'OUT_OF_SERVICE') NOT NULL DEFAULT 'AVAILABLE',
    registration_date TIMESTAMP,
    last_maintenance_date TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    INDEX idx_vehicles_license_plate (license_plate),
    INDEX idx_vehicles_status (status),
    INDEX idx_vehicles_type (vehicle_type)
);

-- ===================================================
-- 4. ROUTES TABLE
-- ===================================================
CREATE TABLE routes (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    start_address VARCHAR(500) NOT NULL,
    end_address VARCHAR(500) NOT NULL,
    start_latitude DOUBLE,
    start_longitude DOUBLE,
    end_latitude DOUBLE,
    end_longitude DOUBLE,
    estimated_distance DOUBLE COMMENT 'Distance in km',
    estimated_duration INT COMMENT 'Duration in minutes',
    waypoints TEXT COMMENT 'JSON string of waypoints',
    is_optimized BOOLEAN DEFAULT FALSE,
    optimization_score DOUBLE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by BIGINT,
    
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,
    INDEX idx_routes_name (name),
    INDEX idx_routes_optimized (is_optimized),
    INDEX idx_routes_created_by (created_by)
);

-- ===================================================
-- 5. DELIVERY ORDERS TABLE
-- ===================================================
CREATE TABLE delivery_orders (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    order_code VARCHAR(100) UNIQUE NOT NULL,
    status ENUM('PENDING', 'ASSIGNED', 'IN_PROGRESS', 'PICKED_UP', 'ON_DELIVERY', 'DELIVERED', 'FAILED', 'CANCELLED') NOT NULL DEFAULT 'PENDING',
    pickup_address VARCHAR(500),
    delivery_address VARCHAR(500) NOT NULL,
    customer_name VARCHAR(255),
    customer_phone VARCHAR(50),
    description TEXT,
    scheduled_time TIMESTAMP,
    actual_delivery_time TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by BIGINT,
    vehicle_id BIGINT,
    customer_id BIGINT,
    route_id BIGINT,
    
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,
    FOREIGN KEY (vehicle_id) REFERENCES vehicles(id) ON DELETE SET NULL,
    FOREIGN KEY (customer_id) REFERENCES customers(id) ON DELETE SET NULL,
    FOREIGN KEY (route_id) REFERENCES routes(id) ON DELETE SET NULL,
    
    INDEX idx_delivery_orders_code (order_code),
    INDEX idx_delivery_orders_status (status),
    INDEX idx_delivery_orders_customer (customer_id),
    INDEX idx_delivery_orders_vehicle (vehicle_id),
    INDEX idx_delivery_orders_created_by (created_by),
    INDEX idx_delivery_orders_scheduled_time (scheduled_time)
);

-- ===================================================
-- 6. DISPATCH ASSIGNMENTS TABLE
-- ===================================================
CREATE TABLE dispatch_assignments (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    assigned_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    status ENUM('ASSIGNED', 'ACCEPTED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED', 'REJECTED') NOT NULL DEFAULT 'ASSIGNED',
    expected_completion_time TIMESTAMP,
    actual_completion_time TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    delivery_order_id BIGINT,
    vehicle_id BIGINT,
    driver_id BIGINT,
    
    FOREIGN KEY (delivery_order_id) REFERENCES delivery_orders(id) ON DELETE CASCADE,
    FOREIGN KEY (vehicle_id) REFERENCES vehicles(id) ON DELETE SET NULL,
    FOREIGN KEY (driver_id) REFERENCES users(id) ON DELETE SET NULL,
    
    INDEX idx_dispatch_assignments_status (status),
    INDEX idx_dispatch_assignments_order (delivery_order_id),
    INDEX idx_dispatch_assignments_driver (driver_id),
    INDEX idx_dispatch_assignments_vehicle (vehicle_id),
    INDEX idx_dispatch_assignments_assigned_at (assigned_at)
);

-- ===================================================
-- 7. ORDER TRACKINGS TABLE
-- ===================================================
CREATE TABLE order_trackings (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    status ENUM('ORDER_CREATED', 'ASSIGNED_TO_DRIVER', 'DRIVER_ACCEPTED', 'PICKUP_STARTED', 'PICKED_UP', 'ON_THE_WAY', 'ARRIVED_AT_DESTINATION', 'DELIVERY_ATTEMPTED', 'DELIVERED', 'FAILED_DELIVERY', 'RETURNED') NOT NULL,
    latitude DOUBLE,
    longitude DOUBLE,
    location VARCHAR(500),
    notes TEXT,
    timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    delivery_order_id BIGINT,
    updated_by BIGINT,
    
    FOREIGN KEY (delivery_order_id) REFERENCES delivery_orders(id) ON DELETE CASCADE,
    FOREIGN KEY (updated_by) REFERENCES users(id) ON DELETE SET NULL,
    
    INDEX idx_order_trackings_order (delivery_order_id),
    INDEX idx_order_trackings_status (status),
    INDEX idx_order_trackings_timestamp (timestamp),
    INDEX idx_order_trackings_updated_by (updated_by)
);

-- ===================================================
-- 8. DELIVERY PROOFS TABLE
-- ===================================================
CREATE TABLE delivery_proofs (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    proof_type VARCHAR(50) NOT NULL COMMENT 'PHOTO, SIGNATURE, RECEIPT',
    file_path VARCHAR(500),
    file_name VARCHAR(255),
    file_size BIGINT,
    mime_type VARCHAR(100),
    description TEXT,
    recipient_name VARCHAR(255),
    recipient_signature TEXT COMMENT 'Base64 encoded signature',
    captured_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    delivery_order_id BIGINT NOT NULL,
    uploaded_by BIGINT,
    
    FOREIGN KEY (delivery_order_id) REFERENCES delivery_orders(id) ON DELETE CASCADE,
    FOREIGN KEY (uploaded_by) REFERENCES users(id) ON DELETE SET NULL,
    
    INDEX idx_delivery_proofs_order (delivery_order_id),
    INDEX idx_delivery_proofs_type (proof_type),
    INDEX idx_delivery_proofs_uploaded_by (uploaded_by)
);

-- ===================================================
-- 9. VEHICLE MAINTENANCES TABLE
-- ===================================================
CREATE TABLE vehicle_maintenances (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    description TEXT,
    maintenance_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    maintenance_type VARCHAR(50) COMMENT 'SCHEDULED, EMERGENCY, REPAIR',
    cost DOUBLE,
    next_maintenance_date TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    vehicle_id BIGINT,
    creator_id BIGINT,
    
    FOREIGN KEY (vehicle_id) REFERENCES vehicles(id) ON DELETE CASCADE,
    FOREIGN KEY (creator_id) REFERENCES users(id) ON DELETE SET NULL,
    
    INDEX idx_vehicle_maintenances_vehicle (vehicle_id),
    INDEX idx_vehicle_maintenances_type (maintenance_type),
    INDEX idx_vehicle_maintenances_date (maintenance_date),
    INDEX idx_vehicle_maintenances_creator (creator_id)
);

-- ===================================================
-- 10. NOTIFICATIONS TABLE
-- ===================================================
CREATE TABLE notifications (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    content TEXT,
    notification_type VARCHAR(100) COMMENT 'ORDER_ASSIGNED, DELIVERY_COMPLETED, MAINTENANCE_DUE, etc.',
    priority_level VARCHAR(20) COMMENT 'LOW, MEDIUM, HIGH, URGENT',
    is_read BOOLEAN DEFAULT FALSE,
    read_at TIMESTAMP NULL,
    scheduled_at TIMESTAMP,
    sent_at TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    entity_type VARCHAR(100) COMMENT 'DeliveryOrder, Vehicle, User, etc.',
    entity_id BIGINT,
    action_url TEXT COMMENT 'URL to navigate when clicked',
    recipient_id BIGINT NOT NULL,
    sender_id BIGINT,
    
    FOREIGN KEY (recipient_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (sender_id) REFERENCES users(id) ON DELETE SET NULL,
    
    INDEX idx_notifications_recipient (recipient_id),
    INDEX idx_notifications_sender (sender_id),
    INDEX idx_notifications_type (notification_type),
    INDEX idx_notifications_read (is_read),
    INDEX idx_notifications_priority (priority_level),
    INDEX idx_notifications_created_at (created_at)
);

-- ===================================================
-- 11. KPI METRICS TABLE
-- ===================================================
CREATE TABLE kpi_metrics (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    metric_name VARCHAR(255) NOT NULL COMMENT 'DELIVERY_SUCCESS_RATE, AVG_DELIVERY_TIME, etc.',
    metric_value DOUBLE NOT NULL,
    metric_unit VARCHAR(50) COMMENT 'PERCENTAGE, MINUTES, COUNT, etc.',
    calculation_date DATE NOT NULL,
    period_type VARCHAR(20) COMMENT 'DAILY, WEEKLY, MONTHLY, YEARLY',
    period_start DATE,
    period_end DATE,
    details TEXT COMMENT 'JSON details of calculation',
    calculated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    user_id BIGINT COMMENT 'For user-specific metrics (driver performance)',
    vehicle_id BIGINT COMMENT 'For vehicle-specific metrics',
    
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (vehicle_id) REFERENCES vehicles(id) ON DELETE CASCADE,
    
    INDEX idx_kpi_metrics_name (metric_name),
    INDEX idx_kpi_metrics_date (calculation_date),
    INDEX idx_kpi_metrics_period (period_type),
    INDEX idx_kpi_metrics_user (user_id),
    INDEX idx_kpi_metrics_vehicle (vehicle_id)
);

-- ===================================================
-- 12. AUDIT LOGS TABLE
-- ===================================================
CREATE TABLE audit_logs (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    action VARCHAR(100) NOT NULL COMMENT 'CREATE, UPDATE, DELETE, LOGIN, LOGOUT',
    entity_type VARCHAR(100) COMMENT 'User, DeliveryOrder, Vehicle, etc.',
    entity_id BIGINT,
    details TEXT COMMENT 'JSON details of changes',
    ip_address VARCHAR(45),
    user_agent TEXT,
    timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    user_id BIGINT,
    
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL,
    
    INDEX idx_audit_logs_action (action),
    INDEX idx_audit_logs_entity (entity_type, entity_id),
    INDEX idx_audit_logs_user (user_id),
    INDEX idx_audit_logs_timestamp (timestamp)
);

-- ===================================================
-- 13. PASSWORD RESET TOKENS TABLE
-- ===================================================
CREATE TABLE password_reset_tokens (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    token VARCHAR(255) UNIQUE NOT NULL,
    expiry TIMESTAMP NOT NULL,
    used BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    user_id BIGINT,
    
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    
    INDEX idx_password_reset_tokens_token (token),
    INDEX idx_password_reset_tokens_user (user_id),
    INDEX idx_password_reset_tokens_expiry (expiry)
);

-- ===================================================
-- 14. SYSTEM CONFIGS TABLE
-- ===================================================
CREATE TABLE system_configs (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    config_key VARCHAR(255) UNIQUE NOT NULL,
    config_value TEXT,
    config_type VARCHAR(50) COMMENT 'STRING, INTEGER, BOOLEAN, JSON',
    description TEXT,
    is_active BOOLEAN DEFAULT TRUE,
    is_system BOOLEAN DEFAULT FALSE COMMENT 'Cannot be deleted if true',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by BIGINT,
    updated_by BIGINT,
    
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE SET NULL,
    FOREIGN KEY (updated_by) REFERENCES users(id) ON DELETE SET NULL,
    
    INDEX idx_system_configs_key (config_key),
    INDEX idx_system_configs_active (is_active),
    INDEX idx_system_configs_system (is_system)
);
