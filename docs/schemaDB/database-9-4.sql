-- MySQL database export
START TRANSACTION;

CREATE TABLE IF NOT EXISTS `vehicles` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của phương tiện',
    `license_plate` VARCHAR(20) NOT NULL UNIQUE COMMENT 'Biển số xe',
    `vehicle_type` VARCHAR(50) NOT NULL DEFAULT '[object Object]' COMMENT 'Loại phương tiện (xe tải, xe van, xe máy, ô tô)',
    `capacity_weight_kg` DECIMAL DEFAULT '[object Object]' COMMENT 'Trọng tải tối đa (kg)',
    `capacity_volume_m3` DECIMAL DEFAULT '[object Object]' COMMENT 'Thể tích chở hàng tối đa (m3)',
    `status_id` TINYINT UNSIGNED NOT NULL COMMENT 'Trạng thái xe (hoạt động, bảo trì, ngừng hoạt động)',
    `current_driver_id` BIGINT COMMENT 'ID tài xế hiện tại, NULL nếu chưa phân công',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo bản ghi',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật cuối cùng',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về phương tiện',
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `order_items` (
    `id` BIGINT AUTO_INCREMENT,
    `order_id` BIGINT NOT NULL,
    `product_id` BIGINT NOT NULL,
    `quantity` INT NOT NULL,
    `unit_price` DECIMAL NOT NULL,
    `shipping_fee` DECIMAL COMMENT 'Phí vận chuyển mà người dùng phải trả',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `notes` TEXT,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `delivery_proofs` (
    `id` BIGINT AUTO_INCREMENT,
    `proof_type` VARCHAR(50) NOT NULL DEFAULT '[object Object]',
    `file_path` VARCHAR(500),
    `file_name` VARCHAR(255),
    `recipient_name` VARCHAR(255),
    `recipient_signature` TEXT,
    `captured_at` DATETIME,
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `order_id` BIGINT NOT NULL,
    `uploaded_by` BIGINT,
    `notes` TEXT,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `addresses` (
    `id` BIGINT AUTO_INCREMENT,
    `order_id` BIGINT NOT NULL,
    `address_type` VARCHAR(50) NOT NULL,
    `address` VARCHAR(500) NOT NULL,
    `latitude` DECIMAL,
    `longitude` DECIMAL,
    `city` VARCHAR(100),
    `state` VARCHAR(100),
    `country` VARCHAR(100) DEFAULT '[object Object]',
    `region` VARCHAR(100),
    `postal_code` VARCHAR(20),
    `contact_name` VARCHAR(255),
    `contact_phone` VARCHAR(50),
    `contact_email` VARCHAR(255),
    `floor_number` VARCHAR(10),
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `products` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của sản phẩm',
    `product_card_id` VARCHAR(50) NOT NULL UNIQUE COMMENT 'Mã thẻ sản phẩm cho catalog',
    `product_code` VARCHAR(50) NOT NULL UNIQUE COMMENT 'Mã SKU/mã nội bộ sản phẩm',
    `name` VARCHAR(255) NOT NULL COMMENT 'Tên hiển thị sản phẩm',
    `description` TEXT COMMENT 'Mô tả chi tiết sản phẩm',
    `category_id` BIGINT NOT NULL COMMENT 'Phân loại danh mục sản phẩm',
    `unit_price` DECIMAL NOT NULL COMMENT 'Giá bán trên một đơn vị',
    `weight` DECIMAL COMMENT 'Trọng lượng sản phẩm (kg)',
    `volume` DECIMAL COMMENT 'Thể tích sản phẩm (m3)',
    `is_fragile` tinyint NOT NULL DEFAULT 0 COMMENT 'Cờ hàng dễ vỡ: 0=Không, 1=Có',
    `stock_quantity` INT NOT NULL DEFAULT '[object Object]' COMMENT 'Số lượng tồn kho hiện tại',
    `product_image` VARCHAR(500) COMMENT 'URL/đường dẫn ảnh sản phẩm',
    `product_status` tinyint NOT NULL DEFAULT 1 COMMENT 'Trạng thái sản phẩm: 0=Ngừng bán, 1=Đang bán',
    `warehouse_id` BIGINT COMMENT 'Kho chính chứa sản phẩm này',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo sản phẩm',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật sản phẩm cuối cùng',
    `created_by` BIGINT COMMENT 'ID người dùng tạo sản phẩm này',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về sản phẩm',
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `categories` (
    `id` BIGINT AUTO_INCREMENT,
    `category_id` VARCHAR(50) NOT NULL UNIQUE,
    `name` VARCHAR(255) NOT NULL,
    `description` TEXT,
    `parent_id` BIGINT,
    `is_active` tinyint NOT NULL DEFAULT 1,
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `notes` TEXT,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `stores` (
    `id` BIGINT AUTO_INCREMENT,
    `store_code` VARCHAR(50) NOT NULL UNIQUE,
    `store_name` VARCHAR(255) NOT NULL,
    `email` VARCHAR(255),
    `phone` VARCHAR(50) NOT NULL,
    `address` TEXT NOT NULL,
    `latitude` DECIMAL,
    `longitude` DECIMAL,
    `is_active` tinyint NOT NULL DEFAULT 1,
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `created_by` BIGINT,
    `notes` TEXT,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `routes` (
    `id` BIGINT AUTO_INCREMENT,
    `name` VARCHAR(255) NOT NULL,
    `waypoints` JSON NOT NULL,
    `estimated_distance_km` DECIMAL DEFAULT '[object Object]',
    `estimated_duration_minutes` INT DEFAULT '[object Object]',
    `estimated_cost` DECIMAL DEFAULT '[object Object]',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `completed_at` DATETIME,
    `created_by` BIGINT,
    `notes` TEXT,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `users` (
    -- Người dùng hệ thống bao gồm tài xế, điều phối, quản trị, v.v.
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của người dùng',
    `username` VARCHAR(100) NOT NULL UNIQUE COMMENT 'Tên đăng nhập duy nhất',
    `email` VARCHAR(255) NOT NULL UNIQUE COMMENT 'Địa chỉ email để đăng nhập và nhận thông báo',
    `password` VARCHAR(255) NOT NULL COMMENT 'Mật khẩu đã mã hóa để xác thực',
    `full_name` VARCHAR(255) COMMENT 'Họ tên đầy đủ để hiển thị',
    `phone` VARCHAR(50) COMMENT 'Số điện thoại liên hệ',
    `role_id` BIGINT NOT NULL COMMENT 'Vai trò người dùng (admin, điều phối, tài xế, xem)',
    `status_id` TINYINT UNSIGNED COMMENT 'Trạng thái tài khoản (hoạt động, ngừng hoạt động, tạm khóa)',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo tài khoản',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật tài khoản cuối cùng',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về người dùng',
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `status` (
    `id` TINYINT UNSIGNED AUTO_INCREMENT COMMENT 'Mã định danh trạng thái duy nhất (1-255)',
    `type` VARCHAR(50) NOT NULL COMMENT 'Phân loại trạng thái (phương tiện, đơn hàng, thanh toán, người dùng, v.v.)',
    `name` VARCHAR(100) NOT NULL COMMENT 'Tên trạng thái dễ đọc',
    `description` TEXT COMMENT 'Mô tả chi tiết ý nghĩa của trạng thái',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo trạng thái',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật trạng thái cuối cùng',
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `inventory_transactions` (
    `id` BIGINT AUTO_INCREMENT,
    `product_id` BIGINT NOT NULL,
    `warehouse_id` BIGINT NOT NULL,
    `status_id` TINYINT UNSIGNED NOT NULL,
    `transaction_type` VARCHAR(50) NOT NULL DEFAULT '[object Object]',
    `quantity` INT NOT NULL,
    `unit_cost` DECIMAL DEFAULT '[object Object]',
    `transaction_date` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `order_id` BIGINT,
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `created_by` BIGINT,
    `notes` TEXT,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `activity_logs` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của nhật ký hoạt động',
    `actor_id` BIGINT COMMENT 'ID người dùng thực hiện hành động',
    `role_id` BIGINT NOT NULL COMMENT 'Vai trò của người dùng tại thời điểm thực hiện',
    `status_id` TINYINT UNSIGNED NOT NULL COMMENT 'Trạng thái hoàn thành hành động',
    `action_type` VARCHAR(50) NOT NULL COMMENT 'Loại hành động (TẠO, CẬP NHẬT, XÓA, ĐĂNG NHẬP, v.v.)',
    `table_name` VARCHAR(64) COMMENT 'Bảng cơ sở dữ liệu bị ảnh hưởng bởi hành động',
    `record_id` BIGINT COMMENT 'ID của bản ghi bị ảnh hưởng',
    `action_timestamp` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời điểm xảy ra hành động',
    `metadata` JSON COMMENT 'Metadata bổ sung (giá trị cũ/mới, IP, v.v.)',
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `orders` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của đơn hàng',
    `order_id` VARCHAR(100) NOT NULL UNIQUE COMMENT 'Mã đơn hàng nghiệp vụ (dễ đọc)',
    `order_customer_id` VARCHAR(50) NOT NULL COMMENT 'Mã định danh khách hàng bên ngoài',
    `status_id` TINYINT UNSIGNED NOT NULL COMMENT 'Trạng thái đơn hàng (chờ xử lý, đang xử lý, hoàn thành, hủy)',
    `store_id` BIGINT COMMENT 'ID cửa hàng liên kết, NULL cho đơn hàng online',
    `description` TEXT COMMENT 'Mô tả và chi tiết đơn hàng',
    `total_amount` DECIMAL DEFAULT '[object Object]' COMMENT 'Tổng số tiền đơn hàng bao gồm phí',
    `benefit_per_order` DECIMAL DEFAULT '[object Object]' COMMENT 'Lợi nhuận/biên lãi dự kiến từ đơn hàng này',
    `order_profit_per_order` DECIMAL DEFAULT '[object Object]' COMMENT 'Lợi nhuận được tính toán cho đơn hàng này',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về đơn hàng',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo đơn hàng',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật đơn hàng cuối cùng',
    `created_by` BIGINT COMMENT 'ID người dùng tạo đơn hàng này',
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `payments` (
    `id` BIGINT AUTO_INCREMENT,
    `order_id` BIGINT NOT NULL,
    `amount` DECIMAL NOT NULL,
    `payment_method` VARCHAR(50) NOT NULL DEFAULT '[object Object]',
    `status_id` TINYINT UNSIGNED NOT NULL,
    `transaction_id` VARCHAR(255),
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `created_by` BIGINT,
    `notes` TEXT,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `deliveries` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của giao hàng',
    `order_id` BIGINT NOT NULL COMMENT 'Đơn hàng liên kết đang được giao',
    `delivery_fee` DECIMAL DEFAULT '[object Object]' COMMENT 'Chi phí vận chuyển doanh nghiệp logistic phải trả',
    `transport_mode` VARCHAR(50) DEFAULT '[object Object]' COMMENT 'Phương thức vận chuyển (đường bộ, hàng không, đường biển, đường sắt)',
    `service_type` VARCHAR(50) DEFAULT '[object Object]' COMMENT 'Mức dịch vụ (tiêu chuẩn, nhanh, ưu tiên)',
    `order_date` DATETIME NOT NULL COMMENT 'Thời điểm đặt hàng',
    `pickup_date` DATETIME COMMENT 'Thời điểm lấy hàng',
    `schedule_delivery_time` DATETIME COMMENT 'Thời gian giao hàng dự kiến',
    `actual_delivery_time` DATETIME COMMENT 'Thời gian hoàn thành giao hàng thực tế',
    `late_delivery_risk` tinyint NOT NULL DEFAULT 0 COMMENT 'Cờ rủi ro giao hàng trễ: 0=Không, 1=Có',
    `delivery_status` VARCHAR(50) DEFAULT '[object Object]' COMMENT 'Trạng thái giao hàng hiện tại',
    `vehicle_id` BIGINT NOT NULL COMMENT 'Phương tiện được phân công cho giao hàng này',
    `driver_id` BIGINT COMMENT 'Tài xế được phân công cho giao hàng này',
    `tracking_id` BIGINT COMMENT 'Bản ghi theo dõi GPS cho giao hàng này',
    `route_id` BIGINT COMMENT 'Tuyến đường tối ưu cho giao hàng này',
    `delivery_attempts` INT DEFAULT '[object Object]' COMMENT 'Số lần thử giao hàng đã thực hiện',
    `delivery_notes` TEXT COMMENT 'Hướng dẫn đặc biệt và ghi chú cho giao hàng',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo bản ghi giao hàng',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật bản ghi giao hàng cuối cùng',
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `roles` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của vai trò',
    `role_name` VARCHAR(50) NOT NULL UNIQUE COMMENT 'Tên vai trò (admin, điều phối, tài xế, xem)',
    `permission` JSON COMMENT 'Đối tượng JSON chứa quyền và quyền truy cập của vai trò',
    `description` TEXT COMMENT 'Mô tả chi tiết trách nhiệm của vai trò',
    `is_active` tinyint NOT NULL DEFAULT 1 COMMENT 'Trạng thái vai trò: 0=Ngừng hoạt động, 1=Hoạt động',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo vai trò',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật vai trò cuối cùng',
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `warehouses` (
    `id` BIGINT AUTO_INCREMENT,
    `warehouse_code` VARCHAR(50) NOT NULL UNIQUE,
    `name` VARCHAR(255) NOT NULL,
    `address` TEXT NOT NULL,
    `latitude` DECIMAL,
    `longitude` DECIMAL,
    `capacity_m3` DECIMAL,
    `is_active` tinyint NOT NULL DEFAULT 1,
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    `created_by` BIGINT,
    `notes` TEXT,
    PRIMARY KEY (`id`)
);



CREATE TABLE IF NOT EXISTS `delivery_tracking` (
    `id` BIGINT AUTO_INCREMENT,
    `vehicle_id` BIGINT NOT NULL,
    `status_id` TINYINT UNSIGNED NOT NULL,
    `latitude` DECIMAL,
    `longitude` DECIMAL,
    `timestamp` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `location` VARCHAR(255),
    `notes` TEXT,
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (`id`)
);


-- Foreign key constraints
ALTER TABLE `addresses` ADD CONSTRAINT `fk_addresses_order_id` FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`);
ALTER TABLE `categories` ADD CONSTRAINT `fk_categories_parent_id` FOREIGN KEY(`parent_id`) REFERENCES `categories`(`id`);
ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_order_id` FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`);
ALTER TABLE `activity_logs` ADD CONSTRAINT `fk_activity_logs_role_id` FOREIGN KEY(`role_id`) REFERENCES `roles`(`id`);
ALTER TABLE `activity_logs` ADD CONSTRAINT `fk_activity_logs_status_id` FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);
ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_route_id` FOREIGN KEY(`route_id`) REFERENCES `routes`(`id`);
ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_tracking_id` FOREIGN KEY(`tracking_id`) REFERENCES `delivery_tracking`(`id`);
ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_vehicle_id` FOREIGN KEY(`vehicle_id`) REFERENCES `vehicles`(`id`);
ALTER TABLE `delivery_proofs` ADD CONSTRAINT `fk_delivery_proofs_order_id` FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`);
ALTER TABLE `delivery_tracking` ADD CONSTRAINT `fk_delivery_tracking_status_id` FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);
ALTER TABLE `delivery_tracking` ADD CONSTRAINT `fk_delivery_tracking_vehicle_id` FOREIGN KEY(`vehicle_id`) REFERENCES `vehicles`(`id`);
ALTER TABLE `inventory_transactions` ADD CONSTRAINT `fk_inventory_transactions_order_id` FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`);
ALTER TABLE `inventory_transactions` ADD CONSTRAINT `fk_inventory_transactions_product_id` FOREIGN KEY(`product_id`) REFERENCES `products`(`id`);
ALTER TABLE `inventory_transactions` ADD CONSTRAINT `fk_inventory_transactions_status_id` FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);
ALTER TABLE `inventory_transactions` ADD CONSTRAINT `fk_inventory_transactions_warehouse_id` FOREIGN KEY(`warehouse_id`) REFERENCES `warehouses`(`id`);
ALTER TABLE `order_items` ADD CONSTRAINT `fk_order_items_order_id` FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`);
ALTER TABLE `order_items` ADD CONSTRAINT `fk_order_items_product_id` FOREIGN KEY(`product_id`) REFERENCES `products`(`id`);
ALTER TABLE `orders` ADD CONSTRAINT `fk_orders_status_id` FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);
ALTER TABLE `orders` ADD CONSTRAINT `fk_orders_store_id` FOREIGN KEY(`store_id`) REFERENCES `stores`(`id`);
ALTER TABLE `payments` ADD CONSTRAINT `fk_payments_order_id` FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`);
ALTER TABLE `payments` ADD CONSTRAINT `fk_payments_status_id` FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);
ALTER TABLE `products` ADD CONSTRAINT `fk_products_category_id` FOREIGN KEY(`category_id`) REFERENCES `categories`(`id`);
ALTER TABLE `products` ADD CONSTRAINT `fk_products_warehouse_id` FOREIGN KEY(`warehouse_id`) REFERENCES `warehouses`(`id`);
ALTER TABLE `vehicles` ADD CONSTRAINT `fk_vehicles_status_id` FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);
ALTER TABLE `users` ADD CONSTRAINT `fk_users_role_id` FOREIGN KEY(`role_id`) REFERENCES `roles`(`id`);
ALTER TABLE `users` ADD CONSTRAINT `fk_users_status_id` FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);
ALTER TABLE `vehicles` ADD CONSTRAINT `fk_vehicles_current_driver_id` FOREIGN KEY(`current_driver_id`) REFERENCES `users`(`id`);
ALTER TABLE `delivery_proofs` ADD CONSTRAINT `fk_delivery_proofs_uploaded_by` FOREIGN KEY(`uploaded_by`) REFERENCES `users`(`id`);
ALTER TABLE `orders` ADD CONSTRAINT `fk_orders_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);
ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_driver_id` FOREIGN KEY(`driver_id`) REFERENCES `users`(`id`);
ALTER TABLE `products` ADD CONSTRAINT `fk_products_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);
ALTER TABLE `stores` ADD CONSTRAINT `fk_stores_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);
ALTER TABLE `routes` ADD CONSTRAINT `fk_routes_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);
ALTER TABLE `inventory_transactions` ADD CONSTRAINT `fk_inventory_transactions_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);
ALTER TABLE `payments` ADD CONSTRAINT `fk_payments_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);
ALTER TABLE `warehouses` ADD CONSTRAINT `fk_warehouses_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);

COMMIT;
