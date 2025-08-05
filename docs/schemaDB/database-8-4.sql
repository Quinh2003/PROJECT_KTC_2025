-- =====================================================================================
-- CSDL HỆ THỐNG QUẢN LÝ GIAO HÀNG VÀ LOGISTICS - KTC PROJECT 2025
-- =====================================================================================
-- Mô tả: Cơ sở dữ liệu quản lý toàn diện hệ thống logistics và giao hàng
-- Bao gồm: Quản lý đơn hàng, sản phẩm, phương tiện, tài xế, kho bãi, 
--          theo dõi giao hàng, thanh toán và báo cáo
-- Phiên bản: 9.4
-- Ngày tạo: 2025
-- Engine: MySQL 8.0+
-- Mã hóa: UTF8MB4
-- =====================================================================================

START TRANSACTION;

-- =====================================================================================
-- BẢNG QUẢN LÝ PHƯƠNG TIỆN VẬN CHUYỂN
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `vehicles` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của phương tiện',
    `license_plate` VARCHAR(20) NOT NULL UNIQUE COMMENT 'Biển số xe',
    `vehicle_type` VARCHAR(50) NOT NULL DEFAULT 'TRUCK' COMMENT 'Loại phương tiện (xe tải, xe van, xe máy, ô tô)',
    `capacity_weight_kg` DECIMAL(10,2) DEFAULT 0.00 COMMENT 'Trọng tải tối đa của phương tiện (kg)',
    `capacity_volume_m3` DECIMAL(10,2) DEFAULT 0.00 COMMENT 'Thể tích chứa hàng tối đa (m3)',
    `status_id` TINYINT UNSIGNED NOT NULL COMMENT 'Trạng thái xe (hoạt động, bảo trì, ngừng hoạt động)',
    `current_driver_id` BIGINT COMMENT 'ID tài xế hiện tại, NULL nếu chưa phân công',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo bản ghi',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật cuối cùng',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về phương tiện',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG CHI TIẾT CÁC MẶT HÀNG TRONG ĐỢN HÀNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `order_items` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của mục hàng',
    `order_id` BIGINT NOT NULL COMMENT 'Mã đơn hàng chứa mục hàng này',
    `product_id` BIGINT NOT NULL COMMENT 'Mã sản phẩm trong mục hàng',
    `quantity` INT NOT NULL COMMENT 'Số lượng sản phẩm đặt mua',
    `unit_price` DECIMAL(15,2) NOT NULL,
    `shipping_fee` DECIMAL(15,2) DEFAULT 0.00 COMMENT 'Phí vận chuyển mà người dùng phải trả',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo mục hàng',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật mục hàng cuối cùng',
    `notes` TEXT COMMENT 'Ghi chú đặc biệt cho mục hàng này',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG CHỨNG CHỈ GIAO HÀNG THÀNH CÔNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `delivery_proofs` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của bằng chứng giao hàng',
    `proof_type` VARCHAR(50) NOT NULL DEFAULT 'PHOTO' COMMENT 'Loại bằng chứng (ảnh, chữ ký, ghi âm)',
    `file_path` VARCHAR(500) COMMENT 'Đường dẫn lưu trữ file bằng chứng',
    `file_name` VARCHAR(255) COMMENT 'Tên file bằng chứng gốc',
    `recipient_name` VARCHAR(255) COMMENT 'Tên người nhận hàng thực tế',
    `recipient_signature` TEXT COMMENT 'Dữ liệu chữ ký số của người nhận',
    `captured_at` DATETIME COMMENT 'Thời gian chụp/ghi nhận bằng chứng',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo bản ghi bằng chứng',
    `order_id` BIGINT NOT NULL COMMENT 'Mã đơn hàng liên quan đến bằng chứng',
    `uploaded_by` BIGINT COMMENT 'ID người dùng (tài xế) upload bằng chứng',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về bằng chứng giao hàng',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG ĐỊA CHỈ GIAO HÀNG VÀ LẤY HÀNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `addresses` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của địa chỉ',
    `order_id` BIGINT NOT NULL COMMENT 'Mã đơn hàng liên quan',
    `address_type` VARCHAR(50) NOT NULL COMMENT 'Loại địa chỉ (giao hàng, lấy hàng, trả hàng)',
    `address` VARCHAR(500) NOT NULL COMMENT 'Địa chỉ đầy đủ (số nhà, đường, phường/xã)',
    `latitude` DECIMAL(10,8) COMMENT 'Tọa độ vĩ độ (GPS)',
    `longitude` DECIMAL(11,8) COMMENT 'Tọa độ kinh độ (GPS)',
    `city` VARCHAR(100) COMMENT 'Thành phố/Tỉnh',
    `state` VARCHAR(100) COMMENT 'Bang/Khu vực',
    `country` VARCHAR(100) DEFAULT 'Vietnam' COMMENT 'Quốc gia',
    `region` VARCHAR(100) COMMENT 'Vùng miền/Khu vực',
    `postal_code` VARCHAR(20) COMMENT 'Mã bưu điện',
    `contact_name` VARCHAR(255) COMMENT 'Tên người liên hệ tại địa chỉ',
    `contact_phone` VARCHAR(20) COMMENT 'Số điện thoại người liên hệ',
    `contact_email` VARCHAR(255) COMMENT 'Email người liên hệ',
    `floor_number` VARCHAR(10) COMMENT 'Số tầng/lầu của tòa nhà',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo địa chỉ',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật địa chỉ cuối cùng',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ SẢN PHẨM
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `products` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của sản phẩm',
    `product_card_id` VARCHAR(50) NOT NULL UNIQUE COMMENT 'Mã thẻ sản phẩm cho catalog',
    `product_code` VARCHAR(50) NOT NULL UNIQUE COMMENT 'Mã SKU/mã nội bộ sản phẩm',
    `name` VARCHAR(255) NOT NULL COMMENT 'Tên hiển thị sản phẩm',
    `description` TEXT COMMENT 'Mô tả chi tiết sản phẩm',
    `category_id` BIGINT NOT NULL COMMENT 'Phân loại danh mục sản phẩm',
    `unit_price` DECIMAL(15,2) NOT NULL COMMENT 'Giá bán trên một đơn vị',
    `weight` DECIMAL(10,3) DEFAULT 0.000 COMMENT 'Trọng lượng sản phẩm (kg)',
    `volume` DECIMAL(10,3) DEFAULT 0.000 COMMENT 'Thể tích sản phẩm (m3)',
    `is_fragile` tinyint NOT NULL DEFAULT 0 COMMENT 'Cờ hàng dễ vỡ: 0=Không, 1=Có',
    `stock_quantity` INT NOT NULL DEFAULT 0 COMMENT 'Số lượng tồn kho hiện tại',
    `product_image` VARCHAR(500) COMMENT 'URL/đường dẫn ảnh sản phẩm',
    `product_status` tinyint NOT NULL DEFAULT 1 COMMENT 'Trạng thái sản phẩm: 0=Ngừng bán, 1=Đang bán',
    `warehouse_id` BIGINT COMMENT 'Kho chính chứa sản phẩm này',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo sản phẩm',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật sản phẩm cuối cùng',
    `created_by` BIGINT COMMENT 'ID người dùng tạo sản phẩm này',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về sản phẩm',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG DANH MỤC SẢN PHẨM
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `categories` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của danh mục',
    `category_id` VARCHAR(50) NOT NULL UNIQUE COMMENT 'Mã danh mục nghiệp vụ (dễ đọc)',
    `name` VARCHAR(255) NOT NULL COMMENT 'Tên hiển thị của danh mục',
    `description` TEXT COMMENT 'Mô tả chi tiết về danh mục',
    `parent_id` BIGINT COMMENT 'ID danh mục cha (cho cấu trúc cây)',
    `is_active` tinyint NOT NULL DEFAULT 1 COMMENT 'Trạng thái sử dụng: 0=Ngừng, 1=Hoạt động',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo danh mục',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật danh mục cuối cùng',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về danh mục',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ CỮA HÀNG/ĐIỂM BAN HÀNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `stores` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của cửa hàng',
    `store_code` VARCHAR(50) NOT NULL UNIQUE COMMENT 'Mã cửa hàng nghiệp vụ (dễ đọc)',
    `store_name` VARCHAR(255) NOT NULL COMMENT 'Tên hiển thị của cửa hàng',
    `email` VARCHAR(255) COMMENT 'Email liên hệ của cửa hàng',
    `phone` VARCHAR(20) NOT NULL COMMENT 'Số điện thoại liên hệ cửa hàng',
    `address` TEXT NOT NULL COMMENT 'Địa chỉ đầy đủ của cửa hàng',
    `latitude` DECIMAL(10,8) COMMENT 'Tọa độ vĩ độ cửa hàng',
    `longitude` DECIMAL(11,8) COMMENT 'Tọa độ kinh độ cửa hàng',
    `is_active` tinyint NOT NULL DEFAULT 1 COMMENT 'Trạng thái hoạt động: 0=Đóng cửa, 1=Hoạt động',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo cửa hàng',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật cửa hàng cuối cùng',
    `created_by` BIGINT COMMENT 'ID người dùng tạo cửa hàng này',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về cửa hàng',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ TUYẾN ĐƯỜNG VẬN CHUYỂN
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `routes` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của tuyến đường',
    `name` VARCHAR(255) NOT NULL COMMENT 'Tên tuyến đường (mô tả ngắn)',
    `waypoints` JSON NOT NULL COMMENT 'Danh sách các điểm dừng trên tuyến (JSON)',
    `estimated_distance_km` DECIMAL(10,2) DEFAULT 0.00 COMMENT 'Khoảng cách dự kiến (km)',
    `estimated_duration_minutes` INT DEFAULT 0 COMMENT 'Thời gian dự kiến (phút)',
    `estimated_cost` DECIMAL(15,2) DEFAULT 0.00 COMMENT 'Chi phí dự kiến cho tuyến',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo tuyến đường',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật tuyến cuối cùng',
    `completed_at` DATETIME COMMENT 'Thời gian hoàn thành tuyến đường',
    `created_by` BIGINT COMMENT 'ID người dùng tạo tuyến đường',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về tuyến đường',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ NGƯỜI DÙNG HỆ THỐNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `users` (
    -- Người dùng hệ thống bao gồm tài xế, điều phối, quản trị, v.v.
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của người dùng',
    `username` VARCHAR(100) NOT NULL UNIQUE COMMENT 'Tên đăng nhập duy nhất',
    `email` VARCHAR(255) NOT NULL UNIQUE COMMENT 'Địa chỉ email để đăng nhập và nhận thông báo',
    `password` VARCHAR(255) NOT NULL COMMENT 'Mật khẩu đã mã hóa để xác thực',
    `full_name` VARCHAR(255) COMMENT 'Họ tên đầy đủ để hiển thị',
    `phone` VARCHAR(20) COMMENT 'Số điện thoại liên hệ',
    `role_id` BIGINT NOT NULL COMMENT 'Vai trò người dùng (admin, điều phối, tài xế, xem)',
    `status_id` TINYINT UNSIGNED COMMENT 'Trạng thái tài khoản (hoạt động, ngừng hoạt động, tạm khóa)',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo tài khoản',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật tài khoản cuối cùng',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về người dùng',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ TRẠNG THÁI HỆ THỐNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `status` (
    `id` TINYINT UNSIGNED AUTO_INCREMENT COMMENT 'Mã định danh trạng thái duy nhất (1-255)',
    `type` VARCHAR(50) NOT NULL COMMENT 'Phân loại trạng thái (phương tiện, đơn hàng, thanh toán, người dùng, v.v.)',
    `name` VARCHAR(100) NOT NULL COMMENT 'Tên trạng thái dễ đọc',
    `description` TEXT COMMENT 'Mô tả chi tiết ý nghĩa của trạng thái',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo trạng thái',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật trạng thái cuối cùng',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ GIAO DỊCH KHO BÃI
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `warehouse_transactions` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của giao dịch kho bãi',
    `product_id` BIGINT NOT NULL COMMENT 'Mã sản phẩm tham gia giao dịch',
    `warehouse_id` BIGINT NOT NULL COMMENT 'Mã kho hàng thực hiện giao dịch',
    `status_id` TINYINT UNSIGNED NOT NULL COMMENT 'Trạng thái giao dịch (thành công, thất bại, chờ xử lý)',
    `transaction_type` VARCHAR(50) NOT NULL DEFAULT 'IN' COMMENT 'Loại giao dịch (nhập kho=IN, xuất kho=OUT, chuyển kho=TRANSFER)',
    `quantity` INT NOT NULL COMMENT 'Số lượng sản phẩm trong giao dịch',
    `unit_cost` DECIMAL(15,2) DEFAULT 0.00 COMMENT 'Giá vốn trên một đơn vị sản phẩm',
    `transaction_date` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian thực hiện giao dịch kho bãi',
    `order_id` BIGINT COMMENT 'Mã đơn hàng liên quan (nếu có)',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo bản ghi giao dịch',
    `created_by` BIGINT COMMENT 'ID người dùng tạo giao dịch',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về giao dịch kho bãi',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG NHẬT KÝ HOẠT ĐỘNG HỆ THỐNG
-- =====================================================================================
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



-- =====================================================================================
-- BẢNG QUẢN LÝ ĐƠN HÀNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `orders` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của đơn hàng',
    `status_id` TINYINT UNSIGNED NOT NULL COMMENT 'Trạng thái đơn hàng (chờ xử lý, đang xử lý, hoàn thành, hủy)',
    `store_id` BIGINT COMMENT 'ID cửa hàng liên kết, NULL cho đơn hàng online',
    `description` TEXT COMMENT 'Mô tả và chi tiết đơn hàng',
    `total_amount` DECIMAL(15,2) DEFAULT 0.00 COMMENT 'Tổng số tiền đơn hàng bao gồm phí',
    `benefit_per_order` DECIMAL(15,2) DEFAULT 0.00 COMMENT 'Lợi nhuận/biên lãi dự kiến từ đơn hàng này',
    `order_profit_per_order` DECIMAL(15,2) DEFAULT 0.00 COMMENT 'Lợi nhuận được tính toán cho đơn hàng này',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về đơn hàng',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo đơn hàng',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật đơn hàng cuối cùng',
    `created_by` BIGINT COMMENT 'ID người dùng tạo đơn hàng này',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ THANH TOÁN
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `payments` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của thanh toán',
    `order_id` BIGINT NOT NULL COMMENT 'Mã đơn hàng được thanh toán',
    `amount` DECIMAL(15,2) NOT NULL 'Tổng số tiền thanh toán',
    `payment_method` VARCHAR(50) NOT NULL DEFAULT 'CASH' COMMENT 'Phương thức thanh toán (tiền mặt, thẻ, chuyển khoản)',
    `status_id` TINYINT UNSIGNED NOT NULL COMMENT 'Trạng thái thanh toán (thành công, thất bại, chờ xử lý)',
    `transaction_id` VARCHAR(255) COMMENT 'Mã giao dịch từ cổng thanh toán',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo bản ghi thanh toán',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật thanh toán cuối cùng',
    `created_by` BIGINT COMMENT 'ID người dùng tạo bản ghi thanh toán',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về thanh toán',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ GIAO HÀNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `deliveries` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của giao hàng',
    `order_id` BIGINT NOT NULL COMMENT 'Đơn hàng liên kết đang được giao',
    `delivery_fee` DECIMAL(15,2) DEFAULT 0.00 COMMENT 'Chi phí vận chuyển doanh nghiệp logistic phải trả',
    `transport_mode` VARCHAR(50) DEFAULT 'ROAD' COMMENT 'Phương thức vận chuyển (đường bộ, hàng không, đường biển, đường sắt)',
    `service_type` VARCHAR(50) DEFAULT 'STANDARD' COMMENT 'Mức dịch vụ (tiêu chuẩn, nhanh, ưu tiên)',
    `order_date` DATETIME NOT NULL COMMENT 'Thời điểm đặt hàng',
    `pickup_date` DATETIME COMMENT 'Thời điểm lấy hàng',
    `schedule_delivery_time` DATETIME COMMENT 'Thời gian giao hàng dự kiến',
    `actual_delivery_time` DATETIME COMMENT 'Thời gian hoàn thành giao hàng thực tế',
    `late_delivery_risk` tinyint NOT NULL DEFAULT 0 COMMENT 'Cờ rủi ro giao hàng trễ: 0=Không, 1=Có',
    `vehicle_id` BIGINT NOT NULL COMMENT 'Phương tiện được phân công cho giao hàng này',
    `driver_id` BIGINT COMMENT 'Tài xế được phân công cho giao hàng này',
    `tracking_id` BIGINT COMMENT 'Bản ghi theo dõi GPS cho giao hàng này',
    `route_id` BIGINT COMMENT 'Tuyến đường tối ưu cho giao hàng này',
    `delivery_attempts` INT DEFAULT 0 COMMENT 'Số lần thử giao hàng đã thực hiện',
    `delivery_notes` TEXT COMMENT 'Hướng dẫn đặc biệt và ghi chú cho giao hàng',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo bản ghi giao hàng',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật bản ghi giao hàng cuối cùng',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG QUẢN LÝ VAI TRÒ NGƯỜI DÙNG
-- =====================================================================================
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



-- =====================================================================================
-- BẢNG QUẢN LÝ KHO BÃI
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `warehouses` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của kho bãi',
    `warehouse_code` VARCHAR(50) NOT NULL UNIQUE COMMENT 'Mã kho nghiệp vụ (dễ đọc)',
    `name` VARCHAR(255) NOT NULL COMMENT 'Tên hiển thị của kho bãi',
    `address` TEXT NOT NULL COMMENT 'Địa chỉ đầy đủ của kho bãi',
    `latitude` DECIMAL(10,8) COMMENT 'Tọa độ vĩ độ kho bãi',
    `longitude` DECIMAL(11,8) COMMENT 'Tọa độ kinh độ kho bãi',
    `capacity_m3` DECIMAL(10,2) DEFAULT 0.00 COMMENT 'Sức chứa tối đa của kho (m3)',
    `is_active` tinyint NOT NULL DEFAULT 1 COMMENT 'Trạng thái hoạt động: 0=Đóng cửa, 1=Hoạt động',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo kho bãi',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật kho bãi cuối cùng',
    `created_by` BIGINT COMMENT 'ID người dùng tạo kho bãi này',
    `notes` TEXT COMMENT 'Ghi chú bổ sung về kho bãi',
    PRIMARY KEY (`id`)
);



-- =====================================================================================
-- BẢNG THEO DÕI GPS GIAO HÀNG
-- =====================================================================================
CREATE TABLE IF NOT EXISTS `delivery_tracking` (
    `id` BIGINT AUTO_INCREMENT COMMENT 'Mã định danh duy nhất của điểm theo dõi',
    `vehicle_id` BIGINT NOT NULL COMMENT 'Mã phương tiện đang được theo dõi',
    `status_id` TINYINT UNSIGNED NOT NULL COMMENT 'Trạng thái giao hàng tại thời điểm này',
    `latitude` DECIMAL(10,8) COMMENT 'Tọa độ vĩ độ hiện tại',
    `longitude` DECIMAL(11,8) COMMENT 'Tọa độ kinh độ hiện tại',
    `timestamp` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian ghi nhận điểm theo dõi',
    `location` VARCHAR(255) COMMENT 'Tên địa điểm hiện tại (dễ đọc)',
    `notes` TEXT COMMENT 'Ghi chú bổ sung tại điểm theo dõi',
    `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT 'Thời gian tạo bản ghi theo dõi',
    `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Thời gian cập nhật bản ghi cuối cùng',
    PRIMARY KEY (`id`)
);


-- =====================================================================================
-- CÁC RÀNG BUỘC KHÓA NGOẠI (FOREIGN KEY CONSTRAINTS)
-- =====================================================================================
-- Thiết lập mối quan hệ tham chiếu giữa các bảng để đảm bảo tính toàn vẹn dữ liệu
-- Tự động xóa/cập nhật cascading khi cần thiết
-- =====================================================================================
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
ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_order_id` FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`);
ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_product_id` FOREIGN KEY(`product_id`) REFERENCES `products`(`id`);
ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_status_id` FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);
ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_warehouse_id` FOREIGN KEY(`warehouse_id`) REFERENCES `warehouses`(`id`);
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
ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);
ALTER TABLE `payments` ADD CONSTRAINT `fk_payments_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);
ALTER TABLE `warehouses` ADD CONSTRAINT `fk_warehouses_created_by` FOREIGN KEY(`created_by`) REFERENCES `users`(`id`);

-- =====================================================================================
-- DATA VALIDATION CONSTRAINTS - Ràng buộc kiểm tra tính hợp lệ của dữ liệu
-- =====================================================================================
-- Các ràng buộc này đảm bảo tính toàn vẹn dữ liệu và ngăn chặn việc nhập dữ liệu không hợp lệ
-- vào hệ thống quản lý chuỗi cung ứng và logistics

-- Ràng buộc kiểm tra sức chứa xe tải
-- Đảm bảo trọng lượng và thể tích chứa hàng của xe không được âm
ALTER TABLE vehicles ADD CONSTRAINT chk_vehicle_capacity 
    CHECK (capacity_weight_kg >= 0 AND capacity_volume_m3 >= 0);

-- Ràng buộc kiểm tra giá sản phẩm
-- Đảm bảo đơn giá sản phẩm không được âm (có thể bằng 0 cho sản phẩm miễn phí)
ALTER TABLE products ADD CONSTRAINT chk_product_price_positive 
    CHECK (unit_price >= 0);

-- Ràng buộc kiểm tra trọng lượng và thể tích sản phẩm
-- Đảm bảo trọng lượng và thể tích sản phẩm không được âm
ALTER TABLE products ADD CONSTRAINT chk_product_weight_volume 
    CHECK (weight >= 0 AND volume >= 0);

-- Ràng buộc kiểm tra số lượng trong đơn hàng
-- Đảm bảo số lượng sản phẩm trong mỗi item của đơn hàng phải lớn hơn 0
ALTER TABLE order_items ADD CONSTRAINT chk_order_item_quantity 
    CHECK (quantity > 0);

-- Ràng buộc kiểm tra số tiền thanh toán
-- Đảm bảo số tiền thanh toán phải lớn hơn 0 (không cho phép thanh toán số âm)
ALTER TABLE payments ADD CONSTRAINT chk_payment_amount 
    CHECK (amount > 0);

-- Ràng buộc kiểm tra số lần giao hàng
-- Đảm bảo số lần thử giao hàng không được âm (tối thiểu là 0)
ALTER TABLE deliveries ADD CONSTRAINT chk_delivery_attempts 
    CHECK (delivery_attempts >= 0);


-- =====================================================================================
-- PERFORMANCE INDEXES - CRITICAL FOR PRODUCTION
-- =====================================================================================
-- Thêm các indexes quan trọng cho hiệu suất truy vấn trong hệ thống logistics
-- Các indexes này được thiết kế dựa trên các truy vấn thường xuyên của hệ thống
-- =====================================================================================

-- Indexes cho bảng ORDERS (truy vấn nhiều nhất)
CREATE INDEX idx_orders_status ON orders(status_id) COMMENT 'Tìm đơn hàng theo trạng thái';
CREATE INDEX idx_orders_customer ON orders(order_customer_id) COMMENT 'Tìm đơn hàng theo khách hàng';
CREATE INDEX idx_orders_status_created ON orders(status_id, created_at) COMMENT 'Sắp xếp đơn hàng theo trạng thái và thời gian';
CREATE INDEX idx_orders_store ON orders(store_id) COMMENT 'Tìm đơn hàng theo cửa hàng';
CREATE INDEX idx_orders_created_by ON orders(created_by) COMMENT 'Tìm đơn hàng theo người tạo';

-- Indexes cho bảng DELIVERIES (core logistics operations)
CREATE INDEX idx_deliveries_vehicle ON deliveries(vehicle_id) COMMENT 'Tìm giao hàng theo phương tiện';
CREATE INDEX idx_deliveries_driver ON deliveries(driver_id) COMMENT 'Tìm giao hàng theo tài xế';
CREATE INDEX idx_deliveries_schedule_time ON deliveries(schedule_delivery_time) COMMENT 'Sắp xếp theo thời gian giao hàng dự kiến';
CREATE INDEX idx_deliveries_route ON deliveries(route_id) COMMENT 'Tìm giao hàng theo tuyến đường';
CREATE INDEX idx_deliveries_order ON deliveries(order_id) COMMENT 'Tìm giao hàng theo đơn hàng';
CREATE INDEX idx_deliveries_tracking ON deliveries(tracking_id) COMMENT 'Tìm giao hàng theo tracking';

-- Indexes cho bảng DELIVERY_TRACKING (real-time tracking)
CREATE INDEX idx_delivery_tracking_vehicle ON delivery_tracking(vehicle_id) COMMENT 'Tracking theo phương tiện';
CREATE INDEX idx_delivery_tracking_vehicle_time ON delivery_tracking(vehicle_id, timestamp DESC) COMMENT 'Tracking theo thời gian mới nhất';
CREATE INDEX idx_delivery_tracking_status ON delivery_tracking(status_id) COMMENT 'Tracking theo trạng thái';
CREATE INDEX idx_delivery_tracking_timestamp ON delivery_tracking(timestamp DESC) COMMENT 'Sắp xếp tracking theo thời gian';

-- Indexes cho bảng PRODUCTS (catalog operations)
CREATE INDEX idx_products_category ON products(category_id) COMMENT 'Tìm sản phẩm theo danh mục';
CREATE INDEX idx_products_warehouse ON products(warehouse_id) COMMENT 'Tìm sản phẩm theo kho';
CREATE INDEX idx_products_status ON products(product_status) COMMENT 'Tìm sản phẩm theo trạng thái';
CREATE INDEX idx_products_code ON products(product_code) COMMENT 'Tìm sản phẩm theo mã SKU';
CREATE INDEX idx_products_card_id ON products(product_card_id) COMMENT 'Tìm sản phẩm theo mã thẻ';

-- Indexes cho bảng ORDER_ITEMS (order details)
CREATE INDEX idx_order_items_order ON order_items(order_id) COMMENT 'Tìm items theo đơn hàng';
CREATE INDEX idx_order_items_product ON order_items(product_id) COMMENT 'Tìm items theo sản phẩm';
CREATE INDEX idx_order_items_order_product ON order_items(order_id, product_id) COMMENT 'Composite index cho order-product';

-- Indexes cho bảng WAREHOUSE_TRANSACTIONS (inventory management)
CREATE INDEX idx_warehouse_trans_product ON warehouse_transactions(product_id) COMMENT 'Giao dịch theo sản phẩm';
CREATE INDEX idx_warehouse_trans_warehouse ON warehouse_transactions(warehouse_id) COMMENT 'Giao dịch theo kho';
CREATE INDEX idx_warehouse_trans_date ON warehouse_transactions(transaction_date DESC) COMMENT 'Giao dịch theo thời gian';
CREATE INDEX idx_warehouse_trans_type ON warehouse_transactions(transaction_type) COMMENT 'Giao dịch theo loại';
CREATE INDEX idx_warehouse_trans_order ON warehouse_transactions(order_id) COMMENT 'Giao dịch theo đơn hàng';
CREATE INDEX idx_warehouse_trans_product_date ON warehouse_transactions(product_id, transaction_date DESC) COMMENT 'Lịch sử giao dịch sản phẩm';

-- Indexes cho bảng PAYMENTS (financial operations)
CREATE INDEX idx_payments_order ON payments(order_id) COMMENT 'Thanh toán theo đơn hàng';
CREATE INDEX idx_payments_status ON payments(status_id) COMMENT 'Thanh toán theo trạng thái';
CREATE INDEX idx_payments_method ON payments(payment_method) COMMENT 'Thanh toán theo phương thức';
CREATE INDEX idx_payments_transaction ON payments(transaction_id) COMMENT 'Thanh toán theo mã giao dịch';
CREATE INDEX idx_payments_created ON payments(created_at DESC) COMMENT 'Thanh toán theo thời gian tạo';

-- Indexes cho bảng USERS (authentication & authorization)
CREATE INDEX idx_users_role ON users(role_id) COMMENT 'Người dùng theo vai trò';
CREATE INDEX idx_users_status ON users(status_id) COMMENT 'Người dùng theo trạng thái';
CREATE INDEX idx_users_email ON users(email) COMMENT 'Tìm người dùng theo email';
CREATE INDEX idx_users_username ON users(username) COMMENT 'Tìm người dùng theo username';

-- Indexes cho bảng VEHICLES (fleet management)
CREATE INDEX idx_vehicles_status ON vehicles(status_id) COMMENT 'Phương tiện theo trạng thái';
CREATE INDEX idx_vehicles_driver ON vehicles(current_driver_id) COMMENT 'Phương tiện theo tài xế hiện tại';
CREATE INDEX idx_vehicles_type ON vehicles(vehicle_type) COMMENT 'Phương tiện theo loại';
CREATE INDEX idx_vehicles_license ON vehicles(license_plate) COMMENT 'Phương tiện theo biển số';

-- Indexes cho bảng ADDRESSES (geographical operations)
CREATE INDEX idx_addresses_order ON addresses(order_id) COMMENT 'Địa chỉ theo đơn hàng';
CREATE INDEX idx_addresses_type ON addresses(address_type) COMMENT 'Địa chỉ theo loại';
CREATE INDEX idx_addresses_city ON addresses(city) COMMENT 'Địa chỉ theo thành phố';
CREATE INDEX idx_addresses_coordinates ON addresses(latitude, longitude) COMMENT 'Tìm kiếm theo tọa độ GPS';

-- Indexes cho bảng DELIVERY_PROOFS (proof management)
CREATE INDEX idx_delivery_proofs_order ON delivery_proofs(order_id) COMMENT 'Bằng chứng theo đơn hàng';
CREATE INDEX idx_delivery_proofs_type ON delivery_proofs(proof_type) COMMENT 'Bằng chứng theo loại';
CREATE INDEX idx_delivery_proofs_uploader ON delivery_proofs(uploaded_by) COMMENT 'Bằng chứng theo người upload';
CREATE INDEX idx_delivery_proofs_captured ON delivery_proofs(captured_at DESC) COMMENT 'Bằng chứng theo thời gian chụp';

-- Indexes cho bảng ACTIVITY_LOGS (audit trail)
CREATE INDEX idx_activity_logs_actor ON activity_logs(actor_id) COMMENT 'Log theo người thực hiện';
CREATE INDEX idx_activity_logs_action_time ON activity_logs(action_timestamp DESC) COMMENT 'Log theo thời gian hành động';
CREATE INDEX idx_activity_logs_table ON activity_logs(table_name) COMMENT 'Log theo bảng bị ảnh hưởng';
CREATE INDEX idx_activity_logs_action_type ON activity_logs(action_type) COMMENT 'Log theo loại hành động';
CREATE INDEX idx_activity_logs_record ON activity_logs(table_name, record_id) COMMENT 'Log theo bản ghi cụ thể';

-- Indexes cho bảng CATEGORIES (product categorization)
CREATE INDEX idx_categories_parent ON categories(parent_id) COMMENT 'Danh mục theo danh mục cha';
CREATE INDEX idx_categories_active ON categories(is_active) COMMENT 'Danh mục đang hoạt động';
CREATE INDEX idx_categories_category_id ON categories(category_id) COMMENT 'Tìm theo mã danh mục';

-- Indexes cho bảng STORES (store operations)
CREATE INDEX idx_stores_active ON stores(is_active) COMMENT 'Cửa hàng đang hoạt động';
CREATE INDEX idx_stores_code ON stores(store_code) COMMENT 'Tìm cửa hàng theo mã';
CREATE INDEX idx_stores_coordinates ON stores(latitude, longitude) COMMENT 'Tìm cửa hàng theo tọa độ';

-- Indexes cho bảng WAREHOUSES (warehouse operations)
CREATE INDEX idx_warehouses_active ON warehouses(is_active) COMMENT 'Kho đang hoạt động';
CREATE INDEX idx_warehouses_code ON warehouses(warehouse_code) COMMENT 'Tìm kho theo mã';
CREATE INDEX idx_warehouses_coordinates ON warehouses(latitude, longitude) COMMENT 'Tìm kho theo tọa độ';

-- Indexes cho bảng ROUTES (route optimization)
CREATE INDEX idx_routes_created_by ON routes(created_by) COMMENT 'Tuyến đường theo người tạo';
CREATE INDEX idx_routes_completed ON routes(completed_at) COMMENT 'Tuyến đường đã hoàn thành';
CREATE INDEX idx_routes_estimated_cost ON routes(estimated_cost) COMMENT 'Sắp xếp theo chi phí dự kiến';

-- Indexes cho bảng STATUS (system status)
CREATE INDEX idx_status_type ON status(type) COMMENT 'Trạng thái theo loại';
CREATE INDEX idx_status_name ON status(name) COMMENT 'Tìm trạng thái theo tên';

-- Indexes cho bảng ROLES (role management)
CREATE INDEX idx_roles_active ON roles(is_active) COMMENT 'Vai trò đang hoạt động';
CREATE INDEX idx_roles_name ON roles(role_name) COMMENT 'Tìm vai trò theo tên';

-- =====================================================================================
-- KẾT THÚC TRANSACTION VÀ COMMIT DỮ LIỆU
-- =====================================================================================
COMMIT;

-- =====================================================================================
-- HẾT FILE DATABASE SCHEMA
-- =====================================================================================
-- Tổng kết:
-- - 16 bảng chính cho hệ thống logistics và giao hàng
-- - Bao gồm: vehicles, order_items, delivery_proofs, addresses, products, categories
--           stores, routes, users, status, warehouse_transactions, activity_logs
--           orders, payments, deliveries, roles, warehouses, delivery_tracking
-- - Thiết lập đầy đủ foreign key constraints
-- - Hỗ trợ tracking GPS, proof delivery, inventory management
-- - Phân quyền người dùng và audit trail
-- =====================================================================================
