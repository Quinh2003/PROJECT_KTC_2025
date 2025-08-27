-- ==================================================================================
-- SCRIPT SETUP TEST DATA CHO CHỨC NĂNG HÓA ĐƠN ĐIỆN TỬ
-- ==================================================================================
-- Mục đích: Tạo dữ liệu test để có thể test việc tạo hóa đơn điện tử
-- Điều kiện: Đơn hàng đã hoàn thành giao hàng với actual_delivery_time
-- ==================================================================================

-- Kiểm tra dữ liệu hiện tại
SELECT 'CURRENT ORDER STATUS' as info;
SELECT o.id, o.status_id, s.name as status_name, o.total_amount 
FROM orders o 
LEFT JOIN status s ON o.status_id = s.id 
ORDER BY o.id LIMIT 5;

SELECT 'CURRENT DELIVERY STATUS' as info;
SELECT d.id, d.order_id, d.actual_delivery_time, d.schedule_delivery_time 
FROM deliveries d 
ORDER BY d.id LIMIT 5;

-- ==================================================================================
-- BƯỚC 1: Tạo/Cập nhật Status cần thiết
-- ==================================================================================

-- Tạo status PROCESSED cho orders nếu chưa có
INSERT IGNORE INTO status (type, name, description) 
VALUES ('ORDER', 'PROCESSED', 'Đơn hàng đã được xử lý và sẵn sàng giao hàng');

-- Tạo status COMPLETED cho orders nếu chưa có  
INSERT IGNORE INTO status (type, name, description)
VALUES ('ORDER', 'COMPLETED', 'Đơn hàng đã hoàn thành');

-- Tạo status DELIVERED cho deliveries nếu chưa có
INSERT IGNORE INTO status (type, name, description)
VALUES ('DELIVERY', 'DELIVERED', 'Đã giao hàng thành công');

-- ==================================================================================
-- BƯỚC 2: Lấy ID của các status đã tạo
-- ==================================================================================

SET @status_processed = (SELECT id FROM status WHERE type = 'ORDER' AND name = 'PROCESSED' LIMIT 1);
SET @status_completed = (SELECT id FROM status WHERE type = 'ORDER' AND name = 'COMPLETED' LIMIT 1);
SET @status_delivered = (SELECT id FROM status WHERE type = 'DELIVERY' AND name = 'DELIVERED' LIMIT 1);

SELECT @status_processed as processed_id, @status_completed as completed_id, @status_delivered as delivered_id;

-- ==================================================================================
-- BƯỚC 3: Cập nhật đơn hàng hiện có để có trạng thái hợp lệ
-- ==================================================================================

-- Cập nhật order đầu tiên về trạng thái COMPLETED
UPDATE orders 
SET status_id = @status_completed,
    total_amount = CASE WHEN total_amount = 0 THEN 500000 ELSE total_amount END,
    updated_at = CURRENT_TIMESTAMP
WHERE id = 1;

-- ==================================================================================
-- BƯỚC 4: Tạo/Cập nhật delivery cho order test
-- ==================================================================================

-- Tìm vehicle và user để gán cho delivery
SET @vehicle_id = (SELECT id FROM vehicles LIMIT 1);
SET @driver_id = (SELECT id FROM users WHERE role_id = (SELECT id FROM roles WHERE role_name = 'DRIVER' LIMIT 1) LIMIT 1);
SET @admin_id = (SELECT id FROM users WHERE role_id = (SELECT id FROM roles WHERE role_name = 'ADMIN' LIMIT 1) LIMIT 1);

-- Nếu không có driver, dùng admin
SET @driver_id = COALESCE(@driver_id, @admin_id);

-- Kiểm tra xem order 1 đã có delivery chưa
SET @existing_delivery = (SELECT id FROM deliveries WHERE order_id = 1 LIMIT 1);

-- Nếu chưa có delivery, tạo mới
INSERT INTO deliveries (
    order_id, 
    delivery_fee, 
    transport_mode, 
    service_type,
    order_date,
    pickup_date,
    schedule_delivery_time,
    actual_delivery_time,  -- QUAN TRỌNG: Set thời gian hoàn thành
    late_delivery_risk,
    vehicle_id,
    driver_id,
    delivery_attempts,
    delivery_notes,
    created_at,
    updated_at
) 
SELECT 
    1 as order_id,
    50000 as delivery_fee,
    'ROAD' as transport_mode,
    'STANDARD' as service_type,
    DATE_SUB(NOW(), INTERVAL 3 DAY) as order_date,
    DATE_SUB(NOW(), INTERVAL 2 DAY) as pickup_date,
    DATE_SUB(NOW(), INTERVAL 1 DAY) as schedule_delivery_time,
    DATE_SUB(NOW(), INTERVAL 1 DAY) as actual_delivery_time,  -- Đã hoàn thành giao hàng 1 ngày trước
    0 as late_delivery_risk,
    @vehicle_id as vehicle_id,
    @driver_id as driver_id,
    1 as delivery_attempts,
    'Test delivery cho chức năng hóa đơn điện tử' as delivery_notes,
    NOW() as created_at,
    NOW() as updated_at
WHERE @existing_delivery IS NULL;

-- Nếu đã có delivery, cập nhật để có actual_delivery_time
UPDATE deliveries 
SET actual_delivery_time = DATE_SUB(NOW(), INTERVAL 1 DAY),
    delivery_fee = 50000,
    updated_at = NOW()
WHERE order_id = 1 AND actual_delivery_time IS NULL;

-- ==================================================================================
-- BƯỚC 5: Tạo thêm 1 order hoàn chỉnh khác để test
-- ==================================================================================

-- Tạo order thứ 2 với đầy đủ thông tin
INSERT INTO orders (
    status_id,
    description,
    total_amount,
    benefit_per_order,
    order_profit_per_order,
    notes,
    created_by
) VALUES (
    @status_completed,
    'Đơn hàng test số 2 cho chức năng hóa đơn điện tử',
    750000,
    50000,
    25000,
    'Đơn hàng test với đầy đủ thông tin',
    @admin_id
) ON DUPLICATE KEY UPDATE
    status_id = @status_completed,
    total_amount = 750000;

-- Lấy ID của order vừa tạo
SET @order_2_id = LAST_INSERT_ID();

-- Tạo delivery cho order thứ 2  
INSERT INTO deliveries (
    order_id,
    delivery_fee,
    transport_mode,
    service_type,
    order_date,
    pickup_date,
    schedule_delivery_time,
    actual_delivery_time,
    late_delivery_risk,
    vehicle_id,
    driver_id,
    delivery_attempts,
    delivery_notes
) VALUES (
    @order_2_id,
    60000,
    'ROAD',
    'EXPRESS',
    DATE_SUB(NOW(), INTERVAL 4 DAY),
    DATE_SUB(NOW(), INTERVAL 3 DAY),
    DATE_SUB(NOW(), INTERVAL 2 DAY),
    DATE_SUB(NOW(), INTERVAL 2 DAY),  -- Hoàn thành 2 ngày trước
    0,
    @vehicle_id,
    @driver_id,
    1,
    'Test delivery cho order 2'
) ON DUPLICATE KEY UPDATE
    actual_delivery_time = DATE_SUB(NOW(), INTERVAL 2 DAY);

-- ==================================================================================
-- BƯỚC 6: Kiểm tra kết quả sau khi setup
-- ==================================================================================

SELECT 'KIỂM TRA KẾT QUẢ SETUP' as info;

SELECT 
    o.id as order_id,
    o.status_id,
    s.name as order_status,
    o.total_amount,
    d.id as delivery_id,
    d.actual_delivery_time,
    CASE 
        WHEN d.actual_delivery_time IS NOT NULL THEN 'ĐÃ HOÀN THÀNH'
        ELSE 'CHƯA HOÀN THÀNH'
    END as delivery_status,
    DATEDIFF(NOW(), d.actual_delivery_time) as days_since_delivery,
    CASE 
        WHEN d.actual_delivery_time IS NOT NULL AND DATEDIFF(NOW(), d.actual_delivery_time) <= 5 THEN 'CÓ THỂ TẠO HÓA ĐƠN'
        ELSE 'KHÔNG THỂ TẠO HÓA ĐƠN'
    END as invoice_eligible
FROM orders o
LEFT JOIN status s ON o.status_id = s.id
LEFT JOIN deliveries d ON o.id = d.order_id
WHERE o.id IN (1, @order_2_id)
ORDER BY o.id;

-- ==================================================================================
-- BƯỚC 7: Thông tin để test API
-- ==================================================================================

SELECT 'THÔNG TIN TEST API' as info;
SELECT CONCAT('Order ID để test tạo hóa đơn: ', GROUP_CONCAT(o.id SEPARATOR ', ')) as test_orders
FROM orders o
LEFT JOIN deliveries d ON o.id = d.order_id
LEFT JOIN status s ON o.status_id = s.id
WHERE d.actual_delivery_time IS NOT NULL
  AND s.name IN ('PROCESSED', 'COMPLETED')
  AND DATEDIFF(NOW(), d.actual_delivery_time) <= 5;

-- ==================================================================================
-- HẾT SCRIPT
-- ==================================================================================

