# Chi tiết bảng Database - KTC Logistics System

## Tổng quan
Tài liệu này mô tả chi tiết cấu trúc của 16 bảng trong hệ thống quản lý logistics và giao hàng KTC Project 2025.

---

## 1. VEHICLES - Quản lý phương tiện vận chuyển

### Mô tả
Bảng quản lý thông tin phương tiện vận chuyển như xe tải, xe van, xe máy với thông tin sức chứa và trạng thái.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của phương tiện |
| `license_plate` | VARCHAR(20) | NOT NULL, UNIQUE | Biển số xe |
| `vehicle_type` | VARCHAR(50) | NOT NULL, DEFAULT 'TRUCK' | Loại phương tiện (xe tải, xe van, xe máy, ô tô) |
| `capacity_weight_kg` | DECIMAL(10,2) | DEFAULT 0.00 | Trọng tải tối đa của phương tiện (kg) |
| `capacity_volume_m3` | DECIMAL(10,2) | DEFAULT 0.00 | Thể tích chứa hàng tối đa (m3) |
| `status_id` | TINYINT UNSIGNED | NOT NULL | Trạng thái xe (hoạt động, bảo trì, ngừng hoạt động) |
| `current_driver_id` | BIGINT | NULLABLE | ID tài xế hiện tại, NULL nếu chưa phân công |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo bản ghi |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật cuối cùng |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về phương tiện |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `status_id` → `status(id)`
  - `current_driver_id` → `users(id)`
- **Unique Constraints**: `license_plate`
- **Check Constraints**: `capacity_weight_kg >= 0 AND capacity_volume_m3 >= 0`

### Indexes
- `idx_vehicles_status` - Tìm phương tiện theo trạng thái
- `idx_vehicles_driver` - Tìm phương tiện theo tài xế hiện tại
- `idx_vehicles_type` - Tìm phương tiện theo loại
- `idx_vehicles_license` - Tìm phương tiện theo biển số

---

## 2. ORDER_ITEMS - Chi tiết các mặt hàng trong đơn hàng

### Mô tả
Bảng chi tiết các sản phẩm trong từng đơn hàng với số lượng và giá tiền.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của mục hàng |
| `order_id` | BIGINT | NOT NULL | Mã đơn hàng chứa mục hàng này |
| `product_id` | BIGINT | NOT NULL | Mã sản phẩm trong mục hàng |
| `quantity` | INT | NOT NULL | Số lượng sản phẩm đặt mua |
| `unit_price` | DECIMAL(15,2) | NOT NULL | Đơn giá sản phẩm |
| `shipping_fee` | DECIMAL(15,2) | DEFAULT 0.00 | Phí vận chuyển mà người dùng phải trả |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo mục hàng |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật mục hàng cuối cùng |
| `notes` | TEXT | NULLABLE | Ghi chú đặc biệt cho mục hàng này |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `order_id` → `orders(id)`
  - `product_id` → `products(id)`
- **Check Constraints**: `quantity > 0`

### Indexes
- `idx_order_items_order` - Tìm items theo đơn hàng
- `idx_order_items_product` - Tìm items theo sản phẩm
- `idx_order_items_order_product` - Composite index cho order-product

---

## 3. DELIVERY_PROOFS - Chứng chỉ giao hàng thành công

### Mô tả
Bảng lưu trữ bằng chứng giao hàng như ảnh, chữ ký, ghi âm để xác nhận giao hàng thành công.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của bằng chứng giao hàng |
| `proof_type` | VARCHAR(50) | NOT NULL, DEFAULT 'PHOTO' | Loại bằng chứng (ảnh, chữ ký, ghi âm) |
| `file_path` | VARCHAR(500) | NULLABLE | Đường dẫn lưu trữ file bằng chứng |
| `file_name` | VARCHAR(255) | NULLABLE | Tên file bằng chứng gốc |
| `recipient_name` | VARCHAR(255) | NULLABLE | Tên người nhận hàng thực tế |
| `recipient_signature` | TEXT | NULLABLE | Dữ liệu chữ ký số của người nhận |
| `captured_at` | DATETIME | NULLABLE | Thời gian chụp/ghi nhận bằng chứng |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo bản ghi bằng chứng |
| `order_id` | BIGINT | NOT NULL | Mã đơn hàng liên quan đến bằng chứng |
| `uploaded_by` | BIGINT | NULLABLE | ID người dùng (tài xế) upload bằng chứng |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về bằng chứng giao hàng |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `order_id` → `orders(id)`
  - `uploaded_by` → `users(id)`

### Indexes
- `idx_delivery_proofs_order` - Bằng chứng theo đơn hàng
- `idx_delivery_proofs_type` - Bằng chứng theo loại
- `idx_delivery_proofs_uploader` - Bằng chứng theo người upload
- `idx_delivery_proofs_captured` - Bằng chứng theo thời gian chụp

---

## 4. ADDRESSES - Địa chỉ giao hàng và lấy hàng

### Mô tả
Bảng quản lý địa chỉ giao hàng, lấy hàng với thông tin GPS và người liên hệ.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của địa chỉ |
| `order_id` | BIGINT | NOT NULL | Mã đơn hàng liên quan |
| `address_type` | VARCHAR(50) | NOT NULL | Loại địa chỉ (giao hàng, lấy hàng, trả hàng) |
| `address` | VARCHAR(500) | NOT NULL | Địa chỉ đầy đủ (số nhà, đường, phường/xã) |
| `latitude` | DECIMAL(10,8) | NULLABLE | Tọa độ vĩ độ (GPS) |
| `longitude` | DECIMAL(11,8) | NULLABLE | Tọa độ kinh độ (GPS) |
| `city` | VARCHAR(100) | NULLABLE | Thành phố/Tỉnh |
| `state` | VARCHAR(100) | NULLABLE | Bang/Khu vực |
| `country` | VARCHAR(100) | DEFAULT 'Vietnam' | Quốc gia |
| `region` | VARCHAR(100) | NULLABLE | Vùng miền/Khu vực |
| `postal_code` | VARCHAR(20) | NULLABLE | Mã bưu điện |
| `contact_name` | VARCHAR(255) | NULLABLE | Tên người liên hệ tại địa chỉ |
| `contact_phone` | VARCHAR(20) | NULLABLE | Số điện thoại người liên hệ |
| `contact_email` | VARCHAR(255) | NULLABLE | Email người liên hệ |
| `floor_number` | VARCHAR(10) | NULLABLE | Số tầng/lầu của tòa nhà |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo địa chỉ |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật địa chỉ cuối cùng |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: `order_id` → `orders(id)`

### Indexes
- `idx_addresses_order` - Địa chỉ theo đơn hàng
- `idx_addresses_type` - Địa chỉ theo loại
- `idx_addresses_city` - Địa chỉ theo thành phố
- `idx_addresses_coordinates` - Tìm kiếm theo tọa độ GPS

---

## 5. PRODUCTS - Quản lý sản phẩm

### Mô tả
Bảng catalog sản phẩm với thông tin chi tiết về giá, trọng lượng, thể tích và tồn kho.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của sản phẩm |
| `product_card_id` | VARCHAR(50) | NOT NULL, UNIQUE | Mã thẻ sản phẩm cho catalog |
| `product_code` | VARCHAR(50) | NOT NULL, UNIQUE | Mã SKU/mã nội bộ sản phẩm |
| `name` | VARCHAR(255) | NOT NULL | Tên hiển thị sản phẩm |
| `description` | TEXT | NULLABLE | Mô tả chi tiết sản phẩm |
| `category_id` | BIGINT | NOT NULL | Phân loại danh mục sản phẩm |
| `unit_price` | DECIMAL(15,2) | NOT NULL | Giá bán trên một đơn vị |
| `weight` | DECIMAL(10,3) | DEFAULT 0.000 | Trọng lượng sản phẩm (kg) |
| `volume` | DECIMAL(10,3) | DEFAULT 0.000 | Thể tích sản phẩm (m3) |
| `is_fragile` | TINYINT | NOT NULL, DEFAULT 0 | Cờ hàng dễ vỡ: 0=Không, 1=Có |
| `stock_quantity` | INT | NOT NULL, DEFAULT 0 | Số lượng tồn kho hiện tại |
| `product_image` | VARCHAR(500) | NULLABLE | URL/đường dẫn ảnh sản phẩm |
| `product_status` | TINYINT | NOT NULL, DEFAULT 1 | Trạng thái sản phẩm: 0=Ngừng bán, 1=Đang bán |
| `warehouse_id` | BIGINT | NULLABLE | Kho chính chứa sản phẩm này |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo sản phẩm |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật sản phẩm cuối cùng |
| `created_by` | BIGINT | NULLABLE | ID người dùng tạo sản phẩm này |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về sản phẩm |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `category_id` → `categories(id)`
  - `warehouse_id` → `warehouses(id)`
  - `created_by` → `users(id)`
- **Unique Constraints**: `product_card_id`, `product_code`
- **Check Constraints**: `unit_price >= 0`, `weight >= 0 AND volume >= 0`

### Indexes
- `idx_products_category` - Tìm sản phẩm theo danh mục
- `idx_products_warehouse` - Tìm sản phẩm theo kho
- `idx_products_status` - Tìm sản phẩm theo trạng thái
- `idx_products_code` - Tìm sản phẩm theo mã SKU
- `idx_products_card_id` - Tìm sản phẩm theo mã thẻ

---

## 6. CATEGORIES - Danh mục sản phẩm

### Mô tả
Bảng phân loại sản phẩm theo cấu trúc cây với danh mục cha-con.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của danh mục |
| `category_id` | VARCHAR(50) | NOT NULL, UNIQUE | Mã danh mục nghiệp vụ (dễ đọc) |
| `name` | VARCHAR(255) | NOT NULL | Tên hiển thị của danh mục |
| `description` | TEXT | NULLABLE | Mô tả chi tiết về danh mục |
| `parent_id` | BIGINT | NULLABLE | ID danh mục cha (cho cấu trúc cây) |
| `is_active` | TINYINT | NOT NULL, DEFAULT 1 | Trạng thái sử dụng: 0=Ngừng, 1=Hoạt động |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo danh mục |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật danh mục cuối cùng |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về danh mục |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: `parent_id` → `categories(id)` (Self-reference)
- **Unique Constraints**: `category_id`

### Indexes
- `idx_categories_parent` - Danh mục theo danh mục cha
- `idx_categories_active` - Danh mục đang hoạt động
- `idx_categories_category_id` - Tìm theo mã danh mục

---

## 7. STORES - Quản lý cửa hàng/điểm bán hàng

### Mô tả
Bảng quản lý các cửa hàng và điểm bán hàng với thông tin liên hệ và vị trí GPS.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của cửa hàng |
| `store_code` | VARCHAR(50) | NOT NULL, UNIQUE | Mã cửa hàng nghiệp vụ (dễ đọc) |
| `store_name` | VARCHAR(255) | NOT NULL | Tên hiển thị của cửa hàng |
| `email` | VARCHAR(255) | NULLABLE | Email liên hệ của cửa hàng |
| `phone` | VARCHAR(20) | NOT NULL | Số điện thoại liên hệ cửa hàng |
| `address` | TEXT | NOT NULL | Địa chỉ đầy đủ của cửa hàng |
| `latitude` | DECIMAL(10,8) | NULLABLE | Tọa độ vĩ độ cửa hàng |
| `longitude` | DECIMAL(11,8) | NULLABLE | Tọa độ kinh độ cửa hàng |
| `is_active` | TINYINT | NOT NULL, DEFAULT 1 | Trạng thái hoạt động: 0=Đóng cửa, 1=Hoạt động |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo cửa hàng |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật cửa hàng cuối cùng |
| `created_by` | BIGINT | NULLABLE | ID người dùng tạo cửa hàng này |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về cửa hàng |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: `created_by` → `users(id)`
- **Unique Constraints**: `store_code`

### Indexes
- `idx_stores_active` - Cửa hàng đang hoạt động
- `idx_stores_code` - Tìm cửa hàng theo mã
- `idx_stores_coordinates` - Tìm cửa hàng theo tọa độ

---

## 8. ROUTES - Quản lý tuyến đường vận chuyển

### Mô tả
Bảng quản lý các tuyến đường tối ưu cho giao hàng với waypoints và tính toán chi phí.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của tuyến đường |
| `name` | VARCHAR(255) | NOT NULL | Tên tuyến đường (mô tả ngắn) |
| `waypoints` | JSON | NOT NULL | Danh sách các điểm dừng trên tuyến (JSON) |
| `estimated_distance_km` | DECIMAL(10,2) | DEFAULT 0.00 | Khoảng cách dự kiến (km) |
| `estimated_duration_minutes` | INT | DEFAULT 0 | Thời gian dự kiến (phút) |
| `estimated_cost` | DECIMAL(15,2) | DEFAULT 0.00 | Chi phí dự kiến cho tuyến |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo tuyến đường |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật tuyến cuối cùng |
| `completed_at` | DATETIME | NULLABLE | Thời gian hoàn thành tuyến đường |
| `created_by` | BIGINT | NULLABLE | ID người dùng tạo tuyến đường |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về tuyến đường |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: `created_by` → `users(id)`

### Indexes
- `idx_routes_created_by` - Tuyến đường theo người tạo
- `idx_routes_completed` - Tuyến đường đã hoàn thành
- `idx_routes_estimated_cost` - Sắp xếp theo chi phí dự kiến

---

## 9. USERS - Quản lý người dùng hệ thống

### Mô tả
Bảng quản lý tất cả người dùng hệ thống bao gồm tài xế, điều phối, quản trị viên.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của người dùng |
| `username` | VARCHAR(100) | NOT NULL, UNIQUE | Tên đăng nhập duy nhất |
| `email` | VARCHAR(255) | NOT NULL, UNIQUE | Địa chỉ email để đăng nhập và nhận thông báo |
| `password` | VARCHAR(255) | NOT NULL | Mật khẩu đã mã hóa để xác thực |
| `full_name` | VARCHAR(255) | NULLABLE | Họ tên đầy đủ để hiển thị |
| `phone` | VARCHAR(20) | NULLABLE | Số điện thoại liên hệ |
| `role_id` | BIGINT | NOT NULL | Vai trò người dùng (admin, điều phối, tài xế, xem) |
| `status_id` | TINYINT UNSIGNED | NULLABLE | Trạng thái tài khoản (hoạt động, ngừng hoạt động, tạm khóa) |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo tài khoản |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật tài khoản cuối cùng |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về người dùng |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `role_id` → `roles(id)`
  - `status_id` → `status(id)`
- **Unique Constraints**: `username`, `email`

### Indexes
- `idx_users_role` - Người dùng theo vai trò
- `idx_users_status` - Người dùng theo trạng thái
- `idx_users_email` - Tìm người dùng theo email
- `idx_users_username` - Tìm người dùng theo username

---

## 10. STATUS - Quản lý trạng thái hệ thống

### Mô tả
Bảng quản lý tất cả trạng thái trong hệ thống cho các đối tượng khác nhau.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | TINYINT UNSIGNED | PRIMARY KEY, AUTO_INCREMENT | Mã định danh trạng thái duy nhất (1-255) |
| `type` | VARCHAR(50) | NOT NULL | Phân loại trạng thái (phương tiện, đơn hàng, thanh toán, người dùng, v.v.) |
| `name` | VARCHAR(100) | NOT NULL | Tên trạng thái dễ đọc |
| `description` | TEXT | NULLABLE | Mô tả chi tiết ý nghĩa của trạng thái |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo trạng thái |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật trạng thái cuối cùng |

### Ràng buộc & Khóa
- **Primary Key**: `id`

### Indexes
- `idx_status_type` - Trạng thái theo loại
- `idx_status_name` - Tìm trạng thái theo tên

---

## 11. WAREHOUSE_TRANSACTIONS - Quản lý giao dịch kho bãi

### Mô tả
Bảng ghi lại tất cả giao dịch nhập/xuất kho với chi tiết số lượng và giá vốn.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của giao dịch kho bãi |
| `product_id` | BIGINT | NOT NULL | Mã sản phẩm tham gia giao dịch |
| `warehouse_id` | BIGINT | NOT NULL | Mã kho hàng thực hiện giao dịch |
| `status_id` | TINYINT UNSIGNED | NOT NULL | Trạng thái giao dịch (thành công, thất bại, chờ xử lý) |
| `transaction_type` | VARCHAR(50) | NOT NULL, DEFAULT 'IN' | Loại giao dịch (nhập kho=IN, xuất kho=OUT, chuyển kho=TRANSFER) |
| `quantity` | INT | NOT NULL | Số lượng sản phẩm trong giao dịch |
| `unit_cost` | DECIMAL(15,2) | DEFAULT 0.00 | Giá vốn trên một đơn vị sản phẩm |
| `transaction_date` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian thực hiện giao dịch kho bãi |
| `order_id` | BIGINT | NULLABLE | Mã đơn hàng liên quan (nếu có) |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo bản ghi giao dịch |
| `created_by` | BIGINT | NULLABLE | ID người dùng tạo giao dịch |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về giao dịch kho bãi |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `product_id` → `products(id)`
  - `warehouse_id` → `warehouses(id)`
  - `status_id` → `status(id)`
  - `order_id` → `orders(id)`
  - `created_by` → `users(id)`

### Indexes
- `idx_warehouse_trans_product` - Giao dịch theo sản phẩm
- `idx_warehouse_trans_warehouse` - Giao dịch theo kho
- `idx_warehouse_trans_date` - Giao dịch theo thời gian
- `idx_warehouse_trans_type` - Giao dịch theo loại
- `idx_warehouse_trans_order` - Giao dịch theo đơn hàng
- `idx_warehouse_trans_product_date` - Lịch sử giao dịch sản phẩm

---

## 12. ACTIVITY_LOGS - Nhật ký hoạt động hệ thống

### Mô tả
Bảng audit trail ghi lại tất cả hoạt động quan trọng trong hệ thống.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của nhật ký hoạt động |
| `actor_id` | BIGINT | NULLABLE | ID người dùng thực hiện hành động |
| `role_id` | BIGINT | NOT NULL | Vai trò của người dùng tại thời điểm thực hiện |
| `status_id` | TINYINT UNSIGNED | NOT NULL | Trạng thái hoàn thành hành động |
| `action_type` | VARCHAR(50) | NOT NULL | Loại hành động (TẠO, CẬP NHẬT, XÓA, ĐĂNG NHẬP, v.v.) |
| `table_name` | VARCHAR(64) | NULLABLE | Bảng cơ sở dữ liệu bị ảnh hưởng bởi hành động |
| `record_id` | BIGINT | NULLABLE | ID của bản ghi bị ảnh hưởng |
| `action_timestamp` | DATETIME | NOT NULL, DEFAULT CURRENT_TIMESTAMP | Thời điểm xảy ra hành động |
| `metadata` | JSON | NULLABLE | Metadata bổ sung (giá trị cũ/mới, IP, v.v.) |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `role_id` → `roles(id)`
  - `status_id` → `status(id)`

### Indexes
- `idx_activity_logs_actor` - Log theo người thực hiện
- `idx_activity_logs_action_time` - Log theo thời gian hành động
- `idx_activity_logs_table` - Log theo bảng bị ảnh hưởng
- `idx_activity_logs_action_type` - Log theo loại hành động
- `idx_activity_logs_record` - Log theo bản ghi cụ thể

---

## 13. ORDERS - Quản lý đơn hàng

### Mô tả
Bảng chính quản lý đơn hàng với thông tin tổng quan và tính toán lợi nhuận.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của đơn hàng |
| `order_id` | VARCHAR(100) | NOT NULL, UNIQUE | Mã đơn hàng nghiệp vụ (dễ đọc) |
| `status_id` | TINYINT UNSIGNED | NOT NULL | Trạng thái đơn hàng (chờ xử lý, đang xử lý, hoàn thành, hủy) |
| `store_id` | BIGINT | NULLABLE | ID cửa hàng liên kết, NULL cho đơn hàng online |
| `description` | TEXT | NULLABLE | Mô tả và chi tiết đơn hàng |
| `total_amount` | DECIMAL(15,2) | DEFAULT 0.00 | Tổng số tiền đơn hàng bao gồm phí |
| `benefit_per_order` | DECIMAL(15,2) | DEFAULT 0.00 | Lợi nhuận/biên lãi dự kiến từ đơn hàng này |
| `order_profit_per_order` | DECIMAL(15,2) | DEFAULT 0.00 | Lợi nhuận được tính toán cho đơn hàng này |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về đơn hàng |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo đơn hàng |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật đơn hàng cuối cùng |
| `created_by` | BIGINT | NULLABLE | ID người dùng tạo đơn hàng này |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `status_id` → `status(id)`
  - `store_id` → `stores(id)`
  - `created_by` → `users(id)`
- **Unique Constraints**: `order_id`

### Indexes
- `idx_orders_status` - Tìm đơn hàng theo trạng thái
- `idx_orders_status_created` - Sắp xếp đơn hàng theo trạng thái và thời gian
- `idx_orders_store` - Tìm đơn hàng theo cửa hàng
- `idx_orders_created_by` - Tìm đơn hàng theo người tạo

---

## 14. PAYMENTS - Quản lý thanh toán

### Mô tả
Bảng quản lý thông tin thanh toán với nhiều phương thức và trạng thái.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của thanh toán |
| `order_id` | BIGINT | NOT NULL | Mã đơn hàng được thanh toán |
| `amount` | DECIMAL(15,2) | NOT NULL | Tổng số tiền thanh toán |
| `payment_method` | VARCHAR(50) | NOT NULL, DEFAULT 'CASH' | Phương thức thanh toán (tiền mặt, thẻ, chuyển khoản) |
| `status_id` | TINYINT UNSIGNED | NOT NULL | Trạng thái thanh toán (thành công, thất bại, chờ xử lý) |
| `transaction_id` | VARCHAR(255) | NULLABLE | Mã giao dịch từ cổng thanh toán |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo bản ghi thanh toán |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật thanh toán cuối cùng |
| `created_by` | BIGINT | NULLABLE | ID người dùng tạo bản ghi thanh toán |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về thanh toán |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `order_id` → `orders(id)`
  - `status_id` → `status(id)`
  - `created_by` → `users(id)`
- **Check Constraints**: `amount > 0`

### Indexes
- `idx_payments_order` - Thanh toán theo đơn hàng
- `idx_payments_status` - Thanh toán theo trạng thái
- `idx_payments_method` - Thanh toán theo phương thức
- `idx_payments_transaction` - Thanh toán theo mã giao dịch
- `idx_payments_created` - Thanh toán theo thời gian tạo

---

## 15. DELIVERIES - Quản lý giao hàng

### Mô tả
Bảng core quản lý giao hàng với phân công phương tiện, tài xế và tracking.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của giao hàng |
| `order_id` | BIGINT | NOT NULL | Đơn hàng liên kết đang được giao |
| `delivery_fee` | DECIMAL(15,2) | DEFAULT 0.00 | Chi phí vận chuyển doanh nghiệp logistic phải trả |
| `transport_mode` | VARCHAR(50) | DEFAULT 'ROAD' | Phương thức vận chuyển (đường bộ, hàng không, đường biển, đường sắt) |
| `service_type` | VARCHAR(50) | DEFAULT 'STANDARD' | Mức dịch vụ (tiêu chuẩn, nhanh, ưu tiên) |
| `order_date` | DATETIME | NOT NULL | Thời điểm đặt hàng |
| `pickup_date` | DATETIME | NULLABLE | Thời điểm lấy hàng |
| `schedule_delivery_time` | DATETIME | NULLABLE | Thời gian giao hàng dự kiến |
| `actual_delivery_time` | DATETIME | NULLABLE | Thời gian hoàn thành giao hàng thực tế |
| `late_delivery_risk` | TINYINT | NOT NULL, DEFAULT 0 | Cờ rủi ro giao hàng trễ: 0=Không, 1=Có |
| `delivery_status` | VARCHAR(50) | DEFAULT 'PENDING' | Trạng thái giao hàng hiện tại |
| `vehicle_id` | BIGINT | NOT NULL | Phương tiện được phân công cho giao hàng này |
| `driver_id` | BIGINT | NULLABLE | Tài xế được phân công cho giao hàng này |
| `tracking_id` | BIGINT | NULLABLE | Bản ghi theo dõi GPS cho giao hàng này |
| `route_id` | BIGINT | NULLABLE | Tuyến đường tối ưu cho giao hàng này |
| `delivery_attempts` | INT | DEFAULT 0 | Số lần thử giao hàng đã thực hiện |
| `delivery_notes` | TEXT | NULLABLE | Hướng dẫn đặc biệt và ghi chú cho giao hàng |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo bản ghi giao hàng |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật bản ghi giao hàng cuối cùng |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `order_id` → `orders(id)`
  - `vehicle_id` → `vehicles(id)`
  - `driver_id` → `users(id)`
  - `tracking_id` → `delivery_tracking(id)`
  - `route_id` → `routes(id)`
- **Check Constraints**: `delivery_attempts >= 0`

### Indexes
- `idx_deliveries_vehicle` - Tìm giao hàng theo phương tiện
- `idx_deliveries_driver` - Tìm giao hàng theo tài xế
- `idx_deliveries_status` - Tìm giao hàng theo trạng thái
- `idx_deliveries_schedule_time` - Sắp xếp theo thời gian giao hàng dự kiến
- `idx_deliveries_route` - Tìm giao hàng theo tuyến đường
- `idx_deliveries_order` - Tìm giao hàng theo đơn hàng
- `idx_deliveries_tracking` - Tìm giao hàng theo tracking

---

## 16. ROLES - Quản lý vai trò người dùng

### Mô tả
Bảng quản lý vai trò và phân quyền với JSON-based permissions.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của vai trò |
| `role_name` | VARCHAR(50) | NOT NULL, UNIQUE | Tên vai trò (admin, điều phối, tài xế, xem) |
| `permission` | JSON | NULLABLE | Đối tượng JSON chứa quyền và quyền truy cập của vai trò |
| `description` | TEXT | NULLABLE | Mô tả chi tiết trách nhiệm của vai trò |
| `is_active` | TINYINT | NOT NULL, DEFAULT 1 | Trạng thái vai trò: 0=Ngừng hoạt động, 1=Hoạt động |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo vai trò |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật vai trò cuối cùng |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Unique Constraints**: `role_name`

### Indexes
- `idx_roles_active` - Vai trò đang hoạt động
- `idx_roles_name` - Tìm vai trò theo tên

---

## 17. WAREHOUSES - Quản lý kho bãi

### Mô tả
Bảng quản lý thông tin kho bãi với sức chứa và vị trí GPS.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của kho bãi |
| `warehouse_code` | VARCHAR(50) | NOT NULL, UNIQUE | Mã kho nghiệp vụ (dễ đọc) |
| `name` | VARCHAR(255) | NOT NULL | Tên hiển thị của kho bãi |
| `address` | TEXT | NOT NULL | Địa chỉ đầy đủ của kho bãi |
| `latitude` | DECIMAL(10,8) | NULLABLE | Tọa độ vĩ độ kho bãi |
| `longitude` | DECIMAL(11,8) | NULLABLE | Tọa độ kinh độ kho bãi |
| `capacity_m3` | DECIMAL(10,2) | DEFAULT 0.00 | Sức chứa tối đa của kho (m3) |
| `is_active` | TINYINT | NOT NULL, DEFAULT 1 | Trạng thái hoạt động: 0=Đóng cửa, 1=Hoạt động |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo kho bãi |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật kho bãi cuối cùng |
| `created_by` | BIGINT | NULLABLE | ID người dùng tạo kho bãi này |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung về kho bãi |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: `created_by` → `users(id)`
- **Unique Constraints**: `warehouse_code`

### Indexes
- `idx_warehouses_active` - Kho đang hoạt động
- `idx_warehouses_code` - Tìm kho theo mã
- `idx_warehouses_coordinates` - Tìm kho theo tọa độ

---

## 18. DELIVERY_TRACKING - Theo dõi GPS giao hàng

### Mô tả
Bảng theo dõi GPS thời gian thực cho phương tiện giao hàng.

### Cấu trúc bảng

| Cột | Kiểu dữ liệu | Constraints | Mô tả |
|-----|--------------|-------------|-------|
| `id` | BIGINT | PRIMARY KEY, AUTO_INCREMENT | Mã định danh duy nhất của điểm theo dõi |
| `vehicle_id` | BIGINT | NOT NULL | Mã phương tiện đang được theo dõi |
| `status_id` | TINYINT UNSIGNED | NOT NULL | Trạng thái giao hàng tại thời điểm này |
| `latitude` | DECIMAL(10,8) | NULLABLE | Tọa độ vĩ độ hiện tại |
| `longitude` | DECIMAL(11,8) | NULLABLE | Tọa độ kinh độ hiện tại |
| `timestamp` | DATETIME | NOT NULL, DEFAULT CURRENT_TIMESTAMP | Thời gian ghi nhận điểm theo dõi |
| `location` | VARCHAR(255) | NULLABLE | Tên địa điểm hiện tại (dễ đọc) |
| `notes` | TEXT | NULLABLE | Ghi chú bổ sung tại điểm theo dõi |
| `created_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP | Thời gian tạo bản ghi theo dõi |
| `updated_at` | DATETIME | DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP | Thời gian cập nhật bản ghi cuối cùng |

### Ràng buộc & Khóa
- **Primary Key**: `id`
- **Foreign Keys**: 
  - `vehicle_id` → `vehicles(id)`
  - `status_id` → `status(id)`

### Indexes
- `idx_delivery_tracking_vehicle` - Tracking theo phương tiện
- `idx_delivery_tracking_vehicle_time` - Tracking theo thời gian mới nhất
- `idx_delivery_tracking_status` - Tracking theo trạng thái
- `idx_delivery_tracking_timestamp` - Sắp xếp tracking theo thời gian

---

## Mối quan hệ tổng quan

### Diagram ERD
```
users ←→ roles
  ↓
vehicles → delivery_tracking
  ↓
deliveries ←→ orders ←→ order_items ←→ products ←→ categories
  ↓           ↓                        ↓
routes    addresses                 warehouses
  ↑           ↓                        ↓
delivery_proofs ←→ payments      warehouse_transactions
                    ↓
                  status
                    ↑
                activity_logs
```

### Khóa ngoại chính
1. **orders** → addresses, order_items, deliveries, payments, delivery_proofs
2. **products** → order_items, warehouse_transactions
3. **users** → vehicles, orders, deliveries, roles, activity_logs
4. **vehicles** → deliveries, delivery_tracking
5. **warehouses** → products, warehouse_transactions

---

## Lưu ý quan trọng

### Performance
- Tất cả bảng đều có indexes phù hợp cho truy vấn thường xuyên
- JSON fields sử dụng cho dữ liệu semi-structured (waypoints, permissions, metadata)
- Partitioning nên được xem xét cho bảng lớn như delivery_tracking

### Security
- Mật khẩu phải được hash trước khi lưu vào bảng users
- Sensitive data có thể cần encryption at rest
- Audit trail đầy đủ qua activity_logs

### Scalability
- Sử dụng BIGINT cho primary keys để hỗ trợ volume lớn
- Connection pooling và prepared statements khuyến nghị
- Read replicas cho reporting queries

---

**Cập nhật cuối**: 2025  
**Phiên bản**: 9.4  
**Tổng số bảng**: 18 bảng