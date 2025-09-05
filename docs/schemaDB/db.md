# 📊 HỆ THỐNG QUẢN LÝ GIAO HÀNG VÀ LOGISTICS - KTC PROJECT 2025

## 📋 TỔNG QUAN HỆ THỐNG

Hệ thống KTC Logistics 2025 sử dụng cơ sở dữ liệu toàn diện quản lý quy trình từ đặt hàng đến giao hàng, với **18 bảng chính** được tối ưu cho hiệu suất cao và khả năng mở rộng.

### 🎯 **Mục tiêu chính:**
- Quản lý đơn hàng giao nhận
- Theo dõi phương tiện và tài xế
- Tối ưu hóa tuyến đường
- Giám sát theo thời gian thực
- Báo cáo và phân tích hiệu suất

### 📝 **Thông tin kỹ thuật:**
- **Database Engine**: MySQL 8.0+
- **Character Set**: UTF8MB4
- **Schema File**: `database-8-4.sql`

---

## 🏗️ CẤU TRÚC CƠ SỞ DỮ LIỆU & MỤC ĐÍCH BẢNG

### **1. Bảng Trạng Thái Hệ Thống (`status`)**
**Mục đích**: Quản lý tập trung tất cả các trạng thái có thể có trong hệ thống.
**Cột chính**:
- `id`: Mã định danh trạng thái duy nhất
- `type`: Phân loại trạng thái (phương tiện, đơn hàng, thanh toán, người dùng)
- `name`: Tên trạng thái dễ đọc
- `description`: Mô tả chi tiết ý nghĩa của trạng thái

### **2. Bảng Vai Trò Người Dùng (`roles`)**
**Mục đích**: Định nghĩa vai trò và phân quyền trong hệ thống.
**Cột chính**:
- `id`: Mã định danh duy nhất của vai trò
- `role_name`: Tên vai trò (admin, điều phối, tài xế, xem)
- `permission`: Đối tượng JSON chứa quyền và quyền truy cập của vai trò
- `description`: Mô tả chi tiết trách nhiệm của vai trò

### **3. Bảng Người Dùng (`users`)**
**Mục đích**: Quản lý thông tin tất cả người dùng hệ thống.
**Cột chính**:
- `id`: Mã định danh duy nhất của người dùng
- `username`: Tên đăng nhập duy nhất
- `email`: Địa chỉ email để đăng nhập và nhận thông báo
- `password`: Mật khẩu đã mã hóa để xác thực
- `full_name`: Họ tên đầy đủ để hiển thị
- `phone`: Số điện thoại liên hệ
- `role_id`: Vai trò người dùng (liên kết với bảng roles)
- `status_id`: Trạng thái tài khoản (liên kết với bảng status)

### **4. Bảng Danh Mục Sản Phẩm (`categories`)**
**Mục đích**: Phân loại sản phẩm theo cấu trúc cây.
**Cột chính**:
- `id`: Mã định danh duy nhất của danh mục
- `name`: Tên hiển thị của danh mục
- `description`: Mô tả chi tiết về danh mục
- `parent_id`: ID danh mục cha (cho cấu trúc cây, liên kết với chính bảng categories)

### **5. Bảng Kho Bãi (`warehouses`)**
**Mục đích**: Quản lý thông tin các kho bãi trong hệ thống logistics.
**Cột chính**:
- `id`: Mã định danh duy nhất của kho bãi
- `name`: Tên hiển thị của kho bãi
- `address`: Địa chỉ đầy đủ của kho bãi
- `latitude`, `longitude`: Tọa độ vĩ độ, kinh độ kho bãi
- `capacity_m3`: Sức chứa tối đa của kho (m3)

### **6. Bảng Sản Phẩm (`products`)**
**Mục đích**: Quản lý thông tin chi tiết về các sản phẩm vận chuyển.
**Cột chính**:
- `id`: Mã định danh duy nhất của sản phẩm
- `name`: Tên hiển thị sản phẩm
- `description`: Mô tả chi tiết sản phẩm
- `category_id`: Phân loại danh mục sản phẩm (liên kết với bảng categories)
- `unit_price`: Giá bán trên một đơn vị
- `weight`: Trọng lượng sản phẩm (kg)
- `volume`: Thể tích sản phẩm (m3)
- `is_fragile`: Cờ hàng dễ vỡ
- `stock_quantity`: Số lượng tồn kho hiện tại
- `warehouse_id`: Kho chính chứa sản phẩm (liên kết với bảng warehouses)

### **7. Bảng Cửa Hàng/Điểm Bán (`stores`)**
**Mục đích**: Quản lý thông tin về các cửa hàng hoặc điểm bán liên quan đến giao hàng.
**Cột chính**:
- `id`: Mã định danh duy nhất của cửa hàng
- Các thông tin về địa chỉ, liên hệ, và tọa độ của cửa hàng

### **8. Bảng Phương Tiện Vận Chuyển (`vehicles`)**
**Mục đích**: Quản lý đội xe vận chuyển.
**Cột chính**:
- `id`: Mã định danh duy nhất của phương tiện
- `license_plate`: Biển số xe (phải tuân theo định dạng)
- Các thông tin về loại phương tiện, tải trọng, trạng thái, và lịch bảo dưỡng

### **9. Bảng Tuyến Đường (`routes`)**
**Mục đích**: Quản lý các tuyến đường vận chuyển được tối ưu hóa.
**Cột chính**:
- `id`: Mã định danh duy nhất của tuyến đường
- Các thông tin về điểm đầu, điểm cuối, quãng đường, thời gian ước tính

### **10. Bảng Đơn Hàng (`orders`)**
**Mục đích**: Quản lý thông tin chính về các đơn hàng cần giao.
**Cột chính**:
- `id`: Mã định danh duy nhất của đơn hàng
- Các thông tin về khách hàng, trạng thái, thanh toán, và thời gian
- `created_by`: Liên kết với người dùng tạo đơn hàng (bảng users)

### **11. Bảng Chi Tiết Đơn Hàng (`order_items`)**
**Mục đích**: Quản lý chi tiết các mặt hàng trong mỗi đơn hàng.
**Cột chính**:
- `id`: Mã định danh duy nhất của mục hàng
- `order_id`: Liên kết với đơn hàng (bảng orders)
- `product_id`: Liên kết với sản phẩm (bảng products)
- `quantity`: Số lượng sản phẩm
- `unit_price`: Giá bán tại thời điểm đặt hàng
- Có ràng buộc unique key đảm bảo một sản phẩm chỉ xuất hiện một lần trong đơn hàng

### **12. Bảng Địa Chỉ (`addresses`)**
**Mục đích**: Quản lý địa chỉ giao hàng và lấy hàng cho các đơn hàng.
**Cột chính**:
- `id`: Mã định danh duy nhất của địa chỉ
- `order_id`: Liên kết với đơn hàng (bảng orders)
- `address_type`: Loại địa chỉ (giao hàng hoặc lấy hàng)
- Các thông tin chi tiết về địa chỉ và tọa độ GPS

### **13. Bảng Thanh Toán (`payments`)**
**Mục đích**: Quản lý thông tin thanh toán cho các đơn hàng.
**Cột chính**:
- `id`: Mã định danh duy nhất của thanh toán
- `order_id`: Liên kết với đơn hàng (bảng orders)
- `transaction_id`: Mã giao dịch thanh toán (duy nhất)
- `amount`: Số tiền thanh toán
- `payment_method`: Phương thức thanh toán
- `status_id`: Trạng thái thanh toán (liên kết với bảng status)

### **14. Bảng Giao Hàng (`deliveries`)**
**Mục đích**: Quản lý quá trình giao hàng từ khi nhận đến khi hoàn thành.
**Cột chính**:
- `id`: Mã định danh duy nhất của giao hàng
- `order_id`: Liên kết với đơn hàng (bảng orders)
- `driver_id`: Tài xế thực hiện giao hàng (liên kết với bảng users)
- `vehicle_id`: Phương tiện sử dụng (liên kết với bảng vehicles)
- `route_id`: Tuyến đường đã chọn (liên kết với bảng routes)
- `pickup_date`: Thời gian lấy hàng
- `actual_delivery_time`: Thời gian giao hàng thực tế
- Có ràng buộc kiểm tra đảm bảo thời gian giao hàng thực tế không sớm hơn thời gian lấy hàng

### **15. Bảng Theo Dõi GPS (`delivery_tracking`)**
**Mục đích**: Lưu trữ dữ liệu theo dõi GPS của các đơn giao hàng trong thời gian thực.
**Cột chính**:
- `id`: Mã định danh duy nhất của điểm theo dõi
- `delivery_id`: Liên kết với giao hàng (bảng deliveries)
- `latitude`, `longitude`: Tọa độ vĩ độ, kinh độ tại thời điểm theo dõi
- `timestamp`: Thời điểm ghi nhận tọa độ
- `status_id`: Trạng thái tại thời điểm theo dõi (liên kết với bảng status)

### **16. Bảng Chứng Từ Giao Hàng (`delivery_proofs`)**
**Mục đích**: Lưu trữ bằng chứng xác nhận đã giao hàng.
**Cột chính**:
- `id`: Mã định danh duy nhất của bằng chứng giao hàng
- `delivery_id`: Liên kết với giao hàng (bảng deliveries)
- `proof_type`: Loại bằng chứng (hình ảnh, chữ ký, mã QR)
- `proof_data`: Dữ liệu bằng chứng
- `uploaded_by`: Người tải lên bằng chứng (liên kết với bảng users)
- `captured_at`: Thời điểm chụp/tạo bằng chứng

### **17. Bảng Giao Dịch Kho (`warehouse_transactions`)**
**Mục đích**: Theo dõi tất cả các giao dịch nhập xuất kho của sản phẩm.
**Cột chính**:
- `id`: Mã định danh duy nhất của giao dịch kho bãi
- `product_id`: Liên kết với sản phẩm (bảng products)
- `warehouse_id`: Liên kết với kho bãi (bảng warehouses)
- `transaction_type`: Loại giao dịch (nhập, xuất)
- `quantity`: Số lượng sản phẩm
- `unit_cost`: Chi phí trên một đơn vị (với ràng buộc phải >= 0)
- `order_id`: Liên kết với đơn hàng nếu có (bảng orders)

### **18. Bảng Nhật Ký Hoạt Động (`activity_logs`)**
**Mục đích**: Ghi lại tất cả các hoạt động trong hệ thống để kiểm toán và theo dõi.
**Cột chính**:
- `id`: Mã định danh duy nhất của nhật ký hoạt động
- `actor_id`: Người thực hiện hành động (liên kết với bảng users)
- `action_type`: Loại hành động (thêm, sửa, xóa)
- `table_name`: Tên bảng bị ảnh hưởng
- `record_id`: ID bản ghi bị ảnh hưởng
- `action_timestamp`: Thời điểm hành động
- `old_data`, `new_data`: Dữ liệu trước và sau khi thay đổi
- `status_id`: Trạng thái của hành động (liên kết với bảng status)

---

## 🔗 MỐI QUAN HỆ VÀ RÀNG BUỘC GIỮA CÁC BẢNG

### **Quan hệ 1-1:**
- `deliveries` ↔ `orders`: Mỗi đơn giao hàng tương ứng với một đơn hàng duy nhất

### **Quan hệ 1-N:**
- `roles` → `users`: Một vai trò có thể được gán cho nhiều người dùng
- `status` → `users`, `orders`, `payments`, `delivery_tracking`: Một trạng thái có thể áp dụng cho nhiều đối tượng
- `users` → `orders`: Một người dùng có thể tạo nhiều đơn hàng
- `users` → `deliveries`: Một tài xế có thể thực hiện nhiều đơn giao hàng
- `categories` → `products`: Một danh mục có thể chứa nhiều sản phẩm
- `warehouses` → `products`: Một kho có thể chứa nhiều sản phẩm
- `orders` → `order_items`: Một đơn hàng có nhiều mặt hàng
- `products` → `order_items`: Một sản phẩm có thể xuất hiện trong nhiều mục đơn hàng
- `orders` → `addresses`: Một đơn hàng có thể có nhiều địa chỉ (lấy và giao)
- `orders` → `payments`: Một đơn hàng có thể có nhiều giao dịch thanh toán
- `deliveries` → `delivery_tracking`: Một đơn giao hàng có nhiều điểm theo dõi GPS
- `deliveries` → `delivery_proofs`: Một đơn giao hàng có thể có nhiều bằng chứng giao hàng
- `products` → `warehouse_transactions`: Một sản phẩm có nhiều giao dịch kho
- `warehouses` → `warehouse_transactions`: Một kho có nhiều giao dịch

### **Quan hệ tự tham chiếu:**
- `categories` → `categories`: Một danh mục có thể có danh mục cha (cấu trúc cây)

### **Ràng buộc toàn vẹn:**
- **Khóa ngoại**: Tất cả mối quan hệ giữa các bảng đều được thực thi bằng ràng buộc khóa ngoại
- **Ràng buộc duy nhất**: 
  - `uk_order_product`: Đảm bảo một sản phẩm chỉ xuất hiện một lần trong đơn hàng
  - `uk_transaction_id`: Đảm bảo mã giao dịch thanh toán là duy nhất
- **Ràng buộc kiểm tra**:
  - `chk_product_is_fragile`: Đảm bảo cờ hàng dễ vỡ chỉ có giá trị 0 hoặc 1
  - `chk_delivery_time_logic`: Đảm bảo thời gian giao hàng thực tế không sớm hơn thời gian lấy hàng
  - `chk_license_plate_format`: Đảm bảo biển số xe có định dạng hợp lệ
  - `chk_warehouse_trans_unit_cost`: Đảm bảo chi phí đơn vị không âm

---

## 📈 INDEXES & HIỆU SUẤT

Hệ thống sử dụng các indexes sau để tối ưu hiệu suất:

### **Indexes theo tên bảng:**
- **Đơn hàng (orders)**: `idx_orders_status`, `idx_orders_status_created`, `idx_orders_store`, `idx_orders_created_by`, `idx_orders_total_amount`, `idx_orders_created_at`
- **Giao hàng (deliveries)**: `idx_deliveries_vehicle`, `idx_deliveries_driver`, `idx_deliveries_schedule_time`, `idx_deliveries_route`, `idx_deliveries_order`
- **Theo dõi GPS (delivery_tracking)**: `idx_delivery_tracking_delivery`, `idx_delivery_tracking_delivery_time`, `idx_delivery_tracking_status`, `idx_delivery_tracking_timestamp`
- **Sản phẩm (products)**: `idx_products_category`, `idx_products_warehouse`, `idx_products_status`
- **Chi tiết đơn hàng (order_items)**: `idx_order_items_order`, `idx_order_items_product`, `idx_order_items_order_product`
- **Giao dịch kho (warehouse_transactions)**: `idx_warehouse_trans_product`, `idx_warehouse_trans_warehouse`, `idx_warehouse_trans_date`, `idx_warehouse_trans_type`, `idx_warehouse_trans_order`, `idx_warehouse_trans_product_date`

### **Indexes theo mục đích:**
- **Truy vấn theo trạng thái**: Giúp nhanh chóng lọc các đơn hàng, phương tiện, tài xế theo trạng thái
- **Truy vấn theo thời gian**: Hỗ trợ sắp xếp và lọc các hoạt động theo mốc thời gian
- **Truy vấn theo mối quan hệ**: Tăng tốc việc truy vấn các bản ghi liên quan (ví dụ: tất cả đơn hàng của một khách hàng)
- **Truy vấn địa lý**: Hỗ trợ tìm kiếm theo tọa độ GPS cho kho, cửa hàng và theo dõi giao hàng

---

## 📝 CÁC QUY TẮC NGHIỆP VỤ QUAN TRỌNG

1. **Quản lý đơn hàng**:
   - Mỗi đơn hàng phải có ít nhất một mặt hàng
   - Đơn hàng chỉ có thể được giao khi đã thanh toán
   - Trạng thái đơn hàng theo quy trình: Đang xử lý → Đã xác nhận → Đang giao → Đã giao

2. **Quản lý kho**:
   - Mọi thay đổi số lượng sản phẩm phải được ghi lại trong bảng giao dịch kho
   - Sản phẩm không thể có số lượng tồn kho âm

3. **Giao hàng**:
   - Tài xế phải có trạng thái "Sẵn sàng" để được phân công đơn hàng
   - Phương tiện phải có trạng thái "Hoạt động" để được sử dụng cho giao hàng
   - Thời gian giao hàng thực tế phải sau thời gian lấy hàng

4. **Bảo mật**:
   - Mật khẩu người dùng phải được mã hóa khi lưu trữ
   - Mọi thay đổi dữ liệu quan trọng phải được ghi log

5. **Thanh toán**:
   - Mã giao dịch thanh toán phải là duy nhất
   - Một đơn hàng có thể được thanh toán nhiều lần (thanh toán một phần)

---

*© 2025 KTC Logistics - Tài liệu nội bộ*
