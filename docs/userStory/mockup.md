# 📐 Giao Diện Mô Phỏng – Hệ Thống Quản Lý Logistics Thông Minh

Dưới đây là danh sách các giao diện chính theo **vai trò người dùng** trong hệ thống. Giao diện được thiết kế trực quan, dễ thao tác để tối ưu hiệu quả quản lý logistics nội bộ (1PL).

---

## 🔐 1. Màn Hình Đăng Nhập

- 📧 **Trường thông tin**: Email, mật khẩu  
- 🔄 **Phân quyền**: Tự động điều hướng tới dashboard theo vai trò (Dispatcher, Admin...)

---

## 🖥️ 2. Dashboard Theo Vai Trò

### 🧭 **Dispatcher Dashboard**

- ➕ Tạo lệnh vận chuyển mới  
- 🚚 Gán xe & tài xế cho từng lệnh  
- 📋 Danh sách lệnh: chờ – đang vận chuyển – đã hoàn thành  
- 🧮 Bộ lọc: theo ngày, tuyến đường, trạng thái  
- 🗺️ Bản đồ theo dõi trực quan hành trình xe  

---

### 🛠 **Fleet Manager Dashboard**

- 🚛 Danh sách phương tiện (biển số, loại xe, tình trạng)  
- 📆 Quản lý lịch bảo trì định kỳ  
- 🔔 Nhắc nhở tự động khi gần tới hạn bảo trì  
- 📜 Lịch sử vận hành từng phương tiện  

---

### 📱 **Driver Mobile App**

- 📥 Nhận lệnh mới từ Dispatcher  
- 🔄 Cập nhật trạng thái: Đang nhận hàng / Đang giao / Đã giao thành công  
- 🗺️ Xem chi tiết tuyến đường (Google Maps API tích hợp)  
- 📸 Gửi hình ảnh biên nhận, ✍️ chữ ký thanh toán  

---

### 📊 **Operations Manager Dashboard**

- 📈 Tổng quan hiệu suất (hôm nay: giao đúng/trễ...)  
- 🔍 Phân tích dữ liệu: tuyến trễ nhất, tài xế hiệu quả nhất  
- 📊 Biểu đồ thống kê: heatmap, cột, tròn (bar, pie, heatmap)  
- 📤 Xuất báo cáo định kỳ: PDF, Excel  

---

### 👑 **Admin Dashboard**

- 👤 Quản lý người dùng: tạo, sửa, xoá tài khoản  
- 🔐 Phân quyền vai trò: Dispatcher, Driver, Manager...  
- ⚙️ Cấu hình hệ thống: API, IoT, lịch định kỳ  
- 📑 Nhật ký hệ thống: theo dõi hoạt động (logs)

---

## 🧩 3. Giao Diện Hỗ Trợ

- 🪟 **Modal xác nhận** khi xoá hoặc giao lệnh  
- 🔔 **Thông báo real-time** (toast notifications)  
- 📱 **Responsive UI**: Tương thích tablet, mobile  

---
