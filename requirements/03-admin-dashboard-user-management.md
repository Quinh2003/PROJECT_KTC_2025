# 👨‍💼 **Admin – Dashboard & Quản lý người dùng**

---

## 📊 Phần 1: Dashboard Admin

---

### ✅ US-ADMIN-DASH-01: System Overview

- **As an Admin**  
  I want to see system overview dashboard  
  So that I can monitor overall performance  
- **🎯 Priority:** High  
- **🧮 Story Points:** 5  

#### 🎯 Tiêu chí chấp nhận:

- Hiển thị tổng số đơn hàng, tài xế, khách hàng  
- Biểu đồ hiệu suất hệ thống theo thời gian  
- Danh sách Top 5 tài xế xuất sắc nhất  
- Cảnh báo hệ thống (nếu có) như downtime, lỗi kết nối, số liệu bất thường

---

### ✅ US-ADMIN-DASH-02: Quick Actions

- **As an Admin**  
  I want to access quick action buttons  
  So that I can perform common administrative tasks faster  
- **🎯 Priority:** Medium  
- **🧮 Story Points:** 2  

#### 🎯 Tiêu chí chấp nhận:

- Nút "Thêm tài xế" để mở nhanh form thêm mới  
- Nút "Xem báo cáo hôm nay" chuyển hướng đến báo cáo ngày  
- Shortcut đến các trang quản trị: Người dùng, Tài xế, Điều phối viên, Cài đặt hệ thống  
- Không bao gồm các thao tác nghiệp vụ như tạo đơn hàng

---

### ✅ US-ADMIN-DASH-03: Manage Dispatchers

- **As an Admin**  
  I want to manage dispatchers in the system  
  So that I can assign, update, or deactivate their accounts  
- **🎯 Priority:** High  
- **🧮 Story Points:** 3  

#### 🎯 Tiêu chí chấp nhận:

- Danh sách tất cả điều phối viên hiện có trong hệ thống  
- Thêm điều phối viên mới: họ tên, email, số điện thoại, phân quyền  
- Cập nhật thông tin điều phối viên (liên hệ, trạng thái, quyền hạn)  
- Kích hoạt / vô hiệu hóa tài khoản  
- Tìm kiếm và lọc theo tên, email, trạng thái hoạt động

---

### ✅ US-ADMIN-DASH-04: User Role Management

- **As an Admin**  
  I want to manage roles and permissions  
  So that I can control who has access to what in the system  
- **🎯 Priority:** High  
- **🧮 Story Points:** 4  

#### 🎯 Tiêu chí chấp nhận:

- Xem danh sách vai trò (Admin, Điều phối viên, Tài xế, Nhân viên, Khách hàng)  
- Giao diện phân quyền cho từng vai trò: Xem / Tạo / Sửa / Xóa  
- Giao quyền hoặc thu hồi quyền của người dùng cụ thể  
- Ghi log lại các thay đổi quyền hạn

---

## 👤 Phần 2: Quản lý Người dùng

---

### ✅ US-ADMIN-USER-01: Create New User

- **As an Admin**  
  I want to create new user accounts  
  So that new employees can access the system  
- **🎯 Priority:** High  
- **🧮 Story Points:** 3  

#### 🎯 Tiêu chí chấp nhận:

- Form nhập thông tin: Họ tên, email, mật khẩu, vai trò  
- Kiểm tra email không được trùng với tài khoản đã tồn tại  
- Mật khẩu được tự động mã hóa khi lưu vào hệ thống  
- Gửi email thông báo khi tạo tài khoản thành công

---

### ✅ US-ADMIN-USER-02: User List & Management

- **As an Admin**  
  I want to view and manage all users  
  So that I can control system access  
- **🎯 Priority:** High  
- **🧮 Story Points:** 4  

#### 🎯 Tiêu chí chấp nhận:

- Danh sách người dùng với khả năng lọc theo vai trò (role)  
- Tìm kiếm theo tên hoặc email  
- Chỉnh sửa thông tin người dùng trực tiếp (inline edit)  
- Kích hoạt / vô hiệu hóa tài khoản người dùng  
- Phân trang danh sách nếu quá dài

---

### ✅ US-ADMIN-USER-03: Role Assignment

- **As an Admin**  
  I want to assign and change user roles  
  So that users have appropriate system permissions  
- **🎯 Priority:** High  
- **🧮 Story Points:** 2  

#### 🎯 Tiêu chí chấp nhận:

- Dropdown để chọn vai trò (Admin, Dispatcher, Driver, Customer)  
- Hiển thị ma trận phân quyền rõ ràng tương ứng với từng vai trò  
- Hiển thị hộp thoại xác nhận nếu thay đổi vai trò ảnh hưởng lớn  
- Ghi lại lịch sử thay đổi vai trò (role change log)
