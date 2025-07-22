# 🖥️ Mô tả Giao Diện Mockup Hệ Thống Logistics

Tài liệu mô tả chi tiết các màn hình giao diện chính của hệ thống quản lý logistics, bao gồm các thành phần UI, chức năng và tương tác người dùng.

---

## 🔐 1. Trang Đăng Nhập

### 🎯 Mục đích:

Cho phép người dùng đăng nhập vào hệ thống theo vai trò đã được cấp.

### 🧩 Thành phần giao diện:

- **Email Input**:
  - Nhập địa chỉ email
  - Placeholder: `example@email.com`
  - Kiểm tra định dạng email hợp lệ
- **Password Input**:
  - Nhập mật khẩu (ẩn ký tự)
  - Placeholder: `••••••••`
- **Checkbox**: "Ghi nhớ đăng nhập"
- **Button**: `Đăng nhập`
- **Link**: `Quên mật khẩu?`
- **Validation**:
  - Nếu thiếu hoặc sai định dạng ➝ hiển thị lỗi rõ ràng

---

## 📝 2. Trang Đăng Ký

### 🎯 Mục đích:

Cho phép **khách hàng đăng ký tài khoản mới** để sử dụng hệ thống, bao gồm khả năng **tạo đơn hàng**.

### 🧩 Thành phần giao diện:

- **Form Đăng ký người dùng**:
  - Họ và tên
  - Email
  - Mật khẩu
  - Xác nhận mật khẩu
- **Dropdown (ẩn/hiện)**: Chọn loại tài khoản (mặc định là "Khách hàng")
- **Button**: `Đăng ký`
- **Validation**:
  - Kiểm tra định dạng email
  - Mật khẩu tối thiểu 6 ký tự, phải trùng nhau
  - Nếu có lỗi ➝ hiển thị thông báo rõ ràng
- **Chuyển hướng**:
  - Sau khi đăng ký thành công ➝ chuyển đến trang đăng nhập hoặc dashboard khách hàng

---

## 📊 3. Dashboard (Trang tổng quan)

### 🎯 Mục đích:

Hiển thị thông tin thống kê, điều hướng các tính năng chính tùy theo vai trò (Quản trị / Điều phối / Khách hàng).

### 🧩 Thành phần:

- **Sidebar điều hướng**:
  - Đơn hàng
  - Phương tiện
  - Kho bãi
  - Báo cáo
  - AI Lộ trình
- **Header**:
  - Tên người dùng + avatar
  - Nút đăng xuất
- **Main content**:
  - Biểu đồ hiệu suất (theo vai trò)
  - Danh sách các hành động gần đây

---

## 📦 4. Quản Lý Đơn Hàng

### 🎯 Mục đích:

Cho phép người dùng (tùy vai trò) tạo và quản lý đơn hàng.

### 🧩 Khác biệt theo vai trò:

- **Khách hàng**:
  - Có thể nhấn `Tạo đơn hàng`
  - Nhập thông tin người nhận, địa chỉ, loại hàng
  - Theo dõi trạng thái đơn hàng
- **Điều phối viên / Quản trị viên**:
  - Có thể cập nhật, xoá, phân công tài xế

### 🧩 Thành phần chung:

- **Bảng danh sách**:
  - Mã đơn, người nhận, trạng thái, ngày tạo
- **Bộ lọc**:
  - Theo trạng thái / thời gian
- **Nút hành động**:
  - ➕ Tạo mới
  - ✏️ Cập nhật
  - 🗑️ Xoá

---

## 🗺️ 5. Bản Đồ 3D Lộ Trình

### 🎯 Mục đích:

Mô phỏng trực quan các tuyến vận chuyển.

### 🧩 Thành phần:

- Bản đồ 3D tương tác
- Markers:
  - Vị trí kho
  - Phương tiện đang di chuyển
- Chế độ xem:
  - Theo từng tài xế
  - Tổng thể hệ thống

---

## 🤖 6. AI Đề Xuất Lộ Trình

### 🎯 Mục đích:

Đưa ra gợi ý tuyến đường tối ưu.

### 🧩 Thành phần:

- Chọn kho xuất phát + điểm đến
- Tùy chọn ưu tiên: thời gian / chi phí
- Hiển thị kết quả:
  - Bản đồ
  - Tuyến gợi ý chi tiết: khoảng cách, thời gian, chi phí ước tính

---

## 📈 7. Thống Kê & Báo Cáo

### 🎯 Mục đích:

Phân tích hiệu suất hoạt động của hệ thống.

### 🧩 Thành phần:

- **Biểu đồ cột**: số đơn theo trạng thái
- **Biểu đồ tròn**: tỉ lệ giao đúng giờ
- **Xuất báo cáo**:
  - 📥 Excel (.xlsx)
  - 📥 PDF

---

✅ **Ghi chú UI/UX**:

- Giao diện phản hồi nhanh, tương thích mobile/tablet
- Màu sắc rõ ràng, thân thiện
- Luồng đăng nhập → dashboard → thao tác đơn hàng mượt mà
