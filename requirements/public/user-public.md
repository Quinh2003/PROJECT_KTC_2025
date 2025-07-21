# ✅ User Stories - Public Website (NextJS - Người dùng)

## 🧩 User Story #1: Đăng ký tài khoản

- **User Story**: _As a new visitor, I want to register an account so that I can create and track delivery orders._
- **Acceptance Criteria**:
  - Nhập: Họ tên, Email, Mật khẩu, Xác nhận mật khẩu
  - Hiển thị lỗi nếu thiếu hoặc định dạng sai
  - Mật khẩu tối thiểu 8 ký tự
  - Email không được trùng
  - Gửi email xác nhận (nếu có)
- **Priority**: High  
- **Story Points**: 5

---

## 🧩 User Story #2: Đăng nhập

- **User Story**: _As a registered user, I want to log in so that I can manage my shipments._
- **Acceptance Criteria**:
  - Nhập email + mật khẩu
  - Hiển thị lỗi nếu sai
  - Lưu token xác thực (JWT)
  - Chuyển hướng về trang chính sau đăng nhập
- **Priority**: High  
- **Story Points**: 3

---

## 🧩 User Story #3: Tạo đơn vận chuyển

- **User Story**: _As a user, I want to create a new delivery order so that I can ship my items easily._
- **Acceptance Criteria**:
  - Nhập đầy đủ thông tin đơn: tên hàng, địa chỉ gửi/nhận, loại hàng
  - Gửi API `/orders`
  - Trả về mã đơn hàng + trạng thái “Chờ xử lý”
- **Priority**: High  
- **Story Points**: 8

---

## 🧩 User Story #4: Tra cứu đơn hàng

- **User Story**: _As a user, I want to track my shipment so that I know where it is._
- **Acceptance Criteria**:
  - Nhập mã đơn hàng
  - Hiển thị: trạng thái đơn (timeline), vị trí hiện tại (nếu có)
  - Gọi API `/orders/{id}`
- **Priority**: Medium  
- **Story Points**: 5

---

## 🧩 User Story #5: Cập nhật thông tin cá nhân

- **User Story**: _As a user, I want to update my profile so that I can manage my personal info._
- **Acceptance Criteria**:
  - Thay đổi tên, email, số điện thoại
  - Kiểm tra định dạng
  - Gọi API PUT `/user`
- **Priority**: Medium  
- **Story Points**: 5

---

## 🧩 User Story #6: Nhận thông báo trạng thái đơn hàng

- **User Story**: _As a user, I want to get real-time updates about my orders so that I don’t miss any changes._
- **Acceptance Criteria**:
  - Khi đơn đổi trạng thái → hiện toast notification
  - Có thể xem danh sách lịch sử thông báo
  - Nhận email (nếu bật)
- **Priority**: Medium  
- **Story Points**: 5

---

## 🧩 User Story #7: Hỗ trợ trực tuyến (Live Chat)

- **User Story**: _As a user, I want to chat with support so that I can get help when I need it._
- **Acceptance Criteria**:
  - Giao diện chat hoặc form liên hệ
  - Nếu không có nhân viên: “Chúng tôi sẽ phản hồi qua email”
  - Nhận phản hồi qua email hoặc giao diện
- **Priority**: Medium  
- **Story Points**: 8
