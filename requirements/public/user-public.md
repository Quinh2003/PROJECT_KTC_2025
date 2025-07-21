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

## 🧩 User Story #3: Cập nhật thông tin cá nhân

- **User Story**: _As a user, I want to update my profile so that I can manage my personal info._
- **Acceptance Criteria**:
  - Thay đổi tên, email, số điện thoại
  - Kiểm tra định dạng
  - Gọi API PUT `/user`
- **Priority**: Medium  
- **Story Points**: 5

---

## 🧩 User Story #4: Tạo đơn vận chuyển

- **User Story**: _As a user, I want to create a delivery order and choose prepaid or postpaid payment._
- **Acceptance Criteria**:
  - Nhập thông tin hàng hóa, địa chỉ gửi & nhận
  - Chọn loại dịch vụ: hỏa tốc, tiêu chuẩn, tiết kiệm
  - **Chọn phương thức thanh toán**: Trả trước hoặc Trả sau
  - Tính toán cước phí tự động
  - Gửi API để tạo đơn và nhận mã đơn
- **Priority**: High
- **Story Points**: 8

---

## 🧩 User Story #4: Tra cứu đơn hàng

- **User Story**: _As a user, I want to track my order status via code._
- **Acceptance Criteria**:
  - Nhập mã đơn vận
  - Nhận được trạng thái hiện tại, lịch sử trạng thái
  - Hiển thị thông tin người nhận/người gửi (ẩn 1 phần)
- **Priority**: Medium
- **Story Points**: 3

---

## 🧩 User Story #5: Nhận thông báo trạng thái đơn hàng

- **User Story**: _As a user, I want to get real-time updates about my orders so that I don’t miss any changes._
- **Acceptance Criteria**:
  - Khi đơn đổi trạng thái → hiện toast notification
  - Có thể xem danh sách lịch sử thông báo
  - Nhận email (nếu bật)
- **Priority**: Medium  
- **Story Points**: 5

---

## 🧩 User Story #6: Hỗ trợ trực tuyến

- **User Story**: _As a user, I want to chat with support so that I can get help when I need it._
- **Acceptance Criteria**:
  - Điền thông tin cá nhân và nội dung cần hỗ trợ qua form
  - Nhận phản hồi qua email
- **Priority**: Medium  
- **Story Points**: 8
  
User Story #9: Theo dõi lộ trình tài xế theo thời gian thực

User Story: As a logistics manager, I want to track drivers’ routes in real-time so that I can monitor their progress and ensure timely deliveries.
Acceptance Criteria: