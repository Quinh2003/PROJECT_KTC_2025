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
  - Trả về mã đơn hàng + trạng thái "Chờ xử lý"
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

- **User Story**: _As a user, I want to get real-time updates about my orders so that I don't miss any changes._
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
  - Nếu không có nhân viên: "Chúng tôi sẽ phản hồi qua email"
  - Nhận phản hồi qua email hoặc giao diện
- **Priority**: Medium  
- **Story Points**: 8

---

## 🧩 User Story #8: Theo dõi lộ trình tài xế theo thời gian thực

- **User Story**: _As a logistics manager, I want to track drivers' routes in real-time so that I can monitor their progress and ensure timely deliveries._
- **Acceptance Criteria**:
  - Hiển thị vị trí thời gian thực của tài xế trên bản đồ 3D sử dụng dữ liệu GPS
  - Hiển thị thông tin tài xế (tên, xe, đơn hàng hiện tại)
  - Cập nhật vị trí mỗi 10 giây
  - Cho phép chọn nhiều tài xế để xem cùng lúc
  - Xử lý trường hợp offline một cách khéo léo (hiển thị vị trí cuối cùng đã biết)
- **Priority**: High  
- **Story Points**: 8

---

## 🧩 User Story #9: Thống kê và báo cáo hiệu suất vận hành

- **User Story**: _As a logistics manager, I want to view performance reports so that I can analyze operational efficiency._
- **Acceptance Criteria**:
  - Tạo báo cáo cho các chỉ số: Tỷ lệ giao hàng thành công, Thời gian giao hàng trung bình, Chi phí mỗi lần giao, Tỷ lệ sử dụng xe
  - Cho phép lọc theo khoảng thời gian, xe hoặc tài xế
  - Hiển thị dữ liệu dưới dạng biểu đồ (cột, đường) sử dụng thư viện như Chart.js
  - Xuất báo cáo dưới định dạng PDF hoặc CSV
  - Đảm bảo việc tạo báo cáo hoàn thành trong vòng 5 giây
- **Priority**: Medium  
- **Story Points**: 8

---

## 🧩 User Story #10: Quản lý tài xế

- **User Story**: _As a logistics manager, I want to manage driver information so that I can assign them to vehicles and orders._
- **Acceptance Criteria**:
  - Thêm tài xế với thông tin: Mã tài xế, Tên, Số bằng lái, Thông tin liên hệ
  - Hiển thị danh sách tài xế với bộ lọc (trạng thái có sẵn, xe được giao)
  - Cho phép chỉnh sửa hoặc xóa thông tin tài xế
  - Kiểm tra dữ liệu đầu vào (Mã tài xế duy nhất, thông tin liên hệ hợp lệ)
  - Hiển thị trạng thái sẵn sàng của tài xế theo thời gian thực
- **Priority**: Medium  
- **Story Points**: 5

---

## 🧩 User Story #11: Nhận thông báo hệ thống

- **User Story**: _As a logistics manager, I want to receive notifications for critical events so that I can take immediate action._
- **Acceptance Criteria**:
  - Thông báo cho các sự kiện: Trễ đơn hàng, xe hỏng, mức tồn kho thấp
  - Hỗ trợ thông báo qua email và cảnh báo dashboard
  - Cho phép người dùng tùy chỉnh tùy chọn thông báo (bật/tắt các cảnh báo cụ thể)
  - Đảm bảo thông báo được gửi trong vòng 10 giây sau sự kiện
  - Hiển thị lịch sử thông báo trong dashboard
- **Priority**: Medium  
- **Story Points**: 5
