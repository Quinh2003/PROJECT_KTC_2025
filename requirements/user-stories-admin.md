# ✅ User Stories - Admin Dashboard (ReactJS - Logistics Manager)

## 🧩 User Story #1: Theo dõi lộ trình tài xế theo thời gian thực

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

## 🧩 User Story #2: Thống kê và báo cáo hiệu suất vận hành

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

## 🧩 User Story #3: Quản lý tài xế

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

## 🧩 User Story #4: Quản lý đơn hàng

- **User Story**: _As a logistics manager, I want to manage delivery orders so that I can track and update order status._
- **Acceptance Criteria**:
  - Xem danh sách tất cả đơn hàng với bộ lọc (trạng thái, ngày, khách hàng)
  - Cập nhật trạng thái đơn hàng (Chờ xử lý → Đang giao → Hoàn thành)
  - Gán tài xế cho đơn hàng
  - Xem chi tiết đơn hàng và lịch sử thay đổi
  - Hủy đơn hàng với lý do
- **Priority**: High  
- **Story Points**: 8

---

## 🧩 User Story #5: Quản lý xe và tuyến đường

- **User Story**: _As a logistics manager, I want to manage vehicles and routes so that I can optimize delivery efficiency._
- **Acceptance Criteria**:
  - Thêm/sửa/xóa thông tin xe (biển số, loại xe, tải trọng, trạng thái)
  - Lập tuyến đường tối ưu cho nhiều điểm giao hàng
  - Phân bổ xe cho tài xế
  - Theo dõi trạng thái xe (hoạt động, bảo trì, hỏng hóc)
  - Xem lịch sử sử dụng xe
- **Priority**: Medium  
- **Story Points**: 8
