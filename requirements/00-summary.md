# 📋 Summary Table of All User Stories

Bảng này cung cấp cái nhìn tổng quan về toàn bộ các **User Stories** trong hệ thống quản lý Logistics.  
Mỗi dòng đại diện cho một tính năng chính của người dùng cuối (user-facing feature), được tổ chức theo:

- **Use Case ID**: Mã định danh duy nhất của từng chức năng.
- **Name**: Tên rút gọn, mô tả ngắn gọn tính năng.
- **Description**: Mô tả chức năng đó giúp người dùng đạt được mục tiêu gì.

Thông qua bảng này, nhóm phát triển và stakeholders có thể dễ dàng theo dõi phạm vi dự án, mức độ ưu tiên và tổ chức triển khai.

| Use Case ID               | Name                                  | Description |
|---------------------------|---------------------------------------|-------------|
| US-COMMON-HEADER-01       | Consistent Header                     | Hiển thị header nhất quán trên mọi trang với logo, tên user, vai trò, và logout |
| US-COMMON-FOOTER-01       | System Footer                         | Hiển thị footer gồm thông tin hệ thống, năm, phiên bản, link bảo mật & hỗ trợ |
| US-COMMON-ERROR-01        | Error Message Display                 | Hiển thị lỗi rõ ràng, phân biệt frontend/backend, kèm hướng dẫn xử lý |
| US-AUTH-LOGIN-01          | Login with Email & Password           | Đăng nhập bằng email/password, báo lỗi nếu sai, chuyển dashboard theo vai trò |
| US-AUTH-FORGOT-01         | Password Recovery                     | Gửi email reset mật khẩu, mã xác thực hết hạn sau 15 phút |
| US-ORDER-CREATE-01        | Create Delivery Order                 | Tạo đơn hàng mới với địa chỉ, mô tả, chọn xe, tài xế, thời gian giao |
| US-ORDER-ASSIGN-01        | Assign Vehicle & Driver               | Gán xe và tài xế, kiểm tra trùng lịch |
| US-ORDER-TRACK-01         | Real-Time Order Tracking              | Theo dõi trạng thái đơn hàng theo thời gian thực trên bản đồ |
| US-FLEET-LIST-01          | View Vehicle List                     | Danh sách xe với thông tin loại, biển số, tình trạng hiện tại |
| US-FLEET-MAINTAIN-01      | Manage Vehicle Maintenance            | Tạo lịch bảo trì, cảnh báo, lưu lịch sử bảo trì từng xe |
| US-DRIVER-HOME-01         | Driver View New Orders                | Tài xế xem đơn mới, chi tiết đơn, tuyến đường |
| US-DRIVER-STATUS-UPDATE-01| Update Delivery Status                | Cập nhật trạng thái: nhận – giao – hoàn tất, gửi GPS, ảnh, chữ ký |
| US-OPS-KPI-01             | View Real-Time KPIs                   | Xem tỷ lệ giao đúng, trễ, thời gian trung bình |
| US-OPS-ANALYTICS-01       | Analyze Route & Driver Performance    | Báo cáo tuyến, tài xế, thời gian, biểu đồ |
| US-AI-SUGGEST-01          | AI Suggest Optimal Routes             | AI đề xuất tuyến giao tối ưu, kèm lý do và khả năng điều chỉnh |
| US-MAP-REALTIME-01        | 3D Real-Time Vehicle Visualization    | Bản đồ 3D (CesiumJS), định vị GPS, thể hiện trạng thái chuyến hàng |
| US-MAP-DETAIL-02          | Detailed Route & Delivery Tracking    | Hiển thị tuyến đường chi tiết, animation di chuyển, ETA, lịch sử hành trình |
| US-MAP-FILTER-03          | Vehicle Filter & Search on Map        | Lọc xe theo trạng thái, tìm kiếm theo biển số, tài xế, mã đơn hàng |
| US-OPS-EXPORT-01          | Export Reports & Data Files           | Xuất báo cáo KPI, dữ liệu phân tích thành Excel/CSV/PDF với tùy chọn |
| US-ADMIN-USERS-01         | Manage Users and Roles                | CRUD user, phân quyền vai trò, theo dõi thay đổi (audit logs) |
| US-ADMIN-CONFIG-01        | Manage System Configurations          | Thiết lập tham số, tích hợp API, thông báo định kỳ |
