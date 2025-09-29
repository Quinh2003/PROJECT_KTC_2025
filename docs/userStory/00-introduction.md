# 📋 KTC Logistics 2025 - User Stories

---

## 👥 Vai Trò Người Dùng & Chức Năng Chính

| Vai Trò            | Chức Năng Chính                                                                 |
|--------------------|----------------------------------------------------------------------------------|
| **Dispatcher**      | Tạo/giao lệnh vận chuyển, phân công tài xế, theo dõi trạng thái đơn hàng real-time |
| **Fleet Manager**   | Quản lý đội xe, lịch bảo trì, tình trạng phương tiện                           |
| **Driver (Tài xế)** | Nhận/giao hàng qua mobile app, cập nhật trạng thái, định vị GPS, chụp ảnh xác nhận |
| **Operations Manager** | Theo dõi hiệu suất vận hành, báo cáo KPI, phân tích và điều phối chiến lược    |
| **Administrator**   | Quản lý người dùng, phân quyền, cấu hình hệ thống, kiểm tra lịch sử hoạt động    |

---

## � Tổng Hợp User Stories & Modules

| ID | Tên | Mô Tả | Module/File |
|----|-----|-------|------------|
| US-COMMON-HEADER-01 | Consistent Header | Hiển thị header nhất quán trên mọi trang với logo, tên user, vai trò, và logout | 🔧 `01-common-ui-stories.md` |
| US-COMMON-FOOTER-01 | System Footer | Hiển thị footer gồm thông tin hệ thống, năm, phiên bản, link bảo mật & hỗ trợ | 🔧 `01-common-ui-stories.md` |
| US-COMMON-ERROR-01 | Error Message Display | Hiển thị lỗi rõ ràng, phân biệt frontend/backend, kèm hướng dẫn xử lý | 🔧 `01-common-ui-stories.md` |
| US-AUTH-LOGIN-01 | Login with Email & Password | Đăng nhập bằng email/password, báo lỗi nếu sai, chuyển dashboard theo vai trò | 🔐 `02-auth-page-stories.md` |
| US-AUTH-FORGOT-01 | Password Recovery | Gửi email reset mật khẩu, mã xác thực hết hạn sau 15 phút | 🔐 `02-auth-page-stories.md` |
| US-ORDER-CREATE-01 | Create Delivery Order | Tạo đơn hàng mới với địa chỉ, mô tả, chọn xe, tài xế, thời gian giao | 📦 `03-dispatcher-order-management.md` |
| US-ORDER-ASSIGN-01 | Assign Vehicle & Driver | Gán xe và tài xế, kiểm tra trùng lịch | 📦 `03-dispatcher-order-management.md` |
| US-ORDER-TRACK-01 | Real-Time Order Tracking | Theo dõi trạng thái đơn hàng theo thời gian thực trên bản đồ | 📦 `03-dispatcher-order-management.md` |
| US-FLEET-LIST-01 | View Vehicle List | Danh sách xe với thông tin loại, biển số, tình trạng hiện tại | 🚛 `04-fleet-management-stories.md` |
| US-FLEET-MAINTAIN-01 | Manage Vehicle Maintenance | Tạo lịch bảo trì, cảnh báo, lưu lịch sử bảo trì từng xe | 🚛 `04-fleet-management-stories.md` |
| US-DRIVER-HOME-01 | Driver View New Orders | Tài xế xem đơn mới, chi tiết đơn, tuyến đường | 📱 `05-driver-mobile-app-stories.md` |
| US-DRIVER-STATUS-UPDATE-01 | Update Delivery Status | Cập nhật trạng thái: nhận – giao – hoàn tất, gửi GPS, ảnh, chữ ký | 📱 `05-driver-mobile-app-stories.md` |
| US-OPS-KPI-01 | View Real-Time KPIs | Xem tỷ lệ giao đúng, trễ, thời gian trung bình | 📊 `06-operations-dashboard-stories.md` |
| US-OPS-ANALYTICS-01 | Analyze Route & Driver Performance | Báo cáo tuyến, tài xế, thời gian, biểu đồ | 📊 `06-operations-dashboard-stories.md` |
| US-AI-SUGGEST-01 | AI Suggest Optimal Routes | AI đề xuất tuyến giao tối ưu, kèm lý do và khả năng điều chỉnh | 🤖 `07-ai-route-and-3d-map-stories.md` |
| US-MAP-REALTIME-01 | 3D Real-Time Vehicle Visualization | Bản đồ 3D (CesiumJS), định vị GPS, thể hiện trạng thái chuyến hàng | 🗺️ `07-ai-route-and-3d-map-stories.md` |
| US-MAP-DETAIL-02 | Detailed Route & Delivery Tracking | Hiển thị tuyến đường chi tiết, animation di chuyển, ETA, lịch sử hành trình | 🗺️ `07-ai-route-and-3d-map-stories.md` |
| US-ADMIN-USERS-01 | User & Role Management | Quản lý người dùng, phân quyền, theo dõi truy cập hệ thống | 👑 `09-admin-system-management.md` |
| US-ADMIN-CONFIG-01 | Manage System Configurations | Thiết lập tham số, tích hợp API, thông báo định kỳ | 👑 `09-admin-system-management.md` |
| US-ADMIN-SECURITY-01 | Security Management | Quản lý bảo mật và giám sát hoạt động hệ thống | 👑 `09-admin-system-management.md` |

---
