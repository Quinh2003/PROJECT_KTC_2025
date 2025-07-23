# 📋 Project Requirements Overview – KTC Logistics 2025

## 📦 Tổng Quan Hệ Thống Quản Lý Logistics Thông Minh

KTC Logistics 2025 là một nền tảng quản lý vận hành logistics dành riêng cho doanh nghiệp nội bộ (1PL) nhằm số hóa toàn diện quy trình vận chuyển, tối ưu chi phí, tăng tính minh bạch, đồng thời hỗ trợ quản trị và vận hành hiệu quả.

---

## 🎯 Mục Tiêu Dự Án

Nền tảng hướng đến các mục tiêu cốt lõi:

- 🧭 **Tối ưu hóa vận chuyển**: Quản lý đơn hàng, phương tiện, tài xế và kho nội bộ một cách trực quan, có kiểm soát  
- 🗺️ **Bản đồ 3D thời gian thực**: Theo dõi trực quan trạng thái và hành trình phương tiện  
- 🤖 **Tích hợp AI**: Gợi ý tuyến đường tối ưu hóa thời gian hoặc chi phí giao hàng  
- 📱 **Ứng dụng di động cho tài xế**: Tiếp nhận đơn hàng, cập nhật trạng thái, xác nhận giao hàng, báo cáo sự cố  
- 📊 **Dashboard phân tích hiệu suất**: Hỗ trợ quản lý vận hành ra quyết định nhanh chóng  
- 👑 **Hệ thống phân quyền người dùng**: Hạn chế truy cập và phân luồng dữ liệu theo vai trò  

> ⚠️ **Lưu ý**: Hệ thống không bao gồm customer-facing portal – đúng định hướng 1PL Logistics.

---

## 📈 Lợi Ích Dự Kiến

- 💰 Giảm chi phí vận hành qua việc tối ưu lộ trình, giảm sai sót thủ công  
- ⏱️ Rút ngắn thời gian giao vận và xử lý đơn  
- 👁️ Nâng cao năng lực giám sát và tính minh bạch  
- ⭐ Cải thiện hiệu quả làm việc và trải nghiệm của Dispatcher, Fleet Manager, Driver và Operations Manager

---

## 👥 Vai Trò Người Dùng & Chức Năng Chính

| Vai Trò            | Chức Năng Chính                                                                 |
|--------------------|----------------------------------------------------------------------------------|
| **Dispatcher**      | Tạo/giao lệnh vận chuyển, phân công tài xế, theo dõi trạng thái đơn hàng real-time |
| **Fleet Manager**   | Quản lý đội xe, lịch bảo trì, tình trạng phương tiện                           |
| **Driver (Tài xế)** | Nhận/giao hàng qua mobile app, cập nhật trạng thái, định vị GPS, chụp ảnh xác nhận |
| **Operations Manager** | Theo dõi hiệu suất vận hành, báo cáo KPI, phân tích và điều phối chiến lược    |
| **Admin**           | Quản lý người dùng, phân quyền, cấu hình hệ thống, kiểm tra lịch sử hoạt động    |

---

## 🗂️ Cấu Trúc Thư Mục User Story (Tách Theo Module)

```bash

├── 00-summary.md               # 19 User Stories summary
├── 01-common-ui-stories.md # 🔧 Header, Footer, Sidebar, Loading, Errors
├── 02-auth-page-stories.md # 🔐 Đăng nhập, đăng ký, quên mật khẩu
├── 03-dispatcher-order-management.md # 📦 Giao lệnh, theo dõi, phân đơn
├── 04-fleet-management-stories.md # 🚛 Quản lý phương tiện & lịch bảo trì
├── 05-driver-mobile-app-stories.md # 📱 Mobile App cho tài xế: nhận đơn, trạng thái
├── 06-operations-dashboard-stories.md # 📊 Báo cáo hiệu suất & phân tích
├── 07-ai-route-optimization-stories.md # 🤖 Gợi ý lộ trình thông minh (AI)
├── 08-3d-map-visualization-stories.md # 🗺️ Bản đồ 3D trực quan hóa real-time
├── 10-admin-system-management.md # 👑 Admin quản lý users, roles, config
├── introduction.md # 📋 Tổng quan dự án
├── mockup.md # 🎨 Mô tả UI chi tiết từng màn hình
└── user-roles.md # 🔑 Phân quyền và truy cập hệ thống

```

---

## 🧩 Mapping: Tính Năng theo Modules & Pages

| Module / Trang                | User Stories Liên Quan                       | Số lượng Pages |
|------------------------------|----------------------------------------------|----------------|
| 🔐 Xác thực người dùng        | US-AUTH-LOGIN-01 → REGISTER-01              | 3              |
| 📦 Quản lý đơn hàng (Dispatcher) | US-ORDER-ASSIGN-01 → ORDER-TRACK-03     | 4              |
| 🚛 Quản lý phương tiện (Fleet) | US-FLEET-01 → US-FLEET-MAINTAIN-03       | 3              |
| 📱 Mobile App (Drivers)       | US-DRIVER-HOME-01 → US-DRIVER-DELIVERY-03 | 7              |
| 📊 Dashboard (Operations)     | US-OPS-KPI-01 → US-OPS-ANALYTICS-03       | 3              |
| 🤖 AI Lộ trình tối ưu         | US-AI-SUGGEST-01 → US-AI-COST-03           | 3              |
| 🗺️ Bản đồ theo dõi 3D        | US-MAP-REALTIME-01 → MAP-DETAIL-02         | 2              |
| 🔧 UI Components dùng chung   | US-COMMON-HEADER-01 → ERROR, FOOTER        | Global         |
| 👑 Admin hệ thống             | US-ADMIN-USERS-01, CONFIG-02               | 2              |

---

## ⚙️ Kiến Trúc Kỹ Thuật & Công Nghệ

| Layer        | Công Nghệ / Thành Phần                                                                 |
|--------------|------------------------------------------------------------------------------------------|
| **Frontend** | ReactJS, TailwindCSS, Three.js, CesiumJS (3D Maps), React Native (Mobile App)          |
| **Backend**  | NestJS / Spring Boot, REST API, SignalR (real-time), OAuth2/OIDC Auth                   |
| **Data Layer** | PostgreSQL multi-tenant, MongoDB (logging), Redis (cache), Stripe (for payroll)     |
| **AI/GPS**   | TSP, Dijkstra, OpenRouteService, Mapbox, Google Maps API                               |
| **Deployment** | Containerized: Docker, Kubernetes, .NET Aspire                                      |

---

## 🗓️ Ưu Tiên & Phân Bổ Theo Sprint

| Module                      | Story Points | Ưu Tiên  | Sprint    |
|----------------------------|--------------|----------|-----------|
| 🔐 Authentication          | 15           | ⭐⭐⭐⭐⭐    | Sprint 1  |
| 📦 Order Management        | 30           | ⭐⭐⭐⭐⭐    | Sprint 2  |
| 🚛 Fleet Management        | 16           | ⭐⭐⭐⭐     | Sprint 2  |
| 📱 Mobile App (Drivers)    | 9            | ⭐⭐⭐⭐     | Sprint 2–3|
| 🤖 AI Route Optimization   | 13           | ⭐⭐⭐⭐     | Sprint 3  |
| 🗺️ 3D Map Real-time        | 12           | ⭐⭐⭐⭐     | Sprint 3  |
| 📊 Operations Analytics    | 11           | ⭐⭐⭐      | Sprint 4  |
| 🔧 Common UI Components    | 8            | ⭐⭐⭐⭐⭐    | Sprint 1  |
