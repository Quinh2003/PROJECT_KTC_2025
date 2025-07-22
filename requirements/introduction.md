# 📋 Project Requirements Overview - KTC Logistics 2025

## 📦 Tổng Quan Hệ Thống Quản Lý Logistics Thông Minh

### 🎯 Mục Tiêu

Xây dựng một nền tảng vận hành logistics hiện đại, trực quan và tối ưu, hỗ trợ:

- 🏢 Quản lý đầu–cuối chuỗi cung ứng: đơn hàng, phương tiện, kho bãi  
- 🗺️ Hiển thị bản đồ 3D các tuyến vận chuyển với theo dõi thời gian thực
- 🤖 AI gợi ý lộ trình tối ưu (theo thời gian hoặc chi phí)  
- 📱 Ứng dụng mobile cho tài xế với chức năng hoàn thiện
- 👤 Portal khách hàng để tạo và theo dõi đơn hàng
- 📊 Báo cáo và thống kê hiệu suất vận hành chi tiết

### 📈 Lợi Ích Chính

- **💰 Tiết kiệm chi phí** vận hành và nhiên liệu  
- **⏱️ Rút ngắn thời gian** giao nhận  
- **👁️ Tăng tính minh bạch** và khả năng giám sát  
- **⭐ Nâng cao trải nghiệm** cho cả điều phối viên, tài xế và khách hàng  

### 👥 Đối Tượng Sử Dụng & Vai Trò

#### 👑 **Administrator (Quản trị viên)**

- **Quyền hạn**: Full system access, user management, system configuration
- **Trách nhiệm**:
  - Quản lý người dùng và phân quyền
  - Cấu hình hệ thống và các tham số
  - Giám sát toàn bộ hoạt động logistics
  - Xem báo cáo tổng hợp và analytics
  - Quản lý master data (warehouses, vehicles, routes)

#### 🚦 **Dispatcher (Điều phối viên)**

- **Quyền hạn**: Order management, vehicle assignment, route optimization
- **Trách nhiệm**:
  - Xử lý và phân công đơn hàng từ khách hàng
  - Phân bổ tài xế và phương tiện tối ưu
  - Sử dụng AI để tối ưu hóa lộ trình
  - Theo dõi tiến độ giao hàng real-time trên bản đồ 3D
  - Xử lý các tình huống bất thường

#### 🚚 **Driver (Tài xế)**

- **Quyền hạn**: Mobile app access, delivery management, status updates
- **Trách nhiệm**:
  - Sử dụng mobile app để nhận đơn hàng được phân công
  - Cập nhật trạng thái giao hàng theo thời gian thực
  - Chụp ảnh và lấy chữ ký xác nhận giao hàng
  - Sử dụng GPS navigation tích hợp
  - Liên lạc với khách hàng khi cần thiết

#### 👤 **Customer (Khách hàng)**

- **Quyền hạn**: Create orders, track deliveries, manage profile
- **Trách nhiệm**:
  - **🆕 Tạo đơn hàng mới** với form đầy đủ thông tin
  - **📍 Theo dõi đơn hàng** theo thời gian thực
  - Xem lịch sử giao dịch và đơn hàng
  - Quản lý hồ sơ cá nhân và phương thức thanh toán
  - Đánh giá chất lượng dịch vụ và tài xế

---

## 📁 Cây Phân Cấp Thư Mục User Story & Các File `.md`

```bash
├── 01-common-ui-stories.md               # 🔧 Header, Footer, Loading/Error States
├── 02-auth-page-stories.md               # 🔐 Trang Đăng nhập/Đăng ký/Quên mật khẩu  
├── 03-admin-dashboard-user-management.md # 👑 Dashboard Admin + Quản lý người dùng
├── 04-order-management-stories.md        # 📦 Trang Quản lý Đơn hàng (Dispatcher)
├── 05-vehicle-management-stories.md      # 🚛 Trang Quản lý Phương tiện
├── 06-warehouse-management-stories.md    # 🏢 Trang Quản lý Kho bãi
├── 07-3d-map-visualization-stories.md    # 🗺️ Trang Bản Đồ 3D & Theo dõi thời gian thực
├── 08-ai-route-optimization-stories.md   # 🤖 Module AI Đề Xuất Lộ Trình Tối Ưu
├── 09-driver-mobile-stories.md           # 📱 Ứng dụng Mobile cho Tài xế (7 pages)
├── 10-customer-tracking-stories.md       # 👤 Portal Khách hàng (6 pages)
├── 11-reporting-analytics-stories.md     # 📊 Báo cáo & Analytics Dashboard (6 pages)
├── introduction.md                       # 📋 File này - Tổng quan dự án
├── mockup.md                            # 🎨 Mô tả giao diện từng trang
└── user-roles.md                        # 🔑 Phân quyền chi tiết theo vai trò
```

---

## 📊 Mapping Giữa Pages/Modules vs User Stories

| 🖥️ **Trang Giao Diện / Module**          | 📝 **User Stories Liên Quan**                                               | 📄 **Số Pages** |
|-------------------------------------------|-----------------------------------------------------------------------------|------------------|
| **🔐 Trang Đăng Nhập**                    | `US-AUTH-LOGIN-01` - Đăng nhập hệ thống                                    | 1 page           |
| **📝 Trang Đăng Ký**                      | `US-AUTH-REGISTER-01` - Đăng ký tài khoản mới                             | 1 page           |
| **🔒 Trang Quên Mật Khẩu**                | `US-AUTH-FORGOT-01` - Khôi phục mật khẩu                                  | 1 page           |
| **👑 Dashboard Admin**                    | `US-ADMIN-DASH-01` → `US-ADMIN-DASH-04`                                   | 3 pages          |
| **📦 Quản lý Đơn hàng**                   | `US-ORDER-LIST-01` → `US-ORDER-ASSIGN-03`                                 | 4 pages          |
| **🚛 Quản lý Phương tiện**                | `US-VEHICLE-01` → `US-VEHICLE-03`                                          | 3 pages          |
| **🏢 Quản lý Kho Bãi**                    | `US-WAREHOUSE-01` → `US-WAREHOUSE-02`                                     | 2 pages          |
| **🗺️ Bản Đồ 3D Visualization**           | `US-MAP-MAIN-01` → `US-MAP-ANALYTICS-02`                                  | 2 pages          |
| **🤖 AI Đề Xuất Lộ Trình**               | `US-AI-01` → `US-AI-03`                                                   | 3 pages          |
| **📱 Mobile App Tài xế**                  | `US-DRIVER-DASH-01` → `US-DRIVER-SETTINGS-02`                             | 2 pages          |
| **👤 Portal Khách hàng**                  | `US-CUSTOMER-DASH-01` → `US-CUSTOMER-TRACK-01`                            | 3 pages          |
| **📊 Báo cáo & Analytics**                | `US-REPORT-EXEC-01` → `US-REPORT-OPS-02`                                  | 2 pages          |
| **🔧 UI Components Chung**                | `US-COMMON-HEADER-01`, `US-COMMON-FOOTER-01`, `US-COMMON-LOADING-01`      | Global           |

---

### 🧰 Công Nghệ Dự Kiến

- **Frontend**: ReactJS + Three.js, Tailwind CSS  
- **Backend**: NestJS / Spring Boot  
- **Data & Realtime**: PostgreSQL / MongoDB, WebSocket/Firebase  
- **AI Lộ Trình**: Thuật toán TSP, Dijkstra hoặc dịch vụ OpenRouteService  
- **Bản đồ**: Mapbox / CesiumJS / Google Maps API  

---

## 📈 Phân Bổ Story Points Theo Module

| **Module**                    | **Tổng Story Points** | **Độ Ưu Tiên** | **Timeline** |
|-------------------------------|----------------------|----------------|--------------|
| 🔐 Authentication             | 15 points            | ⭐⭐⭐⭐⭐        | Sprint 1     |
| 👑 Admin Dashboard            | 25 points            | ⭐⭐⭐⭐⭐        | Sprint 1-2   |
| 📦 Order Management           | 30 points            | ⭐⭐⭐⭐⭐        | Sprint 2     |
| 🗺️ 3D Map Visualization       | 14 points            | ⭐⭐⭐⭐         | Sprint 3     |
| 📱 Driver Mobile App          | 9 points             | ⭐⭐⭐⭐⭐        | Sprint 2-3   |
| 👤 Customer Portal            | 14 points            | ⭐⭐⭐⭐         | Sprint 3     |
| 📊 Reporting & Analytics      | 13 points            | ⭐⭐⭐           | Sprint 4     |
| 🔧 Common UI Components       | 8 points             | ⭐⭐⭐⭐⭐        | Sprint 1     |
