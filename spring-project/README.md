# KTC Logistics Backend - Spring Boot Application

## 🚀 Docker Hub Repository

**Public Repository:** https://hub.docker.com/r/fanglee2003/ktc-logistics-backend

## 📦 Quick Start

### Pull và chạy container:

```bash
docker pull fanglee2003/ktc-logistics-backend
docker run -d -p 8080:8080 fanglee2003/ktc-logistics-backend
```

## 🌐 API Resources & Endpoints

### 🔐 Authentication & Authorization

| Endpoint                        | Method | Description                    | Status      |
|----------------------------------|--------|--------------------------------|-------------|
| `/api/auth/login`                | POST   | User login                     | ✅ Đã làm   |
| `/api/auth/register`             | POST   | User registration              | ✅ Đã làm   |
| `/api/auth/google`               | POST   | Google OAuth login             | ✅ Đã làm   |
| `/api/auth/google-credential`    | POST   | Google credential login        | ✅ Đã làm   |
| `/api/auth/forgot-password`      | POST   | Send password reset email      | ❌ Chưa làm |
| `/api/auth/reset-password`       | POST   | Reset password with token      | ❌ Chưa làm |
| `/api/auth/logout`               | POST   | User logout                    | ❌ Chưa làm |

### 👥 User Management

| Endpoint                              | Method | Description                   | Status      |
|----------------------------------------|--------|-------------------------------|-------------|
| `/api/users`                          | GET    | Get all users                 | ✅ Đã làm   |
| `/api/users/{id}`                     | GET    | Get user by ID                | ✅ Đã làm   |
| `/api/users/{id}`                     | PUT    | Update user                   | ✅ Đã làm   |
| `/api/users/{id}`                     | DELETE | Delete user (soft)            | ✅ Đã làm   |
| `/api/users/profile`                  | GET    | Get current user profile      | ✅ Đã làm   |
| `/api/users/profile`                  | PUT    | Update current user profile   | ✅ Đã làm   |
| `/api/users/{id}/activity-logs`       | GET    | Get user activity logs        | ❌ Chưa làm |

### 🏗️ Category Management

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/categories`                          | GET    | Get all categories           | ✅ Đã làm   |
| `/api/categories/tree`                     | GET    | Get category tree            | ✅ Đã làm   |
| `/api/categories/{id}`                     | GET    | Get category by ID           | ✅ Đã làm   |
| `/api/categories`                          | POST   | Create new category          | ✅ Đã làm   |
| `/api/categories/{id}`                     | PUT    | Update category              | ✅ Đã làm   |
| `/api/categories/{id}`                     | PATCH  | Partial update category      | ✅ Đã làm   |
| `/api/categories/{id}`                     | DELETE | Delete category              | ✅ Đã làm   |
| `/api/categories/{id}/products`            | GET    | Get products in category     | ✅ Đã làm   |
| `/api/categories/{id}/statistics`          | GET    | Get category statistics      | ✅ Đã làm   |

### 📦 Product Management

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/products`                            | GET    | Get all products             | ✅ Đã làm   |
| `/api/products/{id}`                       | GET    | Get product by ID            | ✅ Đã làm   |
| `/api/products`                            | POST   | Create new product           | ✅ Đã làm   |
| `/api/products/{id}`                       | PUT    | Update product               | ✅ Đã làm   |
| `/api/products/{id}`                       | PATCH  | Partial update product       | ✅ Đã làm   |
| `/api/products/{id}`                       | DELETE | Delete product               | ✅ Đã làm   |
| `/api/products/{id}/inventory`             | GET    | Get product inventory        | ❌ Chưa làm |

### 📋 Order Management

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/orders`                              | GET    | Get all orders               | ✅ Đã làm   |
| `/api/orders/{id}`                         | GET    | Get order by ID              | ✅ Đã làm   |
| `/api/orders`                              | POST   | Create new order             | ✅ Đã làm   |
| `/api/orders/{id}`                         | PUT    | Update order                 | ✅ Đã làm   |
| `/api/orders/{id}`                         | PATCH  | Partial update order         | ✅ Đã làm   |
| `/api/orders/{id}/status`                  | PATCH  | Update order status          | ✅ Đã làm   |
| `/api/orders/{id}/items`                   | GET    | Get order items              | ❌ Chưa làm |
| `/api/orders/{id}/tracking`                | GET    | Get order tracking info      | ✅ Đã làm   |

### 🚚 Vehicle Management

| Endpoint                                   | Method     | Description                  | Status      |
|---------------------------------------------|------------|------------------------------|-------------|
| `/api/vehicles`                            | GET        | Get all vehicles             | ✅ Đã làm   |
| `/api/vehicles/{id}`                       | GET        | Get vehicle by ID            | ✅ Đã làm   |
| `/api/vehicles`                            | POST       | Create new vehicle           | ✅ Đã làm   |
| `/api/vehicles/{id}`                       | PUT/PATCH  | Update vehicle               | ✅ Đã làm   |
| `/api/vehicles/{id}`                       | DELETE     | Delete vehicle               | ✅ Đã làm   |
| `/api/vehicles/{id}/maintenance`           | GET        | Get vehicle maintenance      | ❌ Chưa làm |
| `/api/vehicles/{id}/assignments`           | GET        | Get vehicle assignments      | ❌ Chưa làm |
| `/api/vehicles/available`                  | GET        | Get available vehicles       | ❌ Chưa làm |

### 📍 Route Management

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/routes`                              | GET    | Get all routes               | ✅ Đã làm   |
| `/api/routes/{id}`                         | GET    | Get route by ID              | ✅ Đã làm   |
| `/api/routes`                              | POST   | Create new route             | ✅ Đã làm   |
| `/api/routes/{id}`                         | DELETE | Delete route                 | ✅ Đã làm   |
| `/api/routes/{id}`                         | PUT    | Update route                 | ✅ Đã làm   |
| `/api/routes/{id}/tracking`                | GET    | Get route tracking           | ✅ Đã làm   |

### 🚛 Delivery Management

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/deliveries`                          | GET    | Get all deliveries           | ✅ Đã làm   |
| `/api/deliveries/{id}`                     | GET    | Get delivery by ID           | ✅ Đã làm   |
| `/api/deliveries`                          | POST   | Create new delivery          | ✅ Đã làm   |
| `/api/deliveries/{id}`                     | DELETE | Delete delivery              | ✅ Đã làm   |
| `/api/deliveries/{id}`                     | PUT    | Update delivery              | ✅ Đã làm   |
| `/api/deliveries/{id}/tracking`            | GET    | Get delivery tracking        | ✅ Đã làm   |

### 📱 GPS Tracking

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/tracking/vehicles/{id}`              | GET    | Get vehicle real-time loc    | ❌ Chưa làm |
| `/api/tracking/deliveries/{id}`            | GET    | Get delivery tracking points | ❌ Chưa làm |
| `/api/tracking/update`                     | POST   | Update GPS location          | ❌ Chưa làm |
| `/api/tracking/history`                    | GET    | Get tracking history         | ❌ Chưa làm |

### 💰 Payment Management

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/payments`                            | GET    | Get all payments             | ✅ Đã làm   |
| `/api/payments/{id}`                       | GET    | Get payment by ID            | ✅ Đã làm   |
| `/api/payments/{id}`                       | DELETE | Delete payment               | ✅ Đã làm   |
| `/api/payments`                            | POST   | Create payment               | ✅ Đã làm   |
| `/api/payments/{id}`                       | PUT    | Update payment               | ✅ Đã làm   |

### 🏪 Store Management

| Endpoint                                   | Method     | Description                  | Status      |
|---------------------------------------------|------------|------------------------------|-------------|
| `/api/stores`                              | GET        | Get all stores               | ✅ Đã làm   |
| `/api/stores/{id}`                         | GET        | Get store by ID              | ✅ Đã làm   |
| `/api/stores`                              | POST       | Create new store             | ✅ Đã làm   |
| `/api/stores/{id}`                         | PUT/PATCH  | Update store                 | ✅ Đã làm   |
| `/api/stores/{id}`                         | DELETE     | Delete store                 | ✅ Đã làm   |

### 🏭 Warehouse Management

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/warehouses`                          | GET    | Get all warehouses           | ✅ Đã làm   |
| `/api/warehouses/{id}`                     | GET    | Get warehouse by ID          | ✅ Đã làm   |
| `/api/warehouses`                          | POST   | Create new warehouse         | ✅ Đã làm   |
| `/api/warehouses/{id}`                     | PUT    | Update warehouse             | ✅ Đã làm   |
| `/api/warehouses/{id}`                     | DELETE | Delete warehouse             | ✅ Đã làm   |
| `/api/warehouses/{id}/transactions`        | GET    | Get warehouse transactions   | ✅ Đã làm |
| `/api/warehouses/{id}/inventory`           | GET    | Get warehouse inventory      | ✅ Đã làm |

### 🧾 Electronic Invoice Management

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/invoices/check-eligibility/{orderId}` | GET    | Check order invoice eligibility | ✅ Đã làm   |
| `/api/invoices`                            | POST   | Create new electronic invoice  | ✅ Đã làm   |
| `/api/invoices`                            | GET    | Get all invoices with filters  | ✅ Đã làm   |
| `/api/invoices/{id}`                       | GET    | Get invoice by ID             | ✅ Đã làm   |
| `/api/invoices/by-order/{orderId}`         | GET    | Get invoice by order ID       | ✅ Đã làm   |
| `/api/invoices/{id}/send-email`            | POST   | Send invoice via email        | ✅ Đã làm   |
| `/api/invoices/{id}/cancel`                | POST   | Cancel invoice (Admin only)   | ✅ Đã làm   |
| `/api/invoices/{id}/generate-pdf`          | POST   | Generate invoice PDF          | ✅ Đã làm   |
| `/api/invoices/{id}/download-pdf`          | GET    | Download invoice PDF          | ✅ Đã làm   |
| `/api/invoices/orders-needing-invoice`     | GET    | Get orders needing invoice    | ✅ Đã làm   |

### 📊 Dashboard & Analytics

| Endpoint                                   | Method | Description                  | Status      |
|---------------------------------------------|--------|------------------------------|-------------|
| `/api/dashboard/overview`                  | GET    | Get dashboard overview stats | ❌ Chưa làm |
| `/api/dashboard/kpi`                       | GET    | Get KPI metrics              | ❌ Chưa làm |
| `/api/reports/orders`                      | GET    | Generate order reports       | ❌ Chưa làm |
| `/api/reports/deliveries`                  | GET    | Generate delivery reports    | ❌ Chưa làm |
| `/api/reports/performance`                 | GET    | Generate performance reports | ❌ Chưa làm |
| `/api/reports/export`                      | POST   | Export reports to file       | ❌ Chưa làm |

---

## 📊 API Status Summary

| Status         | Count | Description                        |
|----------------|-------|------------------------------------|
| ✅ Đã làm       | 76    | Đã có controller & test Postman    |
| ❌ Chưa làm     | 19    | Chưa có hoặc chưa hoàn thiện       |
| **Total**      | **95**| **Tổng số endpoint kiểm tra được** |

## 🔧 Configuration

Ứng dụng sử dụng remote MySQL database:

- **Server:** server.aptech.io:3307
- **Database:** fastroute_test
- **Username:** fastroute_user

### 🧾 Electronic Invoice Configuration

Hệ thống hóa đơn thanh toán được cấu hình với:

- **Thời gian xuất hóa đơn:** 365 ngày (có thể config qua `invoice.expiry.days`)
- **Tự động tạo PDF:** Có
- **Tự động gửi email:** Có (config qua `invoice.email.auto.send`)
- **Email SMTP:** Gmail (config qua `spring.mail.*`)
- **Lưu trữ PDF:** `./invoices/pdfs/` (config qua `invoice.pdf.storage.path`)
- **Phân quyền:** ADMIN, OPERATIONS, DISPATCHER

## 🏗️ Tech Stack

- **Framework:** Spring Boot 3.x
- **Security:** Spring Security + JWT
- **Database:** MySQL 8.0+
- **ORM:** JPA/Hibernate
- **Build Tool:** Gradle
- **Container:** Docker

## 📋 Quick Links

- **Health Check:** http://localhost:8080/actuator/health
- **API Documentation:** http://localhost:8080/swagger-ui.html (planned)
- **Database Schema:** [docs/schemaDB/tables_documentation.md](docs/schemaDB/tables_documentation.md)
- **User Stories:** [docs/userStory/](docs/userStory/)