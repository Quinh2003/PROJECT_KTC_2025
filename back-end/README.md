# KTC Logistics Backend - Spring Boot Application

## 🚀 Docker Hub Repository

**Public Repository:**

## 📦 Quick Start

### Pull và chạy container:


### Hoặc sử dụng docker-compose:
```bash
git clone <repository-url>
cd spring-project
docker-compose up -d
```

## 🌐 API Resources & Endpoints

### 🔐 Authentication & Authorization
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/auth/login` | POST | User login with email/password | ✅ **Done** |
| `/api/auth/register` | POST | User registration | ✅ **Done** |
| `/api/auth/google` | POST | Google OAuth login | ✅ **Done** |
| `/api/auth/google-credential` | POST | Google credential login | ✅ **Done** |
| `/api/auth/refresh-token` | POST | Refresh JWT token | 🚧 **In Progress** |
| `/api/auth/forgot-password` | POST | Send password reset email | 📝 **Planned** |
| `/api/auth/reset-password` | POST | Reset password with token | 📝 **Planned** |
| `/api/auth/logout` | POST | User logout | 📝 **Planned** |

### 👥 User Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/users` | GET | Get all users (Admin only) | ✅ **Done** |
| `/api/users/{id}` | GET | Get user by ID | ✅ **Done** |
| `/api/users/{id}` | PUT | Update user information | ✅ **Done** |
| `/api/users/{id}` | DELETE | Delete user (soft delete) | ✅ **Done** |
| `/api/users/profile` | GET | Get current user profile | ✅ **Done** |
| `/api/users/profile` | PUT | Update current user profile | ✅ **Done** |
| `/api/users/{id}/activity-logs` | GET | Get user activity logs | ✅ **Done** |
| `/api/users/search` | GET | Search users by criteria | 📝 **Planned** |

### 🏗️ Category Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/categories` | GET | Get all categories with filters | ✅ **Done** |
| `/api/categories/tree` | GET | Get category tree structure | ✅ **Done** |
| `/api/categories/{id}` | GET | Get category by ID | ✅ **Done** |
| `/api/categories` | POST | Create new category | ✅ **Done** |
| `/api/categories/{id}` | PUT | Update category | ✅ **Done** |
| `/api/categories/{id}` | PATCH | Partial update category | ✅ **Done** |
| `/api/categories/{id}` | DELETE | Delete category | ✅ **Done** |
| `/api/categories/{id}/products` | GET | Get products in category | ✅ **Done** |
| `/api/categories/{id}/statistics` | GET | Get category statistics | ✅ **Done** |

### 📦 Product Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/products` | GET | Get all products with filters | 🚧 **In Progress** |
| `/api/products/{id}` | GET | Get product by ID | 🚧 **In Progress** |
| `/api/products` | POST | Create new product | 🚧 **In Progress** |
| `/api/products/{id}` | PUT | Update product | 🚧 **In Progress** |
| `/api/products/{id}` | DELETE | Delete product | 🚧 **In Progress** |
| `/api/products/search` | GET | Search products | 📝 **Planned** |
| `/api/products/{id}/inventory` | GET | Get product inventory | 📝 **Planned** |

### 📋 Order Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/orders` | GET | Get all orders | 🚧 **In Progress** |
| `/api/orders/{id}` | GET | Get order by ID | 🚧 **In Progress** |
| `/api/orders` | POST | Create new order | 🚧 **In Progress** |
| `/api/orders/{id}` | PUT | Update order | 🚧 **In Progress** |
| `/api/orders/{id}/status` | PATCH | Update order status | 📝 **Planned** |
| `/api/orders/{id}/items` | GET | Get order items | 🚧 **In Progress** |
| `/api/orders/{id}/tracking` | GET | Get order tracking info | 📝 **Planned** |

### 🚚 Vehicle Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/vehicles` | GET | Get all vehicles | 🚧 **In Progress** |
| `/api/vehicles/{id}` | GET | Get vehicle by ID | 🚧 **In Progress** |
| `/api/vehicles` | POST | Create new vehicle | 🚧 **In Progress** |
| `/api/vehicles/{id}` | PUT | Update vehicle | 🚧 **In Progress** |
| `/api/vehicles/{id}` | DELETE | Delete vehicle | 🚧 **In Progress** |
| `/api/vehicles/{id}/maintenance` | GET | Get vehicle maintenance history | 📝 **Planned** |
| `/api/vehicles/{id}/assignments` | GET | Get vehicle assignments | 📝 **Planned** |
| `/api/vehicles/available` | GET | Get available vehicles | 📝 **Planned** |

### 📍 Route Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/routes` | GET | Get all routes | 🚧 **In Progress** |
| `/api/routes/{id}` | GET | Get route by ID | 🚧 **In Progress** |
| `/api/routes` | POST | Create new route | 🚧 **In Progress** |
| `/api/routes/{id}` | PUT | Update route | 🚧 **In Progress** |
| `/api/routes/{id}/optimize` | POST | Optimize route | 📝 **Planned** |
| `/api/routes/{id}/tracking` | GET | Get route tracking | 📝 **Planned** |

### 🚛 Delivery Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/deliveries` | GET | Get all deliveries | 🚧 **In Progress** |
| `/api/deliveries/{id}` | GET | Get delivery by ID | 🚧 **In Progress** |
| `/api/deliveries` | POST | Create new delivery | 🚧 **In Progress** |
| `/api/deliveries/{id}` | PUT | Update delivery | 🚧 **In Progress** |
| `/api/deliveries/{id}/status` | PATCH | Update delivery status | 📝 **Planned** |
| `/api/deliveries/{id}/proof` | POST | Upload delivery proof | 🚧 **In Progress** |
| `/api/deliveries/{id}/tracking` | GET | Get delivery tracking | 🚧 **In Progress** |

### 📱 GPS Tracking
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/tracking/vehicles/{id}` | GET | Get vehicle real-time location | 🚧 **In Progress** |
| `/api/tracking/deliveries/{id}` | GET | Get delivery tracking points | 🚧 **In Progress** |
| `/api/tracking/update` | POST | Update GPS location | 📝 **Planned** |
| `/api/tracking/history` | GET | Get tracking history | 📝 **Planned** |

### 💰 Payment Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/payments` | GET | Get all payments | 🚧 **In Progress** |
| `/api/payments/{id}` | GET | Get payment by ID | 🚧 **In Progress** |
| `/api/payments` | POST | Create payment | 🚧 **In Progress** |
| `/api/payments/{id}/status` | PATCH | Update payment status | 📝 **Planned** |

### 🏪 Store Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/stores` | GET | Get all stores | 🚧 **In Progress** |
| `/api/stores/{id}` | GET | Get store by ID | 🚧 **In Progress** |
| `/api/stores` | POST | Create new store | 🚧 **In Progress** |
| `/api/stores/{id}` | PUT | Update store | 🚧 **In Progress** |

### 🏭 Warehouse Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/warehouses` | GET | Get all warehouses | 🚧 **In Progress** |
| `/api/warehouses/{id}` | GET | Get warehouse by ID | 🚧 **In Progress** |
| `/api/warehouses` | POST | Create new warehouse | 🚧 **In Progress** |
| `/api/warehouses/{id}` | PUT | Update warehouse | 🚧 **In Progress** |
| `/api/warehouses/{id}/transactions` | GET | Get warehouse transactions | 📝 **Planned** |
| `/api/warehouses/{id}/inventory` | GET | Get warehouse inventory | 📝 **Planned** |

### 📊 Dashboard & Analytics
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/dashboard/overview` | GET | Get dashboard overview stats | 🚧 **In Progress** |
| `/api/dashboard/kpi` | GET | Get KPI metrics | 📝 **Planned** |
| `/api/reports/orders` | GET | Generate order reports | 🚧 **In Progress** |
| `/api/reports/deliveries` | GET | Generate delivery reports | 🚧 **In Progress** |
| `/api/reports/performance` | GET | Generate performance reports | 📝 **Planned** |
| `/api/reports/export` | POST | Export reports to file | 📝 **Planned** |

### 👑 Admin Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/admin/users` | GET | Admin - manage all users | 🚧 **In Progress** |
| `/api/admin/system-config` | GET | Get system configurations | 📝 **Planned** |
| `/api/admin/system-config` | PUT | Update system configurations | 📝 **Planned** |
| `/api/admin/activity-logs` | GET | Get all activity logs | 📝 **Planned** |
| `/api/admin/audit` | GET | System audit reports | 📝 **Planned** |

### 🔒 Protected Resources (Testing)
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/protected/profile` | GET | Test JWT authentication | ✅ **Done** |
| `/api/protected/admin/users` | GET | Test admin role access | ✅ **Done** |
| `/api/protected/dispatcher/dashboard` | GET | Test dispatcher access | ✅ **Done** |
| `/api/protected/driver/routes` | GET | Test driver access | ✅ **Done** |

### 📋 Status Management
| Endpoint | Method | Description | Status |
|----------|--------|-------------|--------|
| `/api/status` | GET | Get all system statuses | 🚧 **In Progress** |
| `/api/status/{id}` | GET | Get status by ID | 🚧 **In Progress** |
| `/api/status` | POST | Create new status | 📝 **Planned** |

## 📊 API Status Summary

| Status | Count | Description |
|--------|-------|-------------|
| ✅ **Done** | 17 | Fully implemented and tested |
| 🚧 **In Progress** | 41 | Basic structure exists, needs completion |
| 📝 **Planned** | 32 | Planned for future implementation |
| **Total** | **90** | **Total API endpoints** |

## 🔧 Configuration

Application sử dụng remote MySQL database:
- **Server:** server.aptech.io:3307
- **Database:** fastroute_test
- **Username:** fastroute_user

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

---
