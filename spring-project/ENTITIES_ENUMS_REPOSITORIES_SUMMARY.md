# KTC Logistics System - Entities, Enums & Repositories

## Overview
This document provides a complete overview of the Java entities, enums, and repositories created based on the MySQL database schema for the KTC Logistics and Delivery Management System.

## 📋 Enums Created

### 1. VehicleType
- `TRUCK` - Xe tải
- `VAN` - Xe van  
- `MOTORCYCLE` - Xe máy
- `CAR` - Ô tô

### 2. AddressType
- `DELIVERY` - Giao hàng
- `PICKUP` - Lấy hàng
- `RETURN` - Trả hàng

### 3. ProofType
- `PHOTO` - Ảnh
- `SIGNATURE` - Chữ ký
- `AUDIO` - Ghi âm
- `VIDEO` - Video

### 4. TransactionType
- `IN` - Nhập kho
- `OUT` - Xuất kho
- `TRANSFER` - Chuyển kho

### 5. ActionType
- `CREATE` - Tạo mới
- `UPDATE` - Cập nhật
- `DELETE` - Xóa
- `LOGIN` - Đăng nhập
- `LOGOUT` - Đăng xuất
- `VIEW` - Xem
- `EXPORT` - Xuất dữ liệu
- `IMPORT` - Nhập dữ liệu

### 6. PaymentMethod
- `CASH` - Tiền mặt
- `CARD` - Thẻ
- `BANK_TRANSFER` - Chuyển khoản
- `E_WALLET` - Ví điện tử
- `COD` - Thu hộ

### 7. TransportMode
- `ROAD` - Đường bộ
- `AIR` - Hàng không
- `SEA` - Đường biển
- `RAIL` - Đường sắt

### 8. ServiceType
- `STANDARD` - Tiêu chuẩn
- `FAST` - Nhanh
- `PRIORITY` - Ưu tiên
- `EXPRESS` - Hỏa tốc

### 9. DeliveryStatus
- `PENDING` - Chờ xử lý
- `ASSIGNED` - Đã phân công
- `IN_TRANSIT` - Đang vận chuyển
- `OUT_FOR_DELIVERY` - Đang giao hàng
- `DELIVERED` - Đã giao hàng
- `FAILED` - Giao hàng thất bại
- `RETURNED` - Đã trả hàng
- `CANCELLED` - Đã hủy

## 🏗️ Entities Created

### Core System Entities
1. **Status** - Quản lý trạng thái hệ thống
2. **Role** - Vai trò người dùng
3. **User** - Người dùng hệ thống

### Product & Inventory Management
4. **Category** - Danh mục sản phẩm
5. **Product** - Sản phẩm
6. **Warehouse** - Kho bãi
7. **WarehouseTransaction** - Giao dịch kho bãi

### Store & Order Management
8. **Store** - Cửa hàng
9. **Order** - Đơn hàng
10. **OrderItem** - Chi tiết đơn hàng
11. **Address** - Địa chỉ giao/lấy hàng

### Logistics & Delivery
12. **Vehicle** - Phương tiện vận chuyển
13. **Route** - Tuyến đường
14. **Delivery** - Giao hàng
15. **DeliveryTracking** - Theo dõi GPS
16. **DeliveryProof** - Chứng chỉ giao hàng

### Payment & Financial
17. **Payment** - Thanh toán

### Audit & Logging
18. **ActivityLog** - Nhật ký hoạt động

## 🗃️ Repositories Created

Each entity has a corresponding repository with comprehensive query methods:

### Key Repository Features:
- **Basic CRUD operations** (inherited from JpaRepository)
- **Custom finder methods** for business logic
- **Complex queries** using @Query annotation
- **Statistical methods** for reporting
- **Performance-optimized queries**

### Example Repository Methods:

#### UserRepository
- `findByUsername(String username)`
- `findByEmail(String email)`
- `findActiveDrivers()`
- `countByRoleName(String roleName)`

#### OrderRepository
- `findByStatusNameOrderByCreatedAtDesc(String statusName)`
- `findOrdersBetweenDates(LocalDateTime start, LocalDateTime end)`
- `getTotalRevenueInPeriod(LocalDateTime start, LocalDateTime end)`
- `countTodayOrders()`

#### VehicleRepository
- `findAvailableVehicles()`
- `findAvailableVehiclesByType(VehicleType type)`
- `countActiveVehicles()`

#### DeliveryRepository
- `findOverdueDeliveries(LocalDateTime currentTime)`
- `findAtRiskDeliveries()`
- `findActiveDeliveriesByDriver(Long driverId)`

## 🔗 Entity Relationships

### Key Relationships Implemented:
- **User ↔ Role** (ManyToOne)
- **User ↔ Status** (ManyToOne)
- **Vehicle ↔ User** (current driver)
- **Product ↔ Category** (ManyToOne)
- **Product ↔ Warehouse** (ManyToOne)
- **Order ↔ Store** (ManyToOne)
- **Order ↔ User** (created by)
- **OrderItem ↔ Order** (ManyToOne)
- **OrderItem ↔ Product** (ManyToOne)
- **Delivery ↔ Order** (ManyToOne)
- **Delivery ↔ Vehicle** (ManyToOne)
- **Delivery ↔ User** (driver)
- **Delivery ↔ Route** (ManyToOne)
- **Address ↔ Order** (ManyToOne)
- **Payment ↔ Order** (ManyToOne)

## 🚀 Usage Examples

### Creating a New Order:
```java
@Autowired
private OrderRepository orderRepository;
@Autowired  
private UserRepository userRepository;

// Create new order
Order order = new Order();
order.setOrderId("ORD-2025-001");
order.setDescription("Sample order");
order.setCreatedBy(userRepository.findById(1L).orElse(null));
orderRepository.save(order);
```

### Finding Available Vehicles:
```java
@Autowired
private VehicleRepository vehicleRepository;

// Find available trucks
List<Vehicle> availableTrucks = vehicleRepository
    .findAvailableVehiclesByType(VehicleType.TRUCK);
```

### Tracking Deliveries:
```java
@Autowired
private DeliveryRepository deliveryRepository;

// Find overdue deliveries
List<Delivery> overdueDeliveries = deliveryRepository
    .findOverdueDeliveries(LocalDateTime.now());
```

## 📊 Business Intelligence Queries

The repositories include many statistical and reporting methods:

- **Revenue tracking** - Total revenue by period
- **Performance metrics** - Delivery success rates
- **Inventory management** - Stock levels and transactions
- **User analytics** - User activity and role distribution
- **Vehicle utilization** - Fleet usage statistics

## 🔒 Security & Audit

The system includes comprehensive audit logging through:
- **ActivityLog entity** - Tracks all user actions
- **Created/Updated timestamps** - On all entities
- **User tracking** - Who created/modified records
- **Status management** - Centralized status handling

## 📈 Performance Optimizations

- **Database indexes** defined in SQL schema
- **Lazy loading** for related entities
- **Optimized queries** with proper JOINs
- **Pagination support** through JpaRepository
- **Caching-ready** repository design

This comprehensive structure provides a solid foundation for the KTC Logistics and Delivery Management System with full CRUD operations, business logic support, and reporting capabilities.
