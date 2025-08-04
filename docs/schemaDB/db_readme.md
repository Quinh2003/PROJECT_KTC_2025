# KTC Logistics & Delivery Management System Database

## Tổng quan / Overview

Cơ sở dữ liệu quản lý toàn diện hệ thống logistics và giao hàng cho KTC Project 2025. Hệ thống này được thiết kế để quản lý đầy đủ quy trình từ đặt hàng đến giao hàng, bao gồm quản lý phương tiện, tài xế, kho bãi, và theo dõi GPS thời gian thực.

*Comprehensive database for logistics and delivery management system for KTC Project 2025. This system is designed to manage the complete process from order placement to delivery, including vehicle management, drivers, warehouses, and real-time GPS tracking.*

## Thông tin kỹ thuật / Technical Information

- **Database Engine**: MySQL 8.0+
- **Character Set**: UTF8MB4
- **Version**: 9.4
- **Date Created**: 2025
- **Schema File**: `database-8-4.sql`

## Cấu trúc hệ thống / System Architecture

### 🚚 Core Logistics Tables

| Bảng / Table | Mô tả / Description |
|--------------|---------------------|
| `vehicles` | Quản lý phương tiện vận chuyển (xe tải, xe van, xe máy) |
| `deliveries` | Quản lý giao hàng và phân công tài xế, phương tiện |
| `delivery_tracking` | Theo dõi GPS và trạng thái giao hàng thời gian thực |
| `delivery_proofs` | Chứng từ giao hàng (ảnh, chữ ký, ghi âm) |
| `routes` | Quản lý tuyến đường tối ưu cho giao hàng |

### 📦 Order & Product Management

| Bảng / Table | Mô tả / Description |
|--------------|---------------------|
| `orders` | Đơn hàng chính với thông tin khách hàng và tổng tiền |
| `order_items` | Chi tiết từng sản phẩm trong đơn hàng |
| `products` | Catalog sản phẩm với thông tin trọng lượng, thể tích |
| `categories` | Phân loại sản phẩm theo danh mục |
| `addresses` | Địa chỉ giao hàng và lấy hàng với tọa độ GPS |

### 🏪 Infrastructure Management

| Bảng / Table | Mô tả / Description |
|--------------|---------------------|
| `warehouses` | Quản lý kho bãi với sức chứa và vị trí |
| `warehouse_transactions` | Giao dịch nhập/xuất kho chi tiết |
| `stores` | Cửa hàng và điểm bán hàng |
| `payments` | Thanh toán với nhiều phương thức |

### 👥 User & System Management

| Bảng / Table | Mô tả / Description |
|--------------|---------------------|
| `users` | Người dùng hệ thống (admin, điều phối, tài xế) |
| `roles` | Vai trò và phân quyền với JSON permissions |
| `status` | Trạng thái hệ thống cho các đối tượng |
| `activity_logs` | Nhật ký hoạt động và audit trail |

## Mối quan hệ chính / Key Relationships

```
orders (1) ←→ (n) order_items ←→ (1) products
   ↓                                    ↓
deliveries ←→ vehicles ←→ users      categories
   ↓              ↓         ↓
delivery_tracking routes  roles
   ↓
delivery_proofs
```

## Tính năng nổi bật / Key Features

### 🎯 Logistics Core Features
- **Theo dõi GPS thời gian thực**: Tracking vị trí phương tiện và trạng thái giao hàng
- **Quản lý bằng chứng giao hàng**: Ảnh, chữ ký số, và ghi âm xác nhận
- **Tối ưu hóa tuyến đường**: Lưu trữ waypoints và tính toán chi phí
- **Quản lý đội xe**: Theo dõi sức chứa, trạng thái, và phân công tài xế

### 📊 Business Intelligence
- **Tính toán lợi nhuận**: Theo dõi biên lãi từng đơn hàng
- **Phân tích rủi ro giao hàng**: Cảnh báo giao hàng trễ
- **Báo cáo hoạt động**: Audit trail đầy đủ cho mọi thao tác

### 🔐 Security & Access Control
- **Phân quyền chi tiết**: JSON-based permissions cho từng vai trò
- **Audit trail**: Ghi lại mọi thay đổi với metadata
- **Data validation**: Constraints đảm bảo tính toàn vẹn dữ liệu

## Cài đặt / Installation

### Prerequisites
- MySQL Server 8.0 hoặc cao hơn
- Client hỗ trợ UTF8MB4 encoding

### Bước cài đặt / Installation Steps

1. **Tạo database**:
```sql
CREATE DATABASE ktc_logistics_db 
CHARACTER SET utf8mb4 
COLLATE utf8mb4_unicode_ci;
```

2. **Chạy schema**:
```bash
mysql -u username -p ktc_logistics_db < database-8-4.sql
```

3. **Kiểm tra cài đặt**:
```sql
USE ktc_logistics_db;
SHOW TABLES;
-- Kết quả sẽ hiển thị 16 tables
```

## Indexes quan trọng / Important Indexes

### High-Performance Queries
- **Orders**: `idx_orders_status_created` - Tìm đơn hàng theo trạng thái và thời gian
- **Deliveries**: `idx_deliveries_vehicle_time` - Tracking theo phương tiện
- **GPS Tracking**: `idx_delivery_tracking_vehicle_time` - Real-time location
- **Products**: `idx_products_category` - Catalog browsing

### Business Analytics
- **Warehouse**: `idx_warehouse_trans_product_date` - Lịch sử inventory
- **Payments**: `idx_payments_order` - Financial tracking
- **Activity**: `idx_activity_logs_action_time` - Audit queries

## Validation Rules / Ràng buộc

### Business Logic Constraints
- **Vehicle capacity**: Trọng tải và thể tích >= 0
- **Product pricing**: Unit price >= 0
- **Order quantities**: Quantity > 0 for all items
- **Payment amounts**: Amount > 0
- **Delivery attempts**: >= 0

### Data Integrity
- **Cascading deletes**: Tự động xóa related records
- **Foreign key enforcement**: Đảm bảo tham chiếu hợp lệ
- **Unique constraints**: Ngăn duplicate critical data

## Sử dụng / Usage Examples

### Tạo đơn hàng mới
```sql
-- 1. Tạo đơn hàng
INSERT INTO orders (order_id, status_id, total_amount, created_by) 
VALUES ('ORD-2025-001', 1, 150000.00, 1);

-- 2. Thêm sản phẩm vào đơn hàng
INSERT INTO order_items (order_id, product_id, quantity, unit_price) 
VALUES (LAST_INSERT_ID(), 1, 2, 75000.00);

-- 3. Tạo địa chỉ giao hàng
INSERT INTO addresses (order_id, address_type, address, latitude, longitude) 
VALUES (LAST_INSERT_ID(), 'DELIVERY', '123 Nguyen Van A, Q1, HCM', 10.762622, 106.660172);
```

### Phân công giao hàng
```sql
INSERT INTO deliveries (
    order_id, vehicle_id, driver_id, 
    schedule_delivery_time, delivery_status
) VALUES (
    1, 1, 5, 
    '2025-01-15 14:00:00', 'ASSIGNED'
);
```

### Tracking GPS
```sql
INSERT INTO delivery_tracking (
    vehicle_id, status_id, latitude, longitude, location
) VALUES (
    1, 3, 10.762622, 106.660172, 'Đang trên đường đến khách hàng'
);
```

## Monitoring & Maintenance

### Performance Monitoring
```sql
-- Kiểm tra slow queries
SELECT * FROM mysql.slow_log WHERE query_time > 1;

-- Monitor index usage
SHOW INDEX FROM orders;
```

### Regular Maintenance
```sql
-- Optimize tables quarterly
OPTIMIZE TABLE orders, deliveries, delivery_tracking;

-- Clean old tracking data (> 6 months)
DELETE FROM delivery_tracking 
WHERE timestamp < DATE_SUB(NOW(), INTERVAL 6 MONTH);
```

## Backup Strategy

### Daily Backups
```bash
mysqldump --single-transaction --routines --triggers \
  ktc_logistics_db > backup_$(date +%Y%m%d).sql
```

### Critical Tables (Hourly)
```bash
mysqldump ktc_logistics_db orders deliveries delivery_tracking \
  > critical_backup_$(date +%Y%m%d_%H).sql
```

## Security Considerations

### User Privileges
```sql
-- Tạo user chỉ đọc cho reporting
CREATE USER 'reporter'@'%' IDENTIFIED BY 'secure_password';
GRANT SELECT ON ktc_logistics_db.* TO 'reporter'@'%';

-- Tạo user ứng dụng với quyền hạn chế
CREATE USER 'app_user'@'%' IDENTIFIED BY 'app_password';
GRANT SELECT, INSERT, UPDATE ON ktc_logistics_db.* TO 'app_user'@'%';
REVOKE DELETE ON ktc_logistics_db.* FROM 'app_user'@'%';
```

### Data Protection
- Mã hóa passwords trong bảng users
- Backup encrypted
- Audit trail cho sensitive operations
- Regular security updates

## Troubleshooting

### Common Issues

1. **Foreign Key Errors**:
   - Kiểm tra order của schema execution
   - Đảm bảo parent records tồn tại trước khi insert child

2. **Performance Issues**:
   - Kiểm tra missing indexes
   - Analyze slow query log
   - Consider partitioning cho large tables

3. **Data Integrity**:
   - Regular CHECKSUM verification
   - Monitor constraint violations
   - Validate GPS coordinates ranges

## API Integration Notes

### JSON Fields
- `routes.waypoints`: Array of GPS coordinates
- `roles.permission`: Detailed permission object
- `activity_logs.metadata`: Change tracking data

### Recommended Practices
- Use prepared statements
- Implement connection pooling
- Cache frequently accessed data (categories, status)
- Batch GPS tracking inserts

## Contact & Support

Để được hỗ trợ về database schema này, vui lòng liên hệ team phát triển KTC Project 2025.

*For support regarding this database schema, please contact the KTC Project 2025 development team.*

---

**Last Updated**: 2025
**Version**: 9.4
**Maintainer**: KTC Development Team