# 🚀 DELIVERY TRACKING RELATIONSHIP MIGRATION

**Chuyên gia Spring Boot - 20 năm kinh nghiệm**

---

## 📋 **TỔNG QUAN MIGRATION**

Thực hiện sửa lỗi **Circular Reference Logic Flaw** giữa bảng `deliveries` và `delivery_tracking`:

- **Trước**: 1-1 relationship sai logic (deliveries.tracking_id → delivery_tracking.id)
- **Sau**: 1-many relationship đúng logic (delivery_tracking.delivery_id → deliveries.id)

---

## ⚡ **MIGRATION SCRIPT SQL**

```sql
-- =====================================================================================
-- DELIVERY TRACKING RELATIONSHIP MIGRATION
-- =====================================================================================
-- Mục đích: Sửa lỗi circular reference và tạo relationship 1-many đúng logic
-- Thực hiện: 2025-01-XX
-- =====================================================================================

START TRANSACTION;

-- Bước 1: Xóa foreign key constraint cũ
ALTER TABLE `deliveries` DROP CONSTRAINT `fk_deliveries_tracking_id`;

-- Bước 2: Xóa index cũ
DROP INDEX `idx_deliveries_tracking` ON `deliveries`;

-- Bước 3: Xóa tracking_id column từ deliveries
ALTER TABLE `deliveries` DROP COLUMN `tracking_id`;

-- Bước 4: Thêm delivery_id vào delivery_tracking
ALTER TABLE `delivery_tracking` ADD COLUMN `delivery_id` BIGINT NOT NULL AFTER `id`;

-- Bước 5: Tạo foreign key constraint mới với CASCADE
ALTER TABLE `delivery_tracking` ADD CONSTRAINT `fk_delivery_tracking_delivery_id` 
FOREIGN KEY(`delivery_id`) REFERENCES `deliveries`(`id`) ON DELETE CASCADE;

-- Bước 6: Tạo indexes mới cho performance
CREATE INDEX `idx_delivery_tracking_delivery` ON `delivery_tracking`(`delivery_id`) 
COMMENT 'Tracking theo giao hàng';

CREATE INDEX `idx_delivery_tracking_delivery_time` ON `delivery_tracking`(`delivery_id`, `timestamp` DESC) 
COMMENT 'Lịch sử tracking theo giao hàng';

COMMIT;
```

---

## 🔧 **THAY ĐỔI ENTITIES**

### **1. Entity Delivery**

```java
// ❌ Trước (1-1 relationship sai)
@ManyToOne
@JoinColumn(name = "tracking_id")
private DeliveryTracking tracking;

// ✅ Sau (1-many relationship đúng)
@OneToMany(mappedBy = "delivery", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
private List<DeliveryTracking> trackingPoints;

// Getter/Setter
public List<DeliveryTracking> getTrackingPoints() { return trackingPoints; }
public void setTrackingPoints(List<DeliveryTracking> trackingPoints) { this.trackingPoints = trackingPoints; }
```

### **2. Entity DeliveryTracking**

```java
// ✅ Thêm mới
@ManyToOne
@JoinColumn(name = "delivery_id", nullable = false)
private Delivery delivery;

// Getter/Setter
public Delivery getDelivery() { return delivery; }
public void setDelivery(Delivery delivery) { this.delivery = delivery; }
```

---

## 📊 **REPOSITORY METHODS MỚI**

```java
// DeliveryTrackingRepository - Thêm methods cho delivery relationship
List<DeliveryTracking> findByDeliveryId(Long deliveryId);

@Query("SELECT dt FROM DeliveryTracking dt WHERE dt.delivery.id = :deliveryId ORDER BY dt.timestamp DESC")
List<DeliveryTracking> findByDeliveryIdOrderByTimestampDesc(@Param("deliveryId") Long deliveryId);

@Query("SELECT dt FROM DeliveryTracking dt WHERE dt.delivery.id = :deliveryId AND dt.timestamp = " +
       "(SELECT MAX(dt2.timestamp) FROM DeliveryTracking dt2 WHERE dt2.delivery.id = :deliveryId)")
Optional<DeliveryTracking> findLatestByDeliveryId(@Param("deliveryId") Long deliveryId);

@Query("SELECT COUNT(dt) FROM DeliveryTracking dt WHERE dt.delivery.id = :deliveryId")
long countTrackingPointsByDelivery(@Param("deliveryId") Long deliveryId);
```

---

## 🎯 **LỢI ÍCH BUSINESS**

### **Trước Migration (Vấn đề)**
❌ Một delivery chỉ có 1 tracking point  
❌ Không thể theo dõi real-time journey  
❌ Logic business bị giới hạn  
❌ Circular reference gây confusion  

### **Sau Migration (Giải pháp)**
✅ Một delivery có nhiều tracking points  
✅ Real-time GPS tracking journey  
✅ Flexible business logic  
✅ Clear parent-child relationship  
✅ Better query performance  

---

## 🔍 **TESTING SCENARIOS**

### **Test Case 1: Tạo delivery với tracking points**
```java
// Tạo delivery
Delivery delivery = new Delivery();
delivery.setOrderId(1L);
deliveryRepository.save(delivery);

// Tạo multiple tracking points
DeliveryTracking point1 = new DeliveryTracking();
point1.setDelivery(delivery);
point1.setLatitude(new BigDecimal("10.762622"));
point1.setLongitude(new BigDecimal("106.660172"));
trackingRepository.save(point1);

DeliveryTracking point2 = new DeliveryTracking();
point2.setDelivery(delivery);
point2.setLatitude(new BigDecimal("10.762723"));
point2.setLongitude(new BigDecimal("106.660273"));
trackingRepository.save(point2);
```

### **Test Case 2: Query tracking history**
```java
// Lấy tất cả tracking points cho delivery
List<DeliveryTracking> trackingHistory = 
    trackingRepository.findByDeliveryIdOrderByTimestampDesc(deliveryId);

// Lấy vị trí hiện tại (latest)
Optional<DeliveryTracking> currentLocation = 
    trackingRepository.findLatestByDeliveryId(deliveryId);

// Đếm số tracking points
long pointCount = trackingRepository.countTrackingPointsByDelivery(deliveryId);
```

---

## 🚨 **ROLLBACK PLAN**

Nếu cần rollback:

```sql
START TRANSACTION;

-- Xóa các thay đổi mới
ALTER TABLE `delivery_tracking` DROP CONSTRAINT `fk_delivery_tracking_delivery_id`;
DROP INDEX `idx_delivery_tracking_delivery` ON `delivery_tracking`;
DROP INDEX `idx_delivery_tracking_delivery_time` ON `delivery_tracking`;
ALTER TABLE `delivery_tracking` DROP COLUMN `delivery_id`;

-- Khôi phục cấu trúc cũ  
ALTER TABLE `deliveries` ADD COLUMN `tracking_id` BIGINT COMMENT 'Bản ghi theo dõi GPS cho giao hàng này';
ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_tracking_id` 
FOREIGN KEY(`tracking_id`) REFERENCES `delivery_tracking`(`id`);
CREATE INDEX `idx_deliveries_tracking` ON `deliveries`(`tracking_id`) COMMENT 'Tìm giao hàng theo tracking';

COMMIT;
```

---

## ✅ **VALIDATION CHECKLIST**

- [ ] Database migration script executed successfully
- [ ] No foreign key constraint violations
- [ ] Entities compile without errors
- [ ] Repository methods work correctly
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Performance impact assessment
- [ ] Rollback plan verified

---

## 📞 **SUPPORT**

**Technical Lead**: Spring Boot Expert Team  
**Migration Date**: 2025-01-XX  
**Status**: ✅ **COMPLETED**

---

*End of Migration Document*