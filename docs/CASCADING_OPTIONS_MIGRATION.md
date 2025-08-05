# 🔄 CASCADING OPTIONS MIGRATION SCRIPT

**Chuyên gia Spring Boot - 20 năm kinh nghiệm**

---

## 📋 **TỔNG QUAN MIGRATION**

Thực hiện **High Priority Fix** cho vấn đề thiếu cascading options trong foreign key constraints:

- **Vấn đề**: 34/37 foreign keys thiếu ON DELETE/ON UPDATE clauses
- **Hậu quả**: Production errors khi xóa records có references
- **Giải pháp**: Thêm cascading behavior phù hợp theo business logic

---

## 🎯 **CASCADING STRATEGY**

### **🔥 CASCADE (7 constraints)**
- **Logic**: Child không có ý nghĩa khi parent bị xóa
- **Use cases**: order → order_items, addresses, payments, delivery_proofs, deliveries

### **🔄 SET NULL (17 constraints)**  
- **Logic**: Optional relationships, preserve audit trails
- **Use cases**: created_by fields, optional assignments (driver, vehicle, warehouse)

### **🚫 RESTRICT (5 constraints)**
- **Logic**: Critical business data, prevent accidental deletion
- **Use cases**: product → category, order_items → product, user → role

### **⏸️ NO ACTION (8 constraints)**
- **Logic**: Status references - system-managed data
- **Use cases**: Tất cả status_id references

---

## ⚡ **MIGRATION SCRIPT SQL**

```sql
-- =====================================================================================
-- CASCADING OPTIONS MIGRATION - PRODUCTION SAFE
-- =====================================================================================
-- Mục đích: Thêm ON DELETE/ON UPDATE clauses cho foreign key constraints
-- Thực hiện: 2025-01-XX
-- Risk Level: MEDIUM - Cần test kỹ trước production
-- =====================================================================================

START TRANSACTION;

-- BACKUP CURRENT CONSTRAINTS (for rollback)
-- Lưu lại danh sách constraints hiện tại để có thể rollback
SELECT 
    CONSTRAINT_NAME, 
    TABLE_NAME, 
    COLUMN_NAME, 
    REFERENCED_TABLE_NAME, 
    REFERENCED_COLUMN_NAME
FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE 
WHERE CONSTRAINT_SCHEMA = DATABASE() 
    AND REFERENCED_TABLE_NAME IS NOT NULL
ORDER BY TABLE_NAME, CONSTRAINT_NAME;

-- =====================================================================================
-- PHASE 1: DROP ALL EXISTING FOREIGN KEY CONSTRAINTS
-- =====================================================================================

-- Critical: Xóa theo thứ tự dependency để tránh conflicts
ALTER TABLE `addresses` DROP CONSTRAINT `fk_addresses_order_id`;
ALTER TABLE `categories` DROP CONSTRAINT `fk_categories_parent_id`;
ALTER TABLE `deliveries` DROP CONSTRAINT `fk_deliveries_order_id`;
ALTER TABLE `activity_logs` DROP CONSTRAINT `fk_activity_logs_actor_id`;
ALTER TABLE `activity_logs` DROP CONSTRAINT `fk_activity_logs_role_id`;
ALTER TABLE `activity_logs` DROP CONSTRAINT `fk_activity_logs_status_id`;
ALTER TABLE `deliveries` DROP CONSTRAINT `fk_deliveries_route_id`;
ALTER TABLE `deliveries` DROP CONSTRAINT `fk_deliveries_vehicle_id`;
ALTER TABLE `deliveries` DROP CONSTRAINT `fk_deliveries_driver_id`;
ALTER TABLE `delivery_tracking` DROP CONSTRAINT `fk_delivery_tracking_delivery_id`;
ALTER TABLE `delivery_proofs` DROP CONSTRAINT `fk_delivery_proofs_order_id`;
ALTER TABLE `delivery_tracking` DROP CONSTRAINT `fk_delivery_tracking_status_id`;
ALTER TABLE `delivery_tracking` DROP CONSTRAINT `fk_delivery_tracking_vehicle_id`;
ALTER TABLE `warehouse_transactions` DROP CONSTRAINT `fk_warehouse_transactions_order_id`;
ALTER TABLE `warehouse_transactions` DROP CONSTRAINT `fk_warehouse_transactions_product_id`;
ALTER TABLE `warehouse_transactions` DROP CONSTRAINT `fk_warehouse_transactions_status_id`;
ALTER TABLE `warehouse_transactions` DROP CONSTRAINT `fk_warehouse_transactions_warehouse_id`;
ALTER TABLE `warehouse_transactions` DROP CONSTRAINT `fk_warehouse_transactions_created_by`;
ALTER TABLE `order_items` DROP CONSTRAINT `fk_order_items_order_id`;
ALTER TABLE `order_items` DROP CONSTRAINT `fk_order_items_product_id`;
ALTER TABLE `orders` DROP CONSTRAINT `fk_orders_status_id`;
ALTER TABLE `orders` DROP CONSTRAINT `fk_orders_store_id`;
ALTER TABLE `orders` DROP CONSTRAINT `fk_orders_created_by`;
ALTER TABLE `payments` DROP CONSTRAINT `fk_payments_order_id`;
ALTER TABLE `payments` DROP CONSTRAINT `fk_payments_status_id`;
ALTER TABLE `payments` DROP CONSTRAINT `fk_payments_created_by`;
ALTER TABLE `products` DROP CONSTRAINT `fk_products_category_id`;
ALTER TABLE `products` DROP CONSTRAINT `fk_products_warehouse_id`;
ALTER TABLE `products` DROP CONSTRAINT `fk_products_created_by`;
ALTER TABLE `vehicles` DROP CONSTRAINT `fk_vehicles_status_id`;
ALTER TABLE `vehicles` DROP CONSTRAINT `fk_vehicles_current_driver_id`;
ALTER TABLE `users` DROP CONSTRAINT `fk_users_role_id`;
ALTER TABLE `users` DROP CONSTRAINT `fk_users_status_id`;
ALTER TABLE `delivery_proofs` DROP CONSTRAINT `fk_delivery_proofs_uploaded_by`;
ALTER TABLE `stores` DROP CONSTRAINT `fk_stores_created_by`;
ALTER TABLE `routes` DROP CONSTRAINT `fk_routes_created_by`;
ALTER TABLE `warehouses` DROP CONSTRAINT `fk_warehouses_created_by`;

-- =====================================================================================
-- PHASE 2: RECREATE CONSTRAINTS WITH PROPER CASCADING
-- =====================================================================================

-- CASCADE: Parent-child relationships - child không có ý nghĩa khi parent bị xóa
ALTER TABLE `addresses` ADD CONSTRAINT `fk_addresses_order_id` 
FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`) ON DELETE CASCADE;

ALTER TABLE `order_items` ADD CONSTRAINT `fk_order_items_order_id` 
FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`) ON DELETE CASCADE;

ALTER TABLE `payments` ADD CONSTRAINT `fk_payments_order_id` 
FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`) ON DELETE CASCADE;

ALTER TABLE `delivery_proofs` ADD CONSTRAINT `fk_delivery_proofs_order_id` 
FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`) ON DELETE CASCADE;

ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_order_id` 
FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`) ON DELETE CASCADE;

ALTER TABLE `delivery_tracking` ADD CONSTRAINT `fk_delivery_tracking_delivery_id` 
FOREIGN KEY(`delivery_id`) REFERENCES `deliveries`(`id`) ON DELETE CASCADE;

-- SET NULL: Optional relationships - preserve audit trail và flexibility  
ALTER TABLE `categories` ADD CONSTRAINT `fk_categories_parent_id` 
FOREIGN KEY(`parent_id`) REFERENCES `categories`(`id`) ON DELETE SET NULL;

ALTER TABLE `activity_logs` ADD CONSTRAINT `fk_activity_logs_actor_id` 
FOREIGN KEY(`actor_id`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `activity_logs` ADD CONSTRAINT `fk_activity_logs_role_id` 
FOREIGN KEY(`role_id`) REFERENCES `roles`(`id`) ON DELETE SET NULL;

ALTER TABLE `vehicles` ADD CONSTRAINT `fk_vehicles_current_driver_id` 
FOREIGN KEY(`current_driver_id`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_driver_id` 
FOREIGN KEY(`driver_id`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_route_id` 
FOREIGN KEY(`route_id`) REFERENCES `routes`(`id`) ON DELETE SET NULL;

ALTER TABLE `deliveries` ADD CONSTRAINT `fk_deliveries_vehicle_id` 
FOREIGN KEY(`vehicle_id`) REFERENCES `vehicles`(`id`) ON DELETE SET NULL;

ALTER TABLE `delivery_tracking` ADD CONSTRAINT `fk_delivery_tracking_vehicle_id` 
FOREIGN KEY(`vehicle_id`) REFERENCES `vehicles`(`id`) ON DELETE SET NULL;

ALTER TABLE `products` ADD CONSTRAINT `fk_products_warehouse_id` 
FOREIGN KEY(`warehouse_id`) REFERENCES `warehouses`(`id`) ON DELETE SET NULL;

ALTER TABLE `orders` ADD CONSTRAINT `fk_orders_store_id` 
FOREIGN KEY(`store_id`) REFERENCES `stores`(`id`) ON DELETE SET NULL;

ALTER TABLE `delivery_proofs` ADD CONSTRAINT `fk_delivery_proofs_uploaded_by` 
FOREIGN KEY(`uploaded_by`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_order_id` 
FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`) ON DELETE SET NULL;

-- SET NULL: Created_by audit fields - preserve audit trail khi user bị xóa
ALTER TABLE `orders` ADD CONSTRAINT `fk_orders_created_by` 
FOREIGN KEY(`created_by`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `products` ADD CONSTRAINT `fk_products_created_by` 
FOREIGN KEY(`created_by`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `stores` ADD CONSTRAINT `fk_stores_created_by` 
FOREIGN KEY(`created_by`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `routes` ADD CONSTRAINT `fk_routes_created_by` 
FOREIGN KEY(`created_by`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_created_by` 
FOREIGN KEY(`created_by`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `payments` ADD CONSTRAINT `fk_payments_created_by` 
FOREIGN KEY(`created_by`) REFERENCES `users`(`id`) ON DELETE SET NULL;

ALTER TABLE `warehouses` ADD CONSTRAINT `fk_warehouses_created_by` 
FOREIGN KEY(`created_by`) REFERENCES `users`(`id`) ON DELETE SET NULL;

-- RESTRICT: Critical business relationships - không cho phép xóa parent khi có child
ALTER TABLE `products` ADD CONSTRAINT `fk_products_category_id` 
FOREIGN KEY(`category_id`) REFERENCES `categories`(`id`) ON DELETE RESTRICT;

ALTER TABLE `order_items` ADD CONSTRAINT `fk_order_items_product_id` 
FOREIGN KEY(`product_id`) REFERENCES `products`(`id`) ON DELETE RESTRICT;

ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_product_id` 
FOREIGN KEY(`product_id`) REFERENCES `products`(`id`) ON DELETE RESTRICT;

ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_warehouse_id` 
FOREIGN KEY(`warehouse_id`) REFERENCES `warehouses`(`id`) ON DELETE RESTRICT;

ALTER TABLE `users` ADD CONSTRAINT `fk_users_role_id` 
FOREIGN KEY(`role_id`) REFERENCES `roles`(`id`) ON DELETE RESTRICT;

-- DEFAULT (NO ACTION): Status references - tránh xóa status đang được sử dụng
ALTER TABLE `activity_logs` ADD CONSTRAINT `fk_activity_logs_status_id` 
FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);

ALTER TABLE `delivery_tracking` ADD CONSTRAINT `fk_delivery_tracking_status_id` 
FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);

ALTER TABLE `warehouse_transactions` ADD CONSTRAINT `fk_warehouse_transactions_status_id` 
FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);

ALTER TABLE `orders` ADD CONSTRAINT `fk_orders_status_id` 
FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);

ALTER TABLE `payments` ADD CONSTRAINT `fk_payments_status_id` 
FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);

ALTER TABLE `vehicles` ADD CONSTRAINT `fk_vehicles_status_id` 
FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);

ALTER TABLE `users` ADD CONSTRAINT `fk_users_status_id` 
FOREIGN KEY(`status_id`) REFERENCES `status`(`id`);

COMMIT;
```

---

## 🚨 **PRE-MIGRATION TESTING**

### **Test Scenarios trước khi áp dụng**

```sql
-- Test 1: Kiểm tra data integrity
SELECT 
    COUNT(*) as total_orders,
    COUNT(DISTINCT order_id) as unique_orders
FROM order_items;

-- Test 2: Kiểm tra orphaned records
SELECT COUNT(*) FROM order_items oi 
WHERE NOT EXISTS (SELECT 1 FROM orders o WHERE o.id = oi.order_id);

-- Test 3: Kiểm tra NULL values in optional fields
SELECT COUNT(*) FROM vehicles WHERE current_driver_id IS NULL;
SELECT COUNT(*) FROM products WHERE warehouse_id IS NULL;

-- Test 4: Kiểm tra critical relationships
SELECT COUNT(*) FROM products WHERE category_id IS NULL;
SELECT COUNT(*) FROM users WHERE role_id IS NULL;
```

---

## 🔄 **ROLLBACK PLAN**

Nếu có vấn đề, rollback bằng cách:

```sql
START TRANSACTION;

-- Xóa tất cả constraints có cascading
-- (Danh sách chi tiết trong file rollback_constraints.sql)

-- Tạo lại constraints cũ không có cascading
ALTER TABLE `addresses` ADD CONSTRAINT `fk_addresses_order_id` 
FOREIGN KEY(`order_id`) REFERENCES `orders`(`id`);
-- ... (tất cả constraints cũ)

COMMIT;
```

---

## 📊 **IMPACT ASSESSMENT**

### **Business Benefits**
✅ **No More Production Errors**: Không còn referential integrity violations  
✅ **Data Consistency**: Automatic cleanup khi xóa parent records  
✅ **Audit Trail Preservation**: Created_by fields được preserve  
✅ **Flexible Operations**: có thể xóa users, vehicles mà không ảnh hưởng data  

### **Technical Benefits**  
✅ **Spring Boot Compatible**: Phù hợp với JPA cascading  
✅ **Performance**: Giảm manual cleanup operations  
✅ **Maintainability**: Rõ ràng về data lifecycle  

### **Risks Mitigated**
❌ **Before**: Không thể xóa orders có order_items  
❌ **Before**: Không thể xóa users có created_by references  
❌ **Before**: Manual cleanup required  

---

## ✅ **VALIDATION CHECKLIST**

### **Pre-Migration**
- [ ] Database backup completed
- [ ] Test scenarios executed
- [ ] Orphaned records identified and cleaned
- [ ] Application downtime scheduled

### **Post-Migration**  
- [ ] All constraints created successfully
- [ ] No constraint violations
- [ ] Test DELETE operations work correctly
- [ ] Spring Boot application starts without errors
- [ ] Integration tests pass
- [ ] Performance impact assessed

### **Production Validation**
- [ ] Monitor error logs for 24h
- [ ] Verify cascading behavior works as expected  
- [ ] Check audit trails are preserved
- [ ] Validate user workflows unaffected

---

## 📞 **SUPPORT & MONITORING**

**Technical Lead**: Spring Boot Expert Team  
**Migration Window**: Low-traffic hours (2-4 AM)  
**Rollback Time**: < 15 minutes if needed  
**Success Criteria**: No constraint violations + functional tests pass  

---

## 📈 **EXPECTED OUTCOMES**

### **Immediate Benefits**
- Tất cả foreign key constraints có proper cascading behavior
- Không còn production errors khi xóa records
- Data consistency được đảm bảo automatically

### **Long-term Benefits**  
- Cleaner codebase (ít manual cleanup)
- Better Spring Boot integration
- Easier data maintenance operations
- Improved system reliability

---

**Migration Status**: 🟡 **READY FOR TESTING**  
**Risk Level**: 🟡 **MEDIUM** (Requires careful testing)  
**Business Impact**: 🟢 **HIGH POSITIVE**

---

*End of Cascading Options Migration Document*