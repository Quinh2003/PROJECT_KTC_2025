
package ktc.spring_project.repositories;

import ktc.spring_project.dtos.order.OrderSummaryDTO;
import ktc.spring_project.entities.Order;
// ...existing code...
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import ktc.spring_project.entities.Status;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface OrderRepository extends JpaRepository<Order, Long> {
    Optional<Order> findByOrderCode(String orderCode);
    
    // Kiểm tra trùng lặp orderCode
    boolean existsByOrderCode(String orderCode);

        List<Order> findByStatus_Id(Short statusId);
        Page<Order> findByStatus_Id(Short statusId, org.springframework.data.domain.Pageable pageable);
        @Query("SELECT o FROM Order o WHERE o.status.id <> 2")
        Page<Order> findNotCompletedOrders(Pageable pageable);
        @Query("SELECT o FROM Order o WHERE o.status.id <> 2 ORDER BY o.id DESC")
        List<Order> findAllNotCompletedOrdersSortedByIdDesc();
        List<Order> findByStore_Id(Long storeId);
        List<Order> findByCreatedBy_Id(Long createdBy);

    // Find orders by status with pagination
    Page<Order> findByStatus(Status status, Pageable pageable);

    // Query các đơn hàng có trạng thái AVAILABLE
    @Query("SELECT o FROM Order o WHERE o.status.name = 'AVAILABLE'")
    List<Order> findAvailableOrders();

    // Đếm orders theo khoảng thời gian (tối ưu cho dashboard)
    long countByCreatedAtBetween(LocalDateTime startDate, LocalDateTime endDate);

    // Query các đơn hàng theo tên trạng thái, mới nhất lên đầu
    // @Query("SELECT o FROM Order o WHERE o.status.name = :statusName ORDER BY o.createdAt DESC")
    // Đã có @Query bên dưới, không cần hàm này nữa
    // List<Order> findByStatusNameOrderByCreatedAtDesc(@Param("statusName") String statusName);

    // Query các đơn hàng theo khoảng thời gian tạo
    @Query("SELECT o FROM Order o WHERE o.createdAt BETWEEN :startDate AND :endDate ORDER BY o.createdAt DESC")
    List<Order> findOrdersBetweenDates(@Param("startDate") LocalDateTime startDate,
                        @Param("endDate") LocalDateTime endDate);

    // Query các đơn hàng theo khoảng tổng tiền
    // @Query("SELECT o FROM Order o WHERE o.totalAmount BETWEEN :minAmount AND :maxAmount")
    // Không dùng Spring Data query method cho range, đã có @Query bên trên
    // List<Order> findByTotalAmountRange(@Param("minAmount") BigDecimal minAmount,
    //                     @Param("maxAmount") BigDecimal maxAmount);

    // Đếm số đơn hàng theo tên trạng thái
    @Query("SELECT COUNT(o) FROM Order o WHERE o.status.name = :statusName")
    long countByStatusName(@Param("statusName") String statusName);

    // Tổng doanh thu các đơn đã hoàn thành trong khoảng thời gian
    @Query("SELECT SUM(o.totalAmount) FROM Order o WHERE o.status.name = 'COMPLETED' " +
        "AND o.createdAt BETWEEN :startDate AND :endDate")
    BigDecimal getTotalRevenueInPeriod(@Param("startDate") LocalDateTime startDate,
                        @Param("endDate") LocalDateTime endDate);

    // Đếm số đơn tạo hôm nay
    @Query("SELECT COUNT(o) FROM Order o WHERE DATE(o.createdAt) = CURRENT_DATE")
    long countTodayOrders();

    // Query các đơn đang xử lý (IN_PROGRESS)
    @Query("SELECT o FROM Order o WHERE o.status.name = 'IN_PROGRESS'")
    List<Order> findActiveOrders();

    // Đếm số đơn đã giao hôm nay
    @Query("SELECT COUNT(o) FROM Order o WHERE o.status.name = 'DELIVERED' AND DATE(o.updatedAt) = CURRENT_DATE")
    int countDeliveredOrdersToday();

    // Đếm số đơn đã giao hôm nay theo tài xế (join với Delivery)
    @Query("SELECT COUNT(DISTINCT o) FROM Order o " +
        "JOIN Delivery d ON d.order = o " +
        "WHERE d.driver.id = :driverId " +
        "AND o.status.name = 'DELIVERED' " +
        "AND DATE(o.updatedAt) = CURRENT_DATE")
    int countDeliveredOrdersByDriverIdToday(@Param("driverId") Long driverId);

    @Query("SELECT NEW ktc.spring_project.dtos.order.OrderSummaryDTO(" +
           "o.id, " +
           "o.store.id, " +
           "o.createdAt, " +
           "o.address.address, " +
           "(SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order = o), " +
           "d.deliveryFee, " +
           "o.status.name) " +
           "FROM Order o " +
           "LEFT JOIN Delivery d ON d.order = o " +
           "WHERE o.store.id = :storeId")
    List<OrderSummaryDTO> findOrderSummariesByStoreId(@Param("storeId") Long storeId);

    @Query("SELECT DISTINCT NEW ktc.spring_project.dtos.order.OrderSummaryDTO(" +
           "o.id, " +
           "o.store.id, " +
           "o.createdAt, " +
           "o.address.address, " +
           "(SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order = o), " +
           "(SELECT MAX(d2.deliveryFee) FROM Delivery d2 WHERE d2.order = o), " +
           "o.status.name) " +
           "FROM Order o " +
           "WHERE o.store.id = :storeId " +
           "ORDER BY o.createdAt DESC")
    Page<OrderSummaryDTO> findOrderSummariesByStoreIdPaginated(@Param("storeId") Long storeId, Pageable pageable);

    @Query("SELECT NEW ktc.spring_project.dtos.order.OrderSummaryDTO(" +
           "o.id, " +
           "o.store.id, " +
           "o.createdAt, " +
           "o.address.address, " +
           "(SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order = o), " +
           "d.deliveryFee, " +
           "o.status.name) " +
           "FROM Order o " +
           "LEFT JOIN Delivery d ON d.order = o " +
           "JOIN o.store s " +
           "WHERE s.createdBy.id = :userId")
    List<OrderSummaryDTO> findOrderSummariesByUserId(@Param("userId") Long userId);

    @Query("SELECT DISTINCT NEW ktc.spring_project.dtos.order.OrderSummaryDTO(" +
           "o.id, " +
           "o.store.id, " +
           "o.createdAt, " +
           "o.address.address, " +
           "(SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order = o), " +
           "(SELECT MAX(d2.deliveryFee) FROM Delivery d2 WHERE d2.order = o), " +
           "o.status.name) " +
           "FROM Order o " +
           "JOIN o.store s " +
           "WHERE s.createdBy.id = :userId " +
           "ORDER BY o.createdAt DESC")
    Page<OrderSummaryDTO> findOrderSummariesByUserIdPaginated(@Param("userId") Long userId, Pageable pageable);

    @Query("SELECT NEW ktc.spring_project.dtos.order.OrderSummaryDTO(" +
           "o.id, " +
           "o.store.id, " +
           "o.createdAt, " +
           "o.address.address, " +
           "(SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order = o), " +
           "(SELECT MAX(d2.deliveryFee) FROM Delivery d2 WHERE d2.order = o), " +
           "o.status.name) " +
           "FROM Order o " +
           "WHERE o.store.id = :storeId " +
           "AND o.id = :orderId " +
           "ORDER BY o.createdAt DESC")
    List<OrderSummaryDTO> findOrderSummariesByStoreIdAndOrderId(@Param("storeId") Long storeId, @Param("orderId") Long orderId);

    @Query("SELECT NEW ktc.spring_project.dtos.order.OrderSummaryDTO(" +
           "o.id, " +
           "o.store.id, " +
           "o.createdAt, " +
           "o.address.address, " +
           "(SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order = o), " +
           "(SELECT MAX(d2.deliveryFee) FROM Delivery d2 WHERE d2.order = o), " +
           "o.status.name) " +
           "FROM Order o " +
           "WHERE o.store.id = :storeId " +
           "AND (:fromDate IS NULL OR o.createdAt >= :fromDate) " +
           "AND (:toDate IS NULL OR o.createdAt <= :toDate) " +
           "ORDER BY o.createdAt DESC")
    List<OrderSummaryDTO> findOrderSummariesByStoreIdAndDateRange(
        @Param("storeId") Long storeId, 
        @Param("fromDate") LocalDateTime fromDate, 
        @Param("toDate") LocalDateTime toDate);

    @Query("SELECT NEW ktc.spring_project.dtos.order.OrderSummaryDTO(" +
           "o.id, " +
           "o.store.id, " +
           "o.createdAt, " +
           "o.address.address, " +
           "(SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order = o), " +
           "(SELECT MAX(d2.deliveryFee) FROM Delivery d2 WHERE d2.order = o), " +
           "o.status.name) " +
           "FROM Order o " +
           "WHERE o.store.id = :storeId " +
           "AND (:fromDate IS NULL OR o.createdAt >= :fromDate) " +
           "AND (:toDate IS NULL OR o.createdAt <= :toDate) " +
           "ORDER BY o.createdAt DESC")
    Page<OrderSummaryDTO> findOrderSummariesByStoreIdAndDateRangePaginated(
        @Param("storeId") Long storeId, 
        @Param("fromDate") LocalDateTime fromDate, 
        @Param("toDate") LocalDateTime toDate,
        Pageable pageable);

    /**
     * Unified search method that supports multiple search criteria:
     * - orderId: exact match if provided
     * - fromDate/toDate: date range if provided
     * - statusList: multiple status names if provided (uses IN clause)
     * - storeId: must belong to this store (required)
     */
    @Query("SELECT NEW ktc.spring_project.dtos.order.OrderSummaryDTO(" +
           "o.id, " +
           "o.store.id, " +
           "o.createdAt, " +
           "o.address.address, " +
           "(SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order = o), " +
           "(SELECT MAX(d2.deliveryFee) FROM Delivery d2 WHERE d2.order = o), " +
           "o.status.name) " +
           "FROM Order o " +
           "WHERE o.store.id = :storeId " +
           "AND (:orderId IS NULL OR o.id = :orderId) " +
           "AND (:fromDate IS NULL OR o.createdAt >= :fromDate) " +
           "AND (:toDate IS NULL OR o.createdAt <= :toDate) " +
           "AND (:#{#statusList} IS NULL OR :#{#statusList.size()} = 0 OR o.status.name IN :statusList) " +
           "ORDER BY o.createdAt DESC")
    Page<OrderSummaryDTO> findOrderSummariesByStoreIdWithFiltersPaginated(
        @Param("storeId") Long storeId,
        @Param("orderId") Long orderId,
        @Param("fromDate") LocalDateTime fromDate, 
        @Param("toDate") LocalDateTime toDate,
        @Param("statusList") List<String> statusList,
        Pageable pageable);

    /**
     * Get order statistics by store ID
     * Returns total orders, processing orders, and completed orders count
     */
    @Query("SELECT COUNT(o) FROM Order o WHERE o.store.id = :storeId")
    long countTotalOrdersByStoreId(@Param("storeId") Long storeId);

    @Query("SELECT COUNT(o) FROM Order o WHERE o.store.id = :storeId AND o.status.name = 'Processing'")
    long countProcessingOrdersByStoreId(@Param("storeId") Long storeId);

    @Query("SELECT COUNT(o) FROM Order o WHERE o.store.id = :storeId AND o.status.name = 'Completed'")
    long countCompletedOrdersByStoreId(@Param("storeId") Long storeId);

    // Count all completed orders (for performance metrics)
    @Query("SELECT COUNT(o) FROM Order o WHERE o.status.name = 'Completed'")
    long countAllCompletedOrders();
    
    // Find completed orders with pagination (supports both 'Completed' and 'COMPLETED' status)
    @Query("SELECT o FROM Order o WHERE o.status.name IN ('Completed', 'COMPLETED') ORDER BY o.createdAt DESC")
    Page<Order> findCompletedOrdersPaginated(Pageable pageable);
}