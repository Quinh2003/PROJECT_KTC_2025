package ktc.spring_project.repositories;

import ktc.spring_project.dtos.order.OrderSummaryDTO;
import ktc.spring_project.entities.Order;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import ktc.spring_project.dtos.order.OrderSummaryDTO;

@Repository
public interface OrderRepository extends JpaRepository<Order, Long> {

    List<Order> findByStatus_Id(Short statusId);
    List<Order> findByStore_Id(Long storeId);
    List<Order> findByCreatedBy_Id(Long createdBy);

    // Query các đơn hàng có trạng thái AVAILABLE
    @Query("SELECT o FROM Order o WHERE o.status.name = 'AVAILABLE'")
    List<Order> findAvailableOrders();

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
}