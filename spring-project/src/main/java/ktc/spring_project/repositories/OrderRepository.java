package ktc.spring_project.repositories;

import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.enums.TransportMode;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repository for Order entity
 * Handles order management and logistics operations
 */
@Repository
public interface OrderRepository extends BaseRepository<Order, Long> {
    
    /**
     * Find order by order code
     */
    Optional<Order> findByOrderCode(@Param("orderCode") String orderCode);
    
    /**
     * Find orders by customer
     */
    Page<Order> findByCustomerOrderByCreatedAtDesc(@Param("customer") User customer, Pageable pageable);
    
    /**
     * Find orders by driver
     */
    List<Order> findByDriverOrderByScheduledTimeAsc(@Param("driver") User driver);
    
    /**
     * Find orders by vehicle
     */
    List<Order> findByVehicleOrderByScheduledTimeAsc(@Param("vehicle") Vehicle vehicle);
    
    /**
     * Find orders by status code
     */
    @Query("SELECT o FROM Order o WHERE o.status.code = :statusCode ORDER BY o.createdAt DESC")
    List<Order> findByStatusCode(@Param("statusCode") String statusCode);
    
    /**
     * Find pending orders (ready for assignment)
     */
    @Query("SELECT o FROM Order o WHERE o.status.code IN ('PENDING', 'READY') ORDER BY o.createdAt ASC")
    List<Order> findPendingOrders();
    
    /**
     * Find active orders (in progress)
     */
    @Query("SELECT o FROM Order o WHERE o.status.code IN ('READY', 'ON_DELIVERY') ORDER BY o.scheduledTime ASC")
    List<Order> findActiveOrders();
    
    /**
     * Find orders by transport mode
     */
    List<Order> findByTransportModeOrderByCreatedAtDesc(@Param("transportMode") TransportMode transportMode);
    
    /**
     * Find orders scheduled for date range
     */
    @Query("SELECT o FROM Order o WHERE o.scheduledTime BETWEEN :startDate AND :endDate ORDER BY o.scheduledTime ASC")
    List<Order> findByScheduledTimeRange(@Param("startDate") LocalDateTime startDate, 
                                        @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find orders created within date range
     */
    @Query("SELECT o FROM Order o WHERE o.createdAt BETWEEN :startDate AND :endDate ORDER BY o.createdAt DESC")
    Page<Order> findByCreatedAtRange(@Param("startDate") LocalDateTime startDate, 
                                    @Param("endDate") LocalDateTime endDate, Pageable pageable);
    
    /**
     * Search orders by order code or recipient
     */
    @Query("SELECT o FROM Order o WHERE " +
           "LOWER(o.orderCode) LIKE LOWER(CONCAT('%', :searchTerm, '%')) OR " +
           "LOWER(o.recipientName) LIKE LOWER(CONCAT('%', :searchTerm, '%')) OR " +
           "LOWER(o.recipientPhone) LIKE LOWER(CONCAT('%', :searchTerm, '%')) " +
           "ORDER BY o.createdAt DESC")
    Page<Order> searchOrders(@Param("searchTerm") String searchTerm, Pageable pageable);
    
    /**
     * Find unassigned orders (no driver/vehicle)
     */
    @Query("SELECT o FROM Order o WHERE (o.driver IS NULL OR o.vehicle IS NULL) " +
           "AND o.status.code IN ('PENDING', 'READY') ORDER BY o.createdAt ASC")
    List<Order> findUnassignedOrders();
    
    /**
     * Find overdue orders (past scheduled time)
     */
    @Query("SELECT o FROM Order o WHERE o.scheduledTime < CURRENT_TIMESTAMP " +
           "AND o.status.code NOT IN ('DELIVERED', 'CANCELLED') ORDER BY o.scheduledTime ASC")
    List<Order> findOverdueOrders();
    
    /**
     * Calculate revenue by date range
     */
    @Query("SELECT SUM(o.totalAmount) FROM Order o WHERE o.status.code = 'DELIVERED' " +
           "AND o.actualDeliveryTime BETWEEN :startDate AND :endDate")
    BigDecimal calculateRevenue(@Param("startDate") LocalDateTime startDate, 
                               @Param("endDate") LocalDateTime endDate);
    
    /**
     * Get delivery statistics
     */
    @Query("SELECT o.status.code, COUNT(o) FROM Order o " +
           "WHERE o.createdAt BETWEEN :startDate AND :endDate " +
           "GROUP BY o.status.code ORDER BY o.status.code")
    List<Object[]> getDeliveryStatistics(@Param("startDate") LocalDateTime startDate, 
                                        @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find orders by delivery area (using delivery address)
     */
    @Query("SELECT o FROM Order o WHERE LOWER(o.deliveryAddress) LIKE LOWER(CONCAT('%', :area, '%')) " +
           "ORDER BY o.createdAt DESC")
    List<Order> findByDeliveryArea(@Param("area") String area);
    
    /**
     * Find orders requiring priority delivery
     */
    @Query("SELECT o FROM Order o WHERE o.transportMode = 'PRIORITY' " +
           "AND o.status.code NOT IN ('DELIVERED', 'CANCELLED') ORDER BY o.scheduledTime ASC")
    List<Order> findPriorityOrders();
    
    /**
     * Count orders by customer and status
     */
    @Query("SELECT COUNT(o) FROM Order o WHERE o.customer = :customer AND o.status.code = :statusCode")
    Long countByCustomerAndStatus(@Param("customer") User customer, @Param("statusCode") String statusCode);
    
    /**
     * Find orders with GPS coordinates (for route optimization)
     */
    @Query("SELECT o FROM Order o WHERE o.pickupLatitude IS NOT NULL AND o.pickupLongitude IS NOT NULL " +
           "AND o.deliveryLatitude IS NOT NULL AND o.deliveryLongitude IS NOT NULL " +
           "AND o.status.code IN ('PENDING', 'READY') ORDER BY o.createdAt ASC")
    List<Order> findOrdersWithCoordinates();
}