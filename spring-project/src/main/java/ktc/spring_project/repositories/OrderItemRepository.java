package ktc.spring_project.repositories;

import ktc.spring_project.entities.OrderItem;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.Product;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Repository for OrderItem entity
 * Handles order line items and shipping calculations
 */
@Repository
public interface OrderItemRepository extends BaseRepository<OrderItem, Long> {
    
    /**
     * Find order items by order
     */
    List<OrderItem> findByOrderOrderByCreatedAt(@Param("order") Order order);
    
    /**
     * Find order items by product
     */
    List<OrderItem> findByProductOrderByCreatedAtDesc(@Param("product") Product product);
    
    /**
     * Calculate total order value for an order
     */
    @Query("SELECT SUM(oi.subtotal) FROM OrderItem oi WHERE oi.order = :order")
    BigDecimal calculateOrderTotal(@Param("order") Order order);
    
    /**
     * Calculate total shipping fee for an order
     */
    @Query("SELECT SUM(oi.shippingFee) FROM OrderItem oi WHERE oi.order = :order")
    BigDecimal calculateTotalShippingFee(@Param("order") Order order);
    
    /**
     * Calculate total weight for an order
     */
    @Query("SELECT SUM(oi.weightKg) FROM OrderItem oi WHERE oi.order = :order")
    BigDecimal calculateOrderWeight(@Param("order") Order order);
    
    /**
     * Find most ordered products
     */
    @Query("SELECT oi.product, SUM(oi.quantity) as totalQuantity, COUNT(DISTINCT oi.order) as orderCount " +
           "FROM OrderItem oi WHERE oi.createdAt BETWEEN :startDate AND :endDate " +
           "GROUP BY oi.product ORDER BY totalQuantity DESC")
    List<Object[]> findMostOrderedProducts(@Param("startDate") LocalDateTime startDate, 
                                          @Param("endDate") LocalDateTime endDate);
    
    /**
     * Get revenue by product
     */
    @Query("SELECT oi.product, SUM(oi.subtotal) as revenue, SUM(oi.quantity) as totalQuantity " +
           "FROM OrderItem oi WHERE oi.createdAt BETWEEN :startDate AND :endDate " +
           "GROUP BY oi.product ORDER BY revenue DESC")
    List<Object[]> getRevenueByProduct(@Param("startDate") LocalDateTime startDate, 
                                      @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find high-value order items
     */
    @Query("SELECT oi FROM OrderItem oi WHERE oi.subtotal >= :minValue ORDER BY oi.subtotal DESC")
    List<OrderItem> findHighValueItems(@Param("minValue") BigDecimal minValue);
    
    /**
     * Calculate average order item value
     */
    @Query("SELECT AVG(oi.subtotal) FROM OrderItem oi WHERE oi.createdAt BETWEEN :startDate AND :endDate")
    BigDecimal calculateAverageItemValue(@Param("startDate") LocalDateTime startDate, 
                                        @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find order items requiring special handling (fragile products)
     */
    @Query("SELECT oi FROM OrderItem oi WHERE oi.product.fragile = true " +
           "AND oi.order.status.code NOT IN ('DELIVERED', 'CANCELLED') ORDER BY oi.createdAt")
    List<OrderItem> findFragileItems();
    
    /**
     * Get shipping efficiency metrics
     */
    @Query("SELECT AVG(oi.actualDistanceKm), AVG(oi.shippingFee), AVG(oi.weightKg) " +
           "FROM OrderItem oi WHERE oi.actualDistanceKm IS NOT NULL " +
           "AND oi.createdAt BETWEEN :startDate AND :endDate")
    List<Object[]> getShippingMetrics(@Param("startDate") LocalDateTime startDate, 
                                     @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find order items with calculated distances for route optimization
     */
    @Query("SELECT oi FROM OrderItem oi WHERE oi.actualDistanceKm IS NOT NULL " +
           "ORDER BY oi.actualDistanceKm DESC")
    List<OrderItem> findItemsWithDistances();
    
    /**
     * Count order items by date range
     */
    @Query("SELECT COUNT(oi) FROM OrderItem oi WHERE oi.createdAt BETWEEN :startDate AND :endDate")
    Long countByDateRange(@Param("startDate") LocalDateTime startDate, 
                         @Param("endDate") LocalDateTime endDate);
}