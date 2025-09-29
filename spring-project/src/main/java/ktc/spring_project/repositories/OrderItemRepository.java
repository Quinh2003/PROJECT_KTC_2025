package ktc.spring_project.repositories;

import ktc.spring_project.entities.OrderItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface OrderItemRepository extends JpaRepository<OrderItem, Long> {
    // Batch: lấy tổng quantity cho nhiều orderId
    @Query("SELECT oi.order.id, SUM(oi.quantity) FROM OrderItem oi WHERE oi.order.id IN :orderIds GROUP BY oi.order.id")
    List<Object[]> getTotalQuantityByOrderIdsRaw(@Param("orderIds") List<Long> orderIds);

    // Helper để trả về Map<orderId, totalQuantity>
    default java.util.Map<Long, Long> getTotalQuantityByOrderIds(List<Long> orderIds) {
        List<Object[]> results = getTotalQuantityByOrderIdsRaw(orderIds);
        java.util.Map<Long, Long> map = new java.util.HashMap<>();
        for (Object[] row : results) {
            Long orderId = (Long) row[0];
            Long total = (Long) row[1];
            map.put(orderId, total != null ? total : 0L);
        }
        // Đảm bảo các orderId không có item cũng trả về 0
        for (Long id : orderIds) {
            map.putIfAbsent(id, 0L);
        }
        return map;
    }
    Optional<OrderItem> findByProductCode(String productCode);
    
    List<OrderItem> findByOrderId(Long orderId);
    
    List<OrderItem> findByProductId(Long productId);
    
   @Query("SELECT oi FROM OrderItem oi WHERE oi.order.id = :orderId")
List<OrderItem> findByOrderIdUsingQuery(@Param("orderId") Long orderId);

    // Phân trang theo orderId
    org.springframework.data.domain.Page<OrderItem> findByOrderId(Long orderId, org.springframework.data.domain.Pageable pageable);


//     @Query("SELECT oi FROM OrderItem oi LEFT JOIN FETCH oi.order LEFT JOIN FETCH oi.product WHERE oi.order.id = :orderId")
// List<OrderItem> findByOrderIdWithProduct(@Param("orderId") Long orderId);

    @Query("SELECT SUM(oi.quantity) FROM OrderItem oi WHERE oi.product.id = :productId")
    Long getTotalQuantityByProductId(@Param("productId") Long productId);
    
    @Query("SELECT oi FROM OrderItem oi JOIN oi.order o WHERE o.status.name = :statusName")
    List<OrderItem> findByOrderStatusName(@Param("statusName") String statusName);
    
    @Query("SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order.id = :orderId")
    long countItemsByOrderId(@Param("orderId") Long orderId);
    
    @Query("SELECT SUM(oi.quantity) FROM OrderItem oi WHERE oi.order.id = :orderId")
    Long getTotalQuantityByOrderId(@Param("orderId") Long orderId);
}

