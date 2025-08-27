package ktc.spring_project.repositories;

import ktc.spring_project.entities.OrderItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderItemRepository extends JpaRepository<OrderItem, Long> {
    
    List<OrderItem> findByOrderId(Long orderId);
    
    List<OrderItem> findByProductId(Long productId);
    
   @Query("SELECT oi FROM OrderItem oi WHERE oi.order.id = :orderId")
List<OrderItem> findByOrderIdUsingQuery(@Param("orderId") Long orderId);


    
    @Query("SELECT SUM(oi.quantity) FROM OrderItem oi WHERE oi.product.id = :productId")
    Long getTotalQuantityByProductId(@Param("productId") Long productId);
    
    @Query("SELECT oi FROM OrderItem oi JOIN oi.order o WHERE o.status.name = :statusName")
    List<OrderItem> findByOrderStatusName(@Param("statusName") String statusName);
    
    @Query("SELECT COUNT(oi) FROM OrderItem oi WHERE oi.order.id = :orderId")
    long countItemsByOrderId(@Param("orderId") Long orderId);
}

