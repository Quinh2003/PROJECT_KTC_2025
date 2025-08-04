package ktc.spring_project.repositories;

import ktc.spring_project.entities.InventoryTransaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface InventoryTransactionRepository extends JpaRepository<InventoryTransaction, Long> {
    
    List<InventoryTransaction> findByProductId(Long productId);
    
    List<InventoryTransaction> findByWarehouseId(Long warehouseId);
    
    List<InventoryTransaction> findByOrderId(Long orderId);
    
    List<InventoryTransaction> findByStatusId(Long statusId);
    
    @Query("SELECT it FROM InventoryTransaction it WHERE it.transactionDate BETWEEN :startDate AND :endDate")
    List<InventoryTransaction> findByDateRange(@Param("startDate") LocalDateTime startDate, 
                                             @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT SUM(it.quantity) FROM InventoryTransaction it WHERE it.product.id = :productId AND it.status.code = 'COMPLETED'")
    Integer getTotalQuantityByProductId(@Param("productId") Long productId);
}
