package ktc.spring_project.repositories;

import ktc.spring_project.entities.WarehouseTransaction;
import ktc.spring_project.enums.TransactionType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface WarehouseTransactionRepository extends JpaRepository<WarehouseTransaction, Long> {
    
    List<WarehouseTransaction> findByProductId(Long productId);
    
    List<WarehouseTransaction> findByWarehouseId(Long warehouseId);
    
    List<WarehouseTransaction> findByOrderId(Long orderId);
    
    List<WarehouseTransaction> findByTransactionType(TransactionType transactionType);
    
    List<WarehouseTransaction> findByStatusId(Short statusId);
    
    @Query("SELECT wt FROM WarehouseTransaction wt WHERE wt.transactionDate BETWEEN :startDate AND :endDate ORDER BY wt.transactionDate DESC")
    List<WarehouseTransaction> findTransactionsBetweenDates(@Param("startDate") LocalDateTime startDate, 
                                                          @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT wt FROM WarehouseTransaction wt WHERE wt.product.id = :productId AND wt.warehouse.id = :warehouseId ORDER BY wt.transactionDate DESC")
    List<WarehouseTransaction> findByProductAndWarehouse(@Param("productId") Long productId, 
                                                       @Param("warehouseId") Long warehouseId);
    

    

    
    @Query("SELECT SUM(wt.quantity) FROM WarehouseTransaction wt WHERE wt.product.id = :productId " +
           "AND wt.warehouse.id = :warehouseId AND wt.transactionType = :type AND wt.status.name = 'SUCCESS'")
    Long getTotalQuantityByProductWarehouseAndType(@Param("productId") Long productId, 
                                                 @Param("warehouseId") Long warehouseId, 
                                                 @Param("type") TransactionType type);
    
    @Query("SELECT COUNT(wt) FROM WarehouseTransaction wt WHERE wt.transactionType = :type")
    long countByTransactionType(@Param("type") TransactionType type);
    
    @Query("SELECT wt FROM WarehouseTransaction wt WHERE wt.createdBy.id = :userId ORDER BY wt.createdAt DESC")
    List<WarehouseTransaction> findByCreatedByOrderByCreatedAtDesc(@Param("userId") Long userId);
}

