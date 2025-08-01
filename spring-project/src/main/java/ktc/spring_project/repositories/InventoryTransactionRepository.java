package ktc.spring_project.repositories;

import ktc.spring_project.entities.InventoryTransaction;
import ktc.spring_project.entities.Product;
import ktc.spring_project.entities.Warehouse;
import ktc.spring_project.entities.Order;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Repository for InventoryTransaction entity
 * Handles inventory movements and stock tracking
 */
@Repository
public interface InventoryTransactionRepository extends BaseRepository<InventoryTransaction, Long> {
    
    /**
     * Find transactions by product
     */
    List<InventoryTransaction> findByProductOrderByTransactionDateDesc(@Param("product") Product product);
    
    /**
     * Find transactions by warehouse
     */
    List<InventoryTransaction> findByWarehouseOrderByTransactionDateDesc(@Param("warehouse") Warehouse warehouse);
    
    /**
     * Find transactions by order
     */
    List<InventoryTransaction> findByOrderOrderByTransactionDateDesc(@Param("order") Order order);
    
    /**
     * Find transactions within date range
     */
    @Query("SELECT it FROM InventoryTransaction it WHERE it.transactionDate BETWEEN :startDate AND :endDate ORDER BY it.transactionDate DESC")
    List<InventoryTransaction> findByDateRange(@Param("startDate") LocalDateTime startDate, 
                                              @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find transactions by product and warehouse
     */
    List<InventoryTransaction> findByProductAndWarehouseOrderByTransactionDateDesc(@Param("product") Product product, 
                                                                                  @Param("warehouse") Warehouse warehouse);
    
    /**
     * Calculate current stock for product in warehouse
     */
    @Query("SELECT COALESCE(SUM(it.quantity), 0) FROM InventoryTransaction it " +
           "WHERE it.product = :product AND it.warehouse = :warehouse")
    Integer calculateCurrentStock(@Param("product") Product product, @Param("warehouse") Warehouse warehouse);
    
    /**
     * Get stock movements summary by product
     */
    @Query("SELECT it.product, SUM(CASE WHEN it.quantity > 0 THEN it.quantity ELSE 0 END) as inbound, " +
           "SUM(CASE WHEN it.quantity < 0 THEN ABS(it.quantity) ELSE 0 END) as outbound " +
           "FROM InventoryTransaction it WHERE it.transactionDate BETWEEN :startDate AND :endDate " +
           "GROUP BY it.product ORDER BY it.product.name")
    List<Object[]> getStockMovementsSummary(@Param("startDate") LocalDateTime startDate, 
                                           @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find recent transactions for audit
     */
    @Query("SELECT it FROM InventoryTransaction it ORDER BY it.createdAt DESC")
    Page<InventoryTransaction> findRecentTransactions(Pageable pageable);
    
    /**
     * Find transactions by status
     */
    @Query("SELECT it FROM InventoryTransaction it WHERE it.status.code = :statusCode ORDER BY it.transactionDate DESC")
    List<InventoryTransaction> findByStatus(@Param("statusCode") String statusCode);
    
    /**
     * Get inventory value changes over time
     */
    @Query("SELECT DATE(it.transactionDate) as date, " +
           "SUM(it.quantity * (SELECT p.unitPrice FROM Product p WHERE p.id = it.product.id)) as valueChange " +
           "FROM InventoryTransaction it WHERE it.transactionDate BETWEEN :startDate AND :endDate " +
           "GROUP BY DATE(it.transactionDate) ORDER BY date")
    List<Object[]> getInventoryValueChanges(@Param("startDate") LocalDateTime startDate, 
                                           @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find transactions requiring approval
     */
    @Query("SELECT it FROM InventoryTransaction it WHERE it.status.code = 'PENDING_APPROVAL' ORDER BY it.createdAt ASC")
    List<InventoryTransaction> findPendingApproval();
    
    /**
     * Count transactions by warehouse and date range
     */
    @Query("SELECT COUNT(it) FROM InventoryTransaction it WHERE it.warehouse = :warehouse " +
           "AND it.transactionDate BETWEEN :startDate AND :endDate")
    Long countByWarehouseAndDateRange(@Param("warehouse") Warehouse warehouse,
                                     @Param("startDate") LocalDateTime startDate,
                                     @Param("endDate") LocalDateTime endDate);
}