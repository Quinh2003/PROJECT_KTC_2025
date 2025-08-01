package ktc.spring_project.repositories;

import ktc.spring_project.entities.Warehouse;
import ktc.spring_project.entities.User;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * Repository for Warehouse entity
 * Handles warehouse management and inventory operations
 */
@Repository
public interface WarehouseRepository extends BaseRepository<Warehouse, Long> {
    
    /**
     * Find warehouse by name
     */
    Optional<Warehouse> findByNameAndIsActive(@Param("name") String name, @Param("isActive") Boolean isActive);
    
    /**
     * Find all active warehouses
     */
    @Override
    @Query("SELECT w FROM Warehouse w WHERE w.isActive = true ORDER BY w.name")
    List<Warehouse> findAllActive();
    
    /**
     * Find warehouses by creator
     */
    List<Warehouse> findByCreatedByAndIsActive(@Param("createdBy") User createdBy, @Param("isActive") Boolean isActive);
    
    /**
     * Find warehouses with capacity greater than specified value
     */
    @Query("SELECT w FROM Warehouse w WHERE w.isActive = true AND w.capacity >= :minCapacity ORDER BY w.capacity DESC")
    List<Warehouse> findByMinCapacity(@Param("minCapacity") BigDecimal minCapacity);
    
    /**
     * Find warehouses with weight capacity greater than specified value
     */
    @Query("SELECT w FROM Warehouse w WHERE w.isActive = true AND w.maxWeight >= :minWeight ORDER BY w.maxWeight DESC")
    List<Warehouse> findByMinWeightCapacity(@Param("minWeight") BigDecimal minWeight);
    
    /**
     * Search warehouses by name or address
     */
    @Query("SELECT w FROM Warehouse w WHERE w.isActive = true AND " +
           "(LOWER(w.name) LIKE LOWER(CONCAT('%', :searchTerm, '%')) OR " +
           "LOWER(w.address) LIKE LOWER(CONCAT('%', :searchTerm, '%'))) ORDER BY w.name")
    List<Warehouse> searchByNameOrAddress(@Param("searchTerm") String searchTerm);
    
    /**
     * Find warehouses suitable for product storage (based on capacity)
     */
    @Query("SELECT w FROM Warehouse w WHERE w.isActive = true " +
           "AND (w.capacity IS NULL OR w.capacity >= :requiredCapacity) " +
           "AND (w.maxWeight IS NULL OR w.maxWeight >= :requiredWeight) ORDER BY w.name")
    List<Warehouse> findSuitableWarehouses(@Param("requiredCapacity") BigDecimal requiredCapacity,
                                          @Param("requiredWeight") BigDecimal requiredWeight);
    
    /**
     * Get warehouse utilization statistics
     */
    @Query("SELECT w, COUNT(p) as productCount, SUM(p.stockQuantity) as totalStock " +
           "FROM Warehouse w LEFT JOIN w.products p " +
           "WHERE w.isActive = true GROUP BY w ORDER BY w.name")
    List<Object[]> getWarehouseUtilizationStats();
    
    /**
     * Find warehouses with low capacity utilization
     */
    @Query("SELECT w FROM Warehouse w WHERE w.isActive = true " +
           "AND w.id NOT IN (SELECT DISTINCT p.warehouse.id FROM Product p WHERE p.warehouse IS NOT NULL " +
           "AND p.stockQuantity > 0) ORDER BY w.name")
    List<Warehouse> findUnderutilizedWarehouses();
    
    /**
     * Check if warehouse name exists (for validation)
     */
    boolean existsByNameAndIsActive(@Param("name") String name, @Param("isActive") Boolean isActive);
    
    /**
     * Count active warehouses
     */
    @Query("SELECT COUNT(w) FROM Warehouse w WHERE w.isActive = true")
    Long countActiveWarehouses();
}