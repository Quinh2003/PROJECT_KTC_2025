package ktc.spring_project.repositories;

import ktc.spring_project.entities.Product;
import ktc.spring_project.entities.Warehouse;
import ktc.spring_project.entities.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * Repository for Product entity
 * Handles product catalog and inventory management
 */
@Repository
public interface ProductRepository extends BaseRepository<Product, Long> {
    
    /**
     * Find product by product code
     */
    Optional<Product> findByProductCode(@Param("productCode") String productCode);
    
    /**
     * Check if product code exists
     */
    boolean existsByProductCode(@Param("productCode") String productCode);
    
    /**
     * Find products by warehouse
     */
    List<Product> findByWarehouseOrderByName(@Param("warehouse") Warehouse warehouse);
    
    /**
     * Find products by creator
     */
    List<Product> findByCreatedByOrderByName(@Param("createdBy") User createdBy);
    
    /**
     * Find temporary products (created by customers)
     */
    @Query("SELECT p FROM Product p WHERE p.temporary = true ORDER BY p.createdAt DESC")
    List<Product> findTemporaryProducts();
    
    /**
     * Find permanent products (system catalog)
     */
    @Query("SELECT p FROM Product p WHERE p.temporary = false ORDER BY p.name")
    List<Product> findPermanentProducts();
    
    /**
     * Search products by name, code, or description
     */
    @Query("SELECT p FROM Product p WHERE " +
           "LOWER(p.name) LIKE LOWER(CONCAT('%', :searchTerm, '%')) OR " +
           "LOWER(p.productCode) LIKE LOWER(CONCAT('%', :searchTerm, '%')) OR " +
           "LOWER(p.description) LIKE LOWER(CONCAT('%', :searchTerm, '%')) " +
           "ORDER BY p.name")
    Page<Product> searchProducts(@Param("searchTerm") String searchTerm, Pageable pageable);
    
    /**
     * Find products with low stock
     */
    @Query("SELECT p FROM Product p WHERE p.stockQuantity <= :threshold AND p.temporary = false ORDER BY p.stockQuantity ASC")
    List<Product> findLowStockProducts(@Param("threshold") Integer threshold);
    
    /**
     * Find products with zero stock
     */
    @Query("SELECT p FROM Product p WHERE p.stockQuantity = 0 AND p.temporary = false ORDER BY p.name")
    List<Product> findOutOfStockProducts();
    
    /**
     * Find fragile products
     */
    @Query("SELECT p FROM Product p WHERE p.fragile = true ORDER BY p.name")
    List<Product> findFragileProducts();
    
    /**
     * Find products by price range
     */
    @Query("SELECT p FROM Product p WHERE p.unitPrice BETWEEN :minPrice AND :maxPrice ORDER BY p.unitPrice")
    List<Product> findByPriceRange(@Param("minPrice") BigDecimal minPrice, @Param("maxPrice") BigDecimal maxPrice);
    
    /**
     * Find products by weight range
     */
    @Query("SELECT p FROM Product p WHERE p.weight BETWEEN :minWeight AND :maxWeight ORDER BY p.weight")
    List<Product> findByWeightRange(@Param("minWeight") BigDecimal minWeight, @Param("maxWeight") BigDecimal maxWeight);
    
    /**
     * Get inventory value by warehouse
     */
    @Query("SELECT p.warehouse, SUM(p.stockQuantity * p.unitPrice) as inventoryValue " +
           "FROM Product p WHERE p.warehouse IS NOT NULL AND p.stockQuantity > 0 " +
           "GROUP BY p.warehouse ORDER BY inventoryValue DESC")
    List<Object[]> getInventoryValueByWarehouse();
    
    /**
     * Find best-selling products (most ordered)
     */
    @Query("SELECT p, SUM(oi.quantity) as totalOrdered FROM Product p " +
           "JOIN p.orderItems oi GROUP BY p ORDER BY totalOrdered DESC")
    List<Object[]> findBestSellingProducts(Pageable pageable);
    
    /**
     * Calculate total inventory value
     */
    @Query("SELECT SUM(p.stockQuantity * p.unitPrice) FROM Product p WHERE p.stockQuantity > 0")
    BigDecimal calculateTotalInventoryValue();
    
    /**
     * Find products by multiple criteria
     */
    @Query("SELECT p FROM Product p WHERE " +
           "(:warehouseId IS NULL OR p.warehouse.id = :warehouseId) AND " +
           "(:fragile IS NULL OR p.fragile = :fragile) AND " +
           "(:temporary IS NULL OR p.temporary = :temporary) AND " +
           "(:minStock IS NULL OR p.stockQuantity >= :minStock) " +
           "ORDER BY p.name")
    Page<Product> findByCriteria(@Param("warehouseId") Long warehouseId,
                                @Param("fragile") Boolean fragile,
                                @Param("temporary") Boolean temporary,
                                @Param("minStock") Integer minStock,
                                Pageable pageable);
}