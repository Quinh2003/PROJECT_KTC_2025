package ktc.spring_project.repositories;

import ktc.spring_project.entities.Product;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {
    
    Optional<Product> findByProductCode(String productCode);
    
    Optional<Product> findByProductCardId(String productCardId);
    
    List<Product> findByCategoryId(Long categoryId);
    
    List<Product> findByWarehouseId(Long warehouseId);
    
    List<Product> findByProductStatus(Boolean status);
    
    @Query("SELECT p FROM Product p WHERE p.productStatus = true ORDER BY p.name")
    List<Product> findActiveProductsOrderByName();
    
    @Query("SELECT p FROM Product p WHERE p.stockQuantity <= :threshold AND p.productStatus = true")
    List<Product> findProductsWithLowStock(@Param("threshold") Integer threshold);
    
    @Query("SELECT p FROM Product p WHERE LOWER(p.name) LIKE LOWER(CONCAT('%', :name, '%')) AND p.productStatus = true")
    List<Product> findByNameContainingIgnoreCase(@Param("name") String name);
    
    @Query("SELECT p FROM Product p WHERE p.unitPrice BETWEEN :minPrice AND :maxPrice AND p.productStatus = true")
    List<Product> findByPriceRange(@Param("minPrice") BigDecimal minPrice, @Param("maxPrice") BigDecimal maxPrice);
    
    @Query("SELECT p FROM Product p WHERE p.category.categoryId = :categoryId AND p.productStatus = true")
    List<Product> findActiveByCategoryId(@Param("categoryId") String categoryId);
    
    boolean existsByProductCode(String productCode);
    
    boolean existsByProductCardId(String productCardId);
    
    @Query("SELECT COUNT(p) FROM Product p WHERE p.productStatus = true")
    long countActiveProducts();
    
    @Query("SELECT SUM(p.stockQuantity) FROM Product p WHERE p.productStatus = true")
    Long getTotalStockQuantity();
}
