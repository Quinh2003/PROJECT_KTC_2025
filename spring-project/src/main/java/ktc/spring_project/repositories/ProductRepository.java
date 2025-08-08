package ktc.spring_project.repositories;

import ktc.spring_project.entities.Product;
import ktc.spring_project.enums.ProductStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;


@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {





    List<Product> findByCategoryId(Long categoryId);

    List<Product> findByWarehouseId(Long warehouseId);

    List<Product> findByProductStatus(ProductStatus status);

    // Sửa lỗi: loại bỏ full path enum, chỉ sử dụng enum value
    @Query("SELECT p FROM Product p WHERE p.productStatus = 'AVAILABLE' ORDER BY p.name")
    List<Product> findActiveProductsOrderByName();

    @Query("SELECT p FROM Product p WHERE p.stockQuantity <= :threshold AND p.productStatus = 'AVAILABLE'")
    List<Product> findProductsWithLowStock(@Param("threshold") Integer threshold);

    @Query("SELECT p FROM Product p WHERE LOWER(p.name) LIKE LOWER(CONCAT('%', :name, '%')) AND p.productStatus = 'AVAILABLE'")
    List<Product> findByNameContainingIgnoreCase(@Param("name") String name);

    @Query("SELECT p FROM Product p WHERE p.unitPrice BETWEEN :minPrice AND :maxPrice AND p.productStatus = 'AVAILABLE'")
    List<Product> findByPriceRange(@Param("minPrice") BigDecimal minPrice, @Param("maxPrice") BigDecimal maxPrice);

    @Query("SELECT p FROM Product p WHERE p.category.id = :categoryId AND p.productStatus = 'AVAILABLE'")
    List<Product> findActiveByCategoryId(@Param("categoryId") Long categoryId);

    @Query("SELECT COUNT(p) FROM Product p WHERE p.productStatus = 'AVAILABLE'")
    long countActiveProducts();

    @Query("SELECT SUM(p.stockQuantity) FROM Product p WHERE p.productStatus = 'AVAILABLE'")
    Long getTotalStockQuantity();

}
