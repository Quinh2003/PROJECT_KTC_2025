package ktc.spring_project.services.interfaces;

import ktc.spring_project.dtos.product.CreateProductRequestDTO;
import ktc.spring_project.dtos.product.ProductResponseDTO;
import ktc.spring_project.dtos.common.PagedResponse;
import ktc.spring_project.entities.Product;
import org.springframework.data.domain.Pageable;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * Service interface for Product and Inventory management
 * Handles product catalog, stock management, and inventory transactions
 */
public interface ProductService {
    
    // ===== PRODUCT CRUD =====
    /**
     * Create a new product
     * @param createProductDTO Product creation data
     * @param createdBy User who creates the product
     * @return Created product response
     */
    ProductResponseDTO createProduct(CreateProductRequestDTO createProductDTO, Long createdBy);
    
    /**
     * Update existing product
     * @param productId Product ID
     * @param updateData Updated product data
     * @return Updated product response
     */
    ProductResponseDTO updateProduct(Long productId, CreateProductRequestDTO updateData);
    
    /**
     * Get product by ID
     * @param productId Product ID
     * @return Product response or empty if not found
     */
    Optional<ProductResponseDTO> getProductById(Long productId);
    
    /**
     * Get product entity by ID (for internal service use)
     * @param productId Product ID
     * @return Product entity or empty if not found
     */
    Optional<Product> getProductEntityById(Long productId);
    
    /**
     * Get product by product code
     * @param productCode Unique product code
     * @return Product response or empty if not found
     */
    Optional<ProductResponseDTO> getProductByCode(String productCode);
    
    /**
     * Delete product (soft delete)
     * @param productId Product ID
     */
    void deleteProduct(Long productId);
    
    // ===== PRODUCT QUERIES =====
    /**
     * Get all products with pagination
     * @param pageable Pagination parameters
     * @return Paged product responses
     */
    PagedResponse<ProductResponseDTO> getAllProducts(Pageable pageable);
    
    /**
     * Get products by warehouse
     * @param warehouseId Warehouse ID
     * @param pageable Pagination parameters
     * @return Paged products in specified warehouse
     */
    PagedResponse<ProductResponseDTO> getProductsByWarehouse(Long warehouseId, Pageable pageable);
    
    /**
     * Get temporary products (created by customers)
     * @param pageable Pagination parameters
     * @return Paged temporary products
     */
    PagedResponse<ProductResponseDTO> getTemporaryProducts(Pageable pageable);
    
    /**
     * Get fragile products
     * @param pageable Pagination parameters
     * @return Paged fragile products
     */
    PagedResponse<ProductResponseDTO> getFragileProducts(Pageable pageable);
    
    /**
     * Get products with low stock (below threshold)
     * @param threshold Stock threshold
     * @param pageable Pagination parameters
     * @return Paged low stock products
     */
    PagedResponse<ProductResponseDTO> getLowStockProducts(int threshold, Pageable pageable);
    
    /**
     * Get products within price range
     * @param minPrice Minimum price
     * @param maxPrice Maximum price
     * @param pageable Pagination parameters
     * @return Paged products within price range
     */
    PagedResponse<ProductResponseDTO> getProductsByPriceRange(BigDecimal minPrice, BigDecimal maxPrice, Pageable pageable);
    
    /**
     * Get products within weight range
     * @param minWeight Minimum weight
     * @param maxWeight Maximum weight
     * @param pageable Pagination parameters
     * @return Paged products within weight range
     */
    PagedResponse<ProductResponseDTO> getProductsByWeightRange(BigDecimal minWeight, BigDecimal maxWeight, Pageable pageable);
    
    // ===== SEARCH =====
    /**
     * Search products by name, code, or description
     * @param searchTerm Search term
     * @param pageable Pagination parameters
     * @return Paged search results
     */
    PagedResponse<ProductResponseDTO> searchProducts(String searchTerm, Pageable pageable);
    
    // ===== INVENTORY MANAGEMENT =====
    /**
     * Update product stock quantity
     * @param productId Product ID
     * @param newQuantity New stock quantity
     * @param updatedBy User who updates the stock
     * @return Updated product response
     */
    ProductResponseDTO updateStock(Long productId, Integer newQuantity, Long updatedBy);
    
    /**
     * Add stock to product
     * @param productId Product ID
     * @param quantity Quantity to add
     * @param updatedBy User who adds stock
     * @return Updated product response
     */
    ProductResponseDTO addStock(Long productId, Integer quantity, Long updatedBy);
    
    /**
     * Remove stock from product
     * @param productId Product ID
     * @param quantity Quantity to remove
     * @param updatedBy User who removes stock
     * @return Updated product response
     */
    ProductResponseDTO removeStock(Long productId, Integer quantity, Long updatedBy);
    
    /**
     * Check if sufficient stock is available
     * @param productId Product ID
     * @param requiredQuantity Required quantity
     * @return true if sufficient stock available
     */
    boolean isStockAvailable(Long productId, Integer requiredQuantity);
    
    /**
     * Reserve stock for order
     * @param productId Product ID
     * @param quantity Quantity to reserve
     * @param orderId Order ID for tracking
     * @return true if reservation successful
     */
    boolean reserveStock(Long productId, Integer quantity, Long orderId);
    
    /**
     * Release reserved stock
     * @param productId Product ID
     * @param quantity Quantity to release
     * @param orderId Order ID for tracking
     */
    void releaseStock(Long productId, Integer quantity, Long orderId);
    
    // ===== ANALYTICS =====
    /**
     * Calculate total inventory value
     * @return Total value of all products in inventory
     */
    BigDecimal calculateTotalInventoryValue();
    
    /**
     * Calculate inventory value by warehouse
     * @param warehouseId Warehouse ID
     * @return Total value of products in specified warehouse
     */
    BigDecimal calculateWarehouseInventoryValue(Long warehouseId);
    
    /**
     * Get most ordered products
     * @param limit Number of top products to return
     * @return List of most ordered products with statistics
     */
    List<java.util.Map<String, Object>> getMostOrderedProducts(int limit);
    
    /**
     * Get product statistics
     * @return Map of product statistics (total, low stock, by warehouse, etc.)
     */
    java.util.Map<String, Object> getProductStatistics();
    
    /**
     * Get revenue by product for date range
     * @param startDate Start date
     * @param endDate End date
     * @param limit Number of top products to return
     * @return List of product revenue statistics
     */
    List<java.util.Map<String, Object>> getRevenueByProduct(java.time.LocalDateTime startDate, 
                                                            java.time.LocalDateTime endDate, 
                                                            int limit);
}