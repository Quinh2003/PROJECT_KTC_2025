package ktc.spring_project.services;

import ktc.spring_project.entities.Category;
import ktc.spring_project.entities.Product;
import ktc.spring_project.entities.Status;
import ktc.spring_project.entities.Warehouse;
import ktc.spring_project.repositories.ProductRepository;
import ktc.spring_project.repositories.CategoryRepository;
import ktc.spring_project.repositories.StatusRepository;
import ktc.spring_project.repositories.WarehouseRepository;
import ktc.spring_project.repositories.WarehouseTransactionRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class ProductService {
    @Autowired
    private ProductRepository productRepository;

    @Autowired
    private CategoryRepository categoryRepository;

    @Autowired
    private StatusRepository statusRepository;

    @Autowired
    private WarehouseRepository warehouseRepository;

    @Autowired
    private WarehouseTransactionRepository warehouseTransactionRepository;

    @Autowired
    private ActivityLogService activityLogService;

    public List<Product> findAll() {
        return productRepository.findAll();
    }

    public Optional<Product> findById(Long id) {
        return productRepository.findById(id);
    }

    public Product save(Product entity) {
        return productRepository.save(entity);
    }

    public void delete(Long id) {
        productRepository.deleteById(id);
    }

    /**
     * Get products with optional filters
     *
     * @param category Optional category filter
     * @param status Optional status filter (active/inactive)
     * @param search Optional search text for product name
     * @param warehouseId Optional warehouse ID filter
     * @return List of products matching the criteria
     */
    public List<Product> getFilteredProducts(String category, String status, String search, Long warehouseId) {
        // Default to returning all products if no filters are applied
        if (category == null && status == null && search == null && warehouseId == null) {
            return findAll();
        }

        // In a real implementation, would combine filters based on which parameters are provided
        // For now, implement simple filtering based on the first non-null parameter

        if (category != null) {
            // Assume category is a category ID string
            return productRepository.findActiveByCategoryId(Long.valueOf(category));
        }

        if (status != null) {
            // Convert status string to boolean (e.g., "active" -> true)
            boolean isActive = "active".equalsIgnoreCase(status);
            return productRepository.findByProductStatus(isActive ? ktc.spring_project.enums.ProductStatus.ACTIVE : ktc.spring_project.enums.ProductStatus.INACTIVE);
        }

        if (search != null && !search.trim().isEmpty()) {
            return productRepository.findByNameContainingIgnoreCase(search);
        }

        if (warehouseId != null) {
            return productRepository.findByWarehouseId(warehouseId);
        }

        return new ArrayList<>();
    }

    /**
     * Get product by ID
     *
     * @param id Product ID
     * @return Product entity
     * @throws EntityNotFoundException if product not found
     */
    public Product getProductById(Long id) {
        return productRepository.findById(id)
            .orElseThrow(() -> new EntityNotFoundException("Product not found with id: " + id));
    }

    /**
     * Create a new product
     *
     * @param product Product entity to create
     * @param authentication User authentication for audit
     * @return Created product
     */
    public Product createProduct(Product product, Authentication authentication) {
        // In a real implementation, would extract user ID from authentication
        // and set it as the created_by value

        // Check if product code already exists
        if (productRepository.existsByProductCode(product.getProductCode())) {
            throw new IllegalArgumentException("Product code already exists: " + product.getProductCode());
        }

        // Check if product card ID already exists
        if (productRepository.existsByProductCardId(product.getProductCardId())) {
            throw new IllegalArgumentException("Product card ID already exists: " + product.getProductCardId());
        }

        // Save the product
        Product savedProduct = save(product);

        // Log the activity (in a real implementation)
        // activityLogService.logActivity(authentication, "CREATE", "products", savedProduct.getId());

        return savedProduct;
    }

    /**
     * Update product information
     *
     * @param id Product ID to update
     * @param productDetails Updated product details
     * @param authentication User authentication for audit
     * @return Updated product
     */
    public Product updateProduct(Long id, Product productDetails, Authentication authentication) {
        Product existingProduct = getProductById(id);

        // Update fields
        existingProduct.setName(productDetails.getName());
        existingProduct.setDescription(productDetails.getDescription());
        existingProduct.setCategory(productDetails.getCategory());
        existingProduct.setUnitPrice(productDetails.getUnitPrice());
        existingProduct.setWeight(productDetails.getWeight());
        existingProduct.setVolume(productDetails.getVolume());
        existingProduct.setIsFragile(productDetails.getIsFragile());
        existingProduct.setProductImage(productDetails.getProductImage());
        existingProduct.setProductStatus(productDetails.getProductStatus());
        existingProduct.setWarehouse(productDetails.getWarehouse());
        existingProduct.setNotes(productDetails.getNotes());

        // Don't update immutable or sensitive fields like:
        // - productCode
        // - productCardId
        // - stockQuantity (should be updated through warehouse transactions)
        // - createdBy
        // - createdAt

        Product updatedProduct = save(existingProduct);

        // Log the activity (in a real implementation)
        // activityLogService.logActivity(authentication, "UPDATE", "products", updatedProduct.getId());

        return updatedProduct;
    }

    /**
     * Delete product (soft delete by changing status)
     *
     * @param id Product ID to delete
     * @param authentication User authentication for audit
     */
    public void deleteProduct(Long id, Authentication authentication) {
        Product product = getProductById(id);

        // Soft delete by setting status to inactive
        Status inactiveStatus = statusRepository.findById((short)0)
            .orElseThrow(() -> new EntityNotFoundException("Inactive status not found"));
        product.setProductStatus(inactiveStatus);
        save(product);

        // Log the activity (in a real implementation)
        // activityLogService.logActivity(authentication, "DELETE", "products", id);
    }

    /**
     * Update product stock quantity
     *
     * @param id Product ID
     * @param newQuantity New stock quantity
     * @param reason Reason for adjustment
     * @param authentication User authentication for audit
     * @return Updated product
     */
    public Product updateProductStock(Long id, Integer newQuantity, String reason, Authentication authentication) {
        Product product = getProductById(id);

        // Get current quantity
        Integer currentQuantity = product.getStockQuantity();

        // Calculate difference
        int difference = newQuantity - currentQuantity;

        // Update stock quantity
        product.setStockQuantity(newQuantity);
        Product updatedProduct = save(product);

        // In a real implementation, would create a warehouse transaction record
        // WarehouseTransaction transaction = new WarehouseTransaction();
        // transaction.setProduct(product);
        // transaction.setWarehouse(product.getWarehouse());
        // transaction.setQuantity(Math.abs(difference));
        // transaction.setTransactionType(difference > 0 ? TransactionType.IN : TransactionType.OUT);
        // transaction.setNotes("Manual adjustment: " + reason);
        // transaction.setCreatedBy(getUserFromAuthentication(authentication));
        // warehouseTransactionRepository.save(transaction);

        // Log the activity
        // activityLogService.logActivity(authentication, "STOCK_UPDATE", "products", id);

        return updatedProduct;
    }

    /**
     * Get products with stock quantity below threshold
     *
     * @param threshold Stock threshold
     * @return List of products with stock below threshold
     */
    public List<Product> getLowStockProducts(Integer threshold) {
        return productRepository.findProductsWithLowStock(threshold);
    }
}
