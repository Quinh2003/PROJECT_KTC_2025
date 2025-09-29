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
import java.util.Map;
import java.util.Optional;

@Service
public class ProductService {
    @Autowired
    private ktc.spring_project.services.UserService userService;
    // --- DTO methods for controller ---
    public List<ktc.spring_project.dtos.product.ProductResponseDTO> getFilteredProductsDTO(String category, String status, String search, Long warehouseId) {
        List<Product> products = getFilteredProducts(category, status, search, warehouseId);
        List<ktc.spring_project.dtos.product.ProductResponseDTO> dtos = new ArrayList<>();
        for (Product p : products) {
            dtos.add(toProductResponseDTO(p));
        }
        return dtos;
    }

    public ktc.spring_project.dtos.product.ProductResponseDTO getProductDTOById(Long id) {
        Product p = getProductById(id);
        return toProductResponseDTO(p);
    }

    public ktc.spring_project.dtos.product.ProductResponseDTO createProductFromDTO(ktc.spring_project.dtos.product.CreateProductRequestDTO dto, Authentication authentication) {
        Product product = createProductFromDto(dto, authentication);
        return toProductResponseDTO(product);
    }

    public ktc.spring_project.dtos.product.ProductResponseDTO updateProductFromDTO(Long id, ktc.spring_project.dtos.product.CreateProductRequestDTO dto, Authentication authentication) {
        Product existing = getProductById(id);
        // Update fields from DTO
        existing.setName(dto.getName());
        existing.setDescription(dto.getDescription());
        existing.setUnitPrice(dto.getUnitPrice());
        existing.setWeight(dto.getWeight());
        existing.setIsFragile(dto.getFragile());
        existing.setStockQuantity(dto.getStockQuantity());
        existing.setNotes(dto.getNotes());
        // ...map các trường khác nếu cần...
        Product updated = save(existing);
        return toProductResponseDTO(updated);
    }

    public ktc.spring_project.dtos.product.ProductResponseDTO patchProductDTO(Long id, Map<String, Object> updates, Authentication authentication) {
        Product patched = patchProduct(id, updates, authentication);
        return toProductResponseDTO(patched);
    }

    public ktc.spring_project.dtos.product.ProductResponseDTO updateProductStockDTO(Long id, Integer newQuantity, String reason, Authentication authentication) {
        Product updated = updateProductStock(id, newQuantity, reason, authentication);
        return toProductResponseDTO(updated);
    }

    public List<ktc.spring_project.dtos.product.ProductResponseDTO> getLowStockProducts(Integer threshold) {
        List<Product> products = productRepository.findProductsWithLowStock(threshold);
        List<ktc.spring_project.dtos.product.ProductResponseDTO> dtos = new ArrayList<>();
        for (Product p : products) {
            dtos.add(toProductResponseDTO(p));
        }
        return dtos;
    }

    // Helper: convert Product entity to ProductResponseDTO
    private ktc.spring_project.dtos.product.ProductResponseDTO toProductResponseDTO(Product p) {
        ktc.spring_project.dtos.product.ProductResponseDTO dto = new ktc.spring_project.dtos.product.ProductResponseDTO();
        dto.setId(p.getId());
        dto.setName(p.getName());
        dto.setDescription(p.getDescription());
        dto.setUnitPrice(p.getUnitPrice());
        dto.setWeight(p.getWeight());
        dto.setVolume(p.getVolume());
        dto.setFragile(p.getIsFragile());
        dto.setStockQuantity(p.getStockQuantity());
        dto.setNotes(p.getNotes());
        dto.setWarehouseId(p.getWarehouse() != null ? p.getWarehouse().getId() : null);
        // ...map các trường khác nếu cần...
        return dto;
    }
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

    // Tạo mới sản phẩm từ DTO, kiểm tra trùng tên
    public Product createProductFromDto(ktc.spring_project.dtos.product.CreateProductRequestDTO dto, Authentication authentication) {
    // Không kiểm tra trùng id sản phẩm theo createdByUserId (id sản phẩm là tự sinh)
        Product product = new Product();
        product.setName(dto.getName());
        product.setDescription(dto.getDescription());
        product.setUnitPrice(dto.getUnitPrice());
        product.setWeight(dto.getWeight());
        product.setIsFragile(dto.getFragile());
        product.setStockQuantity(dto.getStockQuantity());
        product.setNotes(dto.getNotes());

        // Map categoryId sang entity (có thể null)
        if (dto.getCategoryId() != null) {
            categoryRepository.findById(dto.getCategoryId()).ifPresent(product::setCategory);
        }
        // Không set category nếu null

        // Map warehouseId sang entity (chỉ set nếu có)
        if (dto.getWarehouseId() != null) {
            warehouseRepository.findById(dto.getWarehouseId()).ifPresent(product::setWarehouse);
        } else {
            product.setWarehouse(null);
        }

        // Map createdByUserId sang entity (chỉ set nếu có)
        if (dto.getCreatedByUserId() != null) {
            ktc.spring_project.entities.User user = userService.getUserById(dto.getCreatedByUserId());
            product.setCreatedBy(user);
        } else {
            product.setCreatedBy(null);
        }

    // Map volume nếu có
    product.setVolume(dto.getVolume());

    // Map productStatus nếu có, mặc định ACTIVE nếu null
    if (dto.getProductStatus() != null) {
        product.setProductStatus(ktc.spring_project.enums.ProductStatus.fromValue(dto.getProductStatus()));
    } else {
        product.setProductStatus(ktc.spring_project.enums.ProductStatus.ACTIVE);
    }
    return save(product);
    }
// ...existing code...
public Product patchProduct(Long id, Map<String, Object> updates, Authentication authentication) {
    Product product = getProductById(id);
    if (updates.containsKey("name")) {
        product.setName((String) updates.get("name"));
    }
    // Thêm các trường khác nếu cần
    // Lưu lại sản phẩm
    return productRepository.save(product);
}
// ...existing code...

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





        // Kiểm tra trùng lặp tên sản phẩm
        if (productRepository.findByName(product.getName()).isPresent()) {
            throw new ktc.spring_project.exceptions.EntityDuplicateException("Product name");
        }
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

    // Soft delete bằng cách đặt status là INACTIVE (enum)
    product.setProductStatus(ktc.spring_project.enums.ProductStatus.INACTIVE);
    save(product);

    // Ghi log nếu cần
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

}
