package ktc.spring_project.controllers;

import ktc.spring_project.entities.Product;
import ktc.spring_project.dtos.product.CreateProductRequestDTO;
import ktc.spring_project.entities.WarehouseTransaction;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.ProductService;
import ktc.spring_project.services.WarehouseTransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * Controller responsible for managing products
 * Based on database schema for products table
 */
@RestController
@RequestMapping("/api/products")
public class ProductController {

    @Autowired
    private ProductService productService;

    @Autowired
    private UserService userService;

    @Autowired
    private WarehouseTransactionService warehouseTransactionService;

    /**
     * Get all products with optional filters
     */
    @GetMapping
    public ResponseEntity<List<Product>> getAllProducts(
            @RequestParam(required = false) String category,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) Long warehouseId) {

        List<Product> products = productService.getFilteredProducts(category, status, search, warehouseId);
        return ResponseEntity.ok(products);
    }

    /**
     * Get product by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Product> getProductById(@PathVariable Long id) {
        Product product = productService.getProductById(id);
        return ResponseEntity.ok(product);
    }

    /**
     * Create new product
     */
    @PostMapping
    public ResponseEntity<Product> createProduct(
            @Valid @RequestBody CreateProductRequestDTO dto,
            Authentication authentication) {

        Product product = new Product();
        product.setName(dto.getName());
        product.setDescription(dto.getDescription());
        product.setUnitPrice(dto.getUnitPrice());
        product.setWeight(dto.getWeight());
    product.setStockQuantity(dto.getStockQuantity());
        product.setNotes(dto.getNotes());
        // Map warehouse, createdBy nếu cần (giả sử có các repository/service)
        // product.setWarehouse(...)
        // product.setCreatedBy(...)

        Product createdProduct = productService.createProduct(product, authentication);
        return new ResponseEntity<>(createdProduct, HttpStatus.CREATED);
    }

    /**
     * Update product information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Product> updateProduct(
            @PathVariable Long id,
            @Valid @RequestBody Product product,
            Authentication authentication) {

        Product updatedProduct = productService.updateProduct(id, product, authentication);
        return ResponseEntity.ok(updatedProduct);
    }

    // ...existing code...

@PatchMapping("/{id}")
public ResponseEntity<Product> patchProduct(
        @PathVariable Long id,
        @RequestBody Map<String, Object> updates,
        Authentication authentication) {
    Product updatedProduct = productService.patchProduct(id, updates, authentication);
    return ResponseEntity.ok(updatedProduct);
}

// ...existing code...
    /**
     * Delete product (soft delete)
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteProduct(
            @PathVariable Long id,
            Authentication authentication) {

        productService.deleteProduct(id, authentication);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get product warehouse transactions
     */
    @GetMapping("/{id}/warehouse-transactions")
    public ResponseEntity<List<WarehouseTransaction>> getProductWarehouseTransactions(
            @PathVariable Long id,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String transactionType) {

        List<WarehouseTransaction> transactions = warehouseTransactionService.getProductTransactions(
                id, dateFrom, dateTo, transactionType);
        return ResponseEntity.ok(transactions);
    }

    /**
     * Update product stock quantity
     */
    @PatchMapping("/{id}/stock")
    public ResponseEntity<Product> updateProductStock(
            @PathVariable Long id,
            @RequestBody Map<String, Object> stockData,
            Authentication authentication) {

        Integer newQuantity = (Integer) stockData.get("quantity");
        String reason = (String) stockData.get("reason");

        Product updatedProduct = productService.updateProductStock(id, newQuantity, reason, authentication);
        return ResponseEntity.ok(updatedProduct);
    }

    /**
     * Get low stock products
     */
    @GetMapping("/low-stock")
    public ResponseEntity<List<Product>> getLowStockProducts(
            @RequestParam(defaultValue = "10") Integer threshold) {

        List<Product> lowStockProducts = productService.getLowStockProducts(threshold);
        return ResponseEntity.ok(lowStockProducts);
    }
}
