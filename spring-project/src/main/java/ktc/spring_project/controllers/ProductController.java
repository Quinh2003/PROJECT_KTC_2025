package ktc.spring_project.controllers;

import ktc.spring_project.dtos.product.CreateProductRequestDTO;
import ktc.spring_project.dtos.product.ProductResponseDTO;
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
    public ResponseEntity<List<ProductResponseDTO>> getAllProducts(
            @RequestParam(required = false) String category,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) Long warehouseId) {
        List<ProductResponseDTO> products = productService.getFilteredProductsDTO(category, status, search, warehouseId);
        return ResponseEntity.ok(products);
    }

    /**
     * Get product by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<ProductResponseDTO> getProductById(@PathVariable Long id) {
        ProductResponseDTO product = productService.getProductDTOById(id);
        return ResponseEntity.ok(product);
    }

    /**
     * Create new product
     */
    @PostMapping
    public ResponseEntity<ProductResponseDTO> createProduct(
            @Valid @RequestBody CreateProductRequestDTO dto,
            Authentication authentication) {
        ProductResponseDTO createdProduct = productService.createProductFromDTO(dto, authentication);
        return new ResponseEntity<>(createdProduct, HttpStatus.CREATED);
    }

    /**
     * Update product information
     */
    @PutMapping("/{id}")
    public ResponseEntity<ProductResponseDTO> updateProduct(
            @PathVariable Long id,
            @Valid @RequestBody CreateProductRequestDTO dto,
            Authentication authentication) {
        ProductResponseDTO updatedProduct = productService.updateProductFromDTO(id, dto, authentication);
        return ResponseEntity.ok(updatedProduct);
    }

    // ...existing code...

@PatchMapping("/{id}")
public ResponseEntity<ProductResponseDTO> patchProduct(
        @PathVariable Long id,
        @RequestBody Map<String, Object> updates,
        Authentication authentication) {
    ProductResponseDTO updatedProduct = productService.patchProductDTO(id, updates, authentication);
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
    public ResponseEntity<ProductResponseDTO> updateProductStock(
            @PathVariable Long id,
            @RequestBody Map<String, Object> stockData,
            Authentication authentication) {
        Integer newQuantity = (Integer) stockData.get("quantity");
        String reason = (String) stockData.get("reason");
        ProductResponseDTO updatedProduct = productService.updateProductStockDTO(id, newQuantity, reason, authentication);
        return ResponseEntity.ok(updatedProduct);
    }

    /**
     * Get low stock products
     */
    @GetMapping("/low-stock")
    public ResponseEntity<List<ProductResponseDTO>> getLowStockProducts(
            @RequestParam(defaultValue = "10") Integer threshold) {
        List<ProductResponseDTO> lowStockProducts = productService.getLowStockProducts(threshold);
        return ResponseEntity.ok(lowStockProducts);
    }
}
