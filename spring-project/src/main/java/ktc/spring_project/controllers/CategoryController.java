package ktc.spring_project.controllers;

import ktc.spring_project.entities.Category;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * Controller responsible for managing product categories
 * Based on database schema for categories table
 */
@RestController
@RequestMapping("/api/categories")
public class CategoryController {

    // Assume these services exist
    @Autowired
    private CategoryService categoryService;

    @Autowired
    private UserService userService;

    /**
     * Get all categories with optional filters
     */
    @GetMapping
    public ResponseEntity<List<Category>> getAllCategories(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Long parentId,
            @RequestParam(required = false) String search) {

        List<Category> categories = categoryService.getFilteredCategories(status, parentId, search);
        return ResponseEntity.ok(categories);
    }

    /**
     * Get category tree structure
     */
    @GetMapping("/tree")
    public ResponseEntity<List<Map<String, Object>>> getCategoryTree(
            @RequestParam(required = false) String status) {

        List<Map<String, Object>> categoryTree = categoryService.getCategoryTree(status);
        return ResponseEntity.ok(categoryTree);
    }

    /**
     * Get category by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Category> getCategoryById(@PathVariable Long id) {
        Category category = categoryService.getCategoryById(id);
        return ResponseEntity.ok(category);
    }

    /**
     * Create new category
     */
    @PostMapping
    public ResponseEntity<Category> createCategory(
            @Valid @RequestBody Category category,
            Authentication authentication) {

        Category createdCategory = categoryService.createCategory(category, authentication);
        return new ResponseEntity<>(createdCategory, HttpStatus.CREATED);
    }

    /**
     * Update category information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Category> updateCategory(
            @PathVariable Long id,
            @Valid @RequestBody Category category,
            Authentication authentication) {

        Category updatedCategory = categoryService.updateCategory(id, category, authentication);
        return ResponseEntity.ok(updatedCategory);
    }

    /**
     * Delete category (soft delete)
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteCategory(
            @PathVariable Long id,
            Authentication authentication) {

        categoryService.deleteCategory(id, authentication);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get products in category
     */
    @GetMapping("/{id}/products")
    public ResponseEntity<List<Map<String, Object>>> getCategoryProducts(
            @PathVariable Long id,
            @RequestParam(required = false) String status,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        List<Map<String, Object>> products = categoryService.getCategoryProducts(id, status, page, size);
        return ResponseEntity.ok(products);
    }

    /**
     * Get category statistics
     */
    @GetMapping("/{id}/statistics")
    public ResponseEntity<Map<String, Object>> getCategoryStatistics(@PathVariable Long id) {
        Map<String, Object> statistics = categoryService.getCategoryStatistics(id);
        return ResponseEntity.ok(statistics);
    }
}
