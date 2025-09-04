package ktc.spring_project.controllers;

import ktc.spring_project.entities.Category;
import ktc.spring_project.services.CategoryService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.dtos.category.CreateCategoryRequestDTO;
import ktc.spring_project.dtos.category.UpdateCategoryRequestDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.stream.Collectors;

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

        // Lấy tất cả danh mục từ service
        List<Category> allCategories = categoryService.getAllCategories();

        // Lọc danh mục theo các tiêu chí nếu cần
        if (status != null || parentId != null || search != null) {
            List<Category> filteredCategories = allCategories.stream()
                .filter(category ->
                    (status == null || (category.getIsActive() == ("1".equals(status) || "true".equalsIgnoreCase(status))))
                    && (parentId == null || (category.getParent() != null && category.getParent().getId().equals(parentId)))
                    && (search == null || category.getName().toLowerCase().contains(search.toLowerCase()))
                )
                .collect(Collectors.toList());
            return ResponseEntity.ok(filteredCategories);
        }

        return ResponseEntity.ok(allCategories);
    }

    /**
     * Get category tree structure
     */
    @GetMapping("/tree")
    public ResponseEntity<List<Map<String, Object>>> getCategoryTree(
            @RequestParam(required = false) String status) {

        // Lấy tất cả danh mục
        List<Category> allCategories = categoryService.getAllCategories();

        // Lọc theo trạng thái nếu cần
        if (status != null) {
            boolean isActiveStatus = "1".equals(status) || "true".equalsIgnoreCase(status);
            allCategories = allCategories.stream()
                .filter(category -> category.getIsActive() == isActiveStatus)
                .collect(Collectors.toList());
        }

        // Tạo cấu trúc cây danh mục
        List<Map<String, Object>> categoryTree = buildCategoryTree(allCategories);

        return ResponseEntity.ok(categoryTree);
    }

    /**
     * Helper method to build category tree
     */
    private List<Map<String, Object>> buildCategoryTree(List<Category> categories) {
        // Tìm các danh mục gốc (không có parent)
        List<Category> rootCategories = categories.stream()
            .filter(category -> category.getParent() == null)
            .collect(Collectors.toList());

        // Tạo cây danh mục từ các danh mục gốc
        return rootCategories.stream()
            .map(rootCategory -> buildCategoryNode(rootCategory, categories))
            .collect(Collectors.toList());
    }

    /**
     * Helper method to build a single category node with its children
     */
    private Map<String, Object> buildCategoryNode(Category category, List<Category> allCategories) {
        Map<String, Object> node = new HashMap<>();
        node.put("id", category.getId());
        node.put("name", category.getName());
        node.put("isActive", category.getIsActive());

        // Tìm tất cả các danh mục con
        List<Category> children = allCategories.stream()
            .filter(c -> c.getParent() != null && c.getParent().getId().equals(category.getId()))
            .collect(Collectors.toList());

        if (!children.isEmpty()) {
            node.put("children", children.stream()
                .map(child -> buildCategoryNode(child, allCategories))
                .collect(Collectors.toList()));
        }

        return node;
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
            @Valid @RequestBody CreateCategoryRequestDTO dto,
            Authentication authentication) {

        // Map DTO sang entity
        Category category = new Category();
        category.setName(dto.getName());
        category.setDescription(dto.getDescription());
        category.setIsActive(dto.getIsActive());
        category.setNotes(dto.getNotes());
        if (dto.getParentId() != null) {
            Category parent = categoryService.getCategoryById(dto.getParentId());
            category.setParent(parent);
        }

        Category createdCategory = categoryService.createCategory(category);
        return new ResponseEntity<>(createdCategory, HttpStatus.CREATED);
    }

    /**
     * Update category information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Category> updateCategory(
            @PathVariable Long id,
            @Valid @RequestBody UpdateCategoryRequestDTO dto,
            Authentication authentication) {

        Category categoryDetails = new Category();
        categoryDetails.setName(dto.getName());
        categoryDetails.setDescription(dto.getDescription());
        categoryDetails.setIsActive(dto.getIsActive());
        categoryDetails.setNotes(dto.getNotes());
        if (dto.getParentId() != null) {
            Category parent = categoryService.getCategoryById(dto.getParentId());
            categoryDetails.setParent(parent);
        }

        Category updatedCategory = categoryService.updateCategory(id, categoryDetails);
        return ResponseEntity.ok(updatedCategory);
    }

    @PatchMapping("/{id}")
public ResponseEntity<Category> patchCategory(
        @PathVariable Long id,
        @RequestBody Map<String, Object> updates,
        Authentication authentication) {

    Category category = categoryService.getCategoryById(id);
    if (updates.containsKey("name")) {
        category.setName((String) updates.get("name"));
    }
    // Thêm các trường khác nếu cần

    Category updatedCategory = categoryService.createCategory(category);
    return ResponseEntity.ok(updatedCategory);
}
    /**
     * Delete category (soft delete)
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteCategory(
            @PathVariable Long id,
            Authentication authentication) {

        // Lấy thông tin người dùng hiện tại nếu cần
        // User currentUser = userService.getCurrentUser(authentication);

        // Xóa danh mục
        categoryService.deleteCategory(id);
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

        // Kiểm tra xem danh mục có tồn tại không
        Category category = categoryService.getCategoryById(id);

        // Trả về danh sách trống tạm thời
        // Trong thực tế, sẽ lấy danh sách sản phẩm thuộc danh mục này từ ProductService
        List<Map<String, Object>> products = new ArrayList<>();

        return ResponseEntity.ok(products);
    }

    /**
     * Get category statistics
     */
    @GetMapping("/{id}/statistics")
    public ResponseEntity<Map<String, Object>> getCategoryStatistics(@PathVariable Long id) {
        // Kiểm tra xem danh mục có tồn tại không
        Category category = categoryService.getCategoryById(id);

        // Tạo thống kê giả tạm thời
        Map<String, Object> statistics = new HashMap<>();
        statistics.put("categoryId", id);
        statistics.put("categoryName", category.getName());
        statistics.put("totalProducts", 0);
        statistics.put("activeProducts", 0);
        statistics.put("averagePrice", 0.0);

        return ResponseEntity.ok(statistics);
    }
}
