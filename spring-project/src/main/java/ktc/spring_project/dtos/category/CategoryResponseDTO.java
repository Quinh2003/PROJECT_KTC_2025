package ktc.spring_project.dtos.category;

import java.sql.Timestamp;
import java.util.List;

/**
 * DTO for category response data
 */
public class CategoryResponseDTO {
    
    private Long id;

    private String name;
    private String description;
    
    // Parent category information
    private Long parentId;
    private String parentName;

    
    // Child categories
    private List<CategoryResponseDTO> children;
    private Integer childrenCount;
    
    private Boolean isActive;
    private String notes;
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Product statistics
    private Integer productCount;
    private Integer activeProductCount;
    
    // Constructors
    public CategoryResponseDTO() {}
    
    public CategoryResponseDTO(Long id, String name, Boolean isActive) {
        this.id = id;
        this.name = name;
        this.isActive = isActive;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    

    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public Long getParentId() { return parentId; }
    public void setParentId(Long parentId) { this.parentId = parentId; }
    
    public String getParentName() { return parentName; }
    public void setParentName(String parentName) { this.parentName = parentName; }
    

    
    public List<CategoryResponseDTO> getChildren() { return children; }
    public void setChildren(List<CategoryResponseDTO> children) { this.children = children; }
    
    public Integer getChildrenCount() { return childrenCount; }
    public void setChildrenCount(Integer childrenCount) { this.childrenCount = childrenCount; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    public Integer getProductCount() { return productCount; }
    public void setProductCount(Integer productCount) { this.productCount = productCount; }
    
    public Integer getActiveProductCount() { return activeProductCount; }
    public void setActiveProductCount(Integer activeProductCount) { this.activeProductCount = activeProductCount; }
    
    // Utility methods
    public String getDisplayName() {
        return name != null ? name : "Category #" + id;
    }
    
    public boolean isRootCategory() {
        return parentId == null;
    }
    
    public boolean isSubCategory() {
        return parentId != null;
    }
    
    public boolean hasChildren() {
        return children != null && !children.isEmpty();
    }
    
    public boolean hasProducts() {
        return productCount != null && productCount > 0;
    }
    
    public boolean isActiveCategory() {
        return Boolean.TRUE.equals(isActive);
    }
    
    public String getFullPath() {
        StringBuilder path = new StringBuilder();
        if (parentName != null) {
            path.append(parentName).append(" > ");
        }
        path.append(name);
        return path.toString();
    }
    
    public String getStatusDisplay() {
        return isActiveCategory() ? "Active" : "Inactive";
    }
    
    public String getCategoryHierarchy() {
        if (parentName != null) {
            return String.format("%s > %s", parentName, name);
        }
        return name;
    }
}