package ktc.spring_project.dtos.category;

/**
 * DTO for creating a new category
 */
public class CreateCategoryRequestDTO {
    
    private String categoryId;
    private String name;
    private String description;
    private Long parentId;
    private Boolean isActive = true;
    private String notes;
    
    // Constructors
    public CreateCategoryRequestDTO() {}
    
    public CreateCategoryRequestDTO(String categoryId, String name) {
        this.categoryId = categoryId;
        this.name = name;
    }
    
    public CreateCategoryRequestDTO(String categoryId, String name, String description, Long parentId) {
        this.categoryId = categoryId;
        this.name = name;
        this.description = description;
        this.parentId = parentId;
    }
    
    // Getters and Setters
    public String getCategoryId() { return categoryId; }
    public void setCategoryId(String categoryId) { this.categoryId = categoryId; }
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public Long getParentId() { return parentId; }
    public void setParentId(Long parentId) { this.parentId = parentId; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    // Validation methods
    public boolean isValid() {
        return categoryId != null && !categoryId.trim().isEmpty() &&
               name != null && !name.trim().isEmpty();
    }
    
    public boolean isRootCategory() {
        return parentId == null;
    }
    
    public boolean isSubCategory() {
        return parentId != null;
    }
    
    public String getDisplayName() {
        return name != null ? name : categoryId;
    }
}