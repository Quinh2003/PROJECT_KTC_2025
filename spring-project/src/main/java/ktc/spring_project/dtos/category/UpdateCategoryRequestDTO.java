package ktc.spring_project.dtos.category;

/**
 * DTO for updating an existing category
 */
public class UpdateCategoryRequestDTO {
    
    private String name;
    private String description;
    private Long parentId;
    private Boolean isActive;
    private String notes;
    
    // Constructors
    public UpdateCategoryRequestDTO() {}
    
    public UpdateCategoryRequestDTO(String name, String description) {
        this.name = name;
        this.description = description;
    }
    
    // Getters and Setters
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
    public boolean hasValidName() {
        return name != null && !name.trim().isEmpty();
    }
    
    public boolean isActivating() {
        return Boolean.TRUE.equals(isActive);
    }
    
    public boolean isDeactivating() {
        return Boolean.FALSE.equals(isActive);
    }
    
    public boolean isChangingParent() {
        return parentId != null;
    }
    
    public boolean isMovingToRoot() {
        return parentId != null && parentId == 0L; // 0 indicates moving to root
    }
}