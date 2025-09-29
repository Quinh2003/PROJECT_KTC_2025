
package ktc.spring_project.dtos.category;

import jakarta.validation.constraints.NotBlank;

/**
 * DTO for updating an existing category
 */
public class UpdateCategoryRequestDTO {

    @NotBlank(message = "Category name is required")
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
}