package ktc.spring_project.dtos.status;

import jakarta.validation.constraints.*;

/**
 * DTO for creating new status
 */
public class CreateStatusRequestDTO {
    
    @NotBlank(message = "Status type is required")
    @Size(max = 50, message = "Status type must not exceed 50 characters")
    private String type;
    
    @NotBlank(message = "Status name is required")
    @Size(max = 100, message = "Status name must not exceed 100 characters")
    private String name;
    
    @NotBlank(message = "Description is required")
    @Size(max = 255, message = "Description must not exceed 255 characters")
    private String description;
    
    // Constructors
    public CreateStatusRequestDTO() {}
    
    public CreateStatusRequestDTO(String type, String name, String description) {
        this.type = type;
        this.name = name;
        this.description = description;
    }
    
    // Getters and Setters
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
}