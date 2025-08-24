package ktc.spring_project.dtos.status;

import jakarta.validation.constraints.*;

/**
 * DTO for updating existing status
 */
public class UpdateStatusRequestDTO {
    
    @Size(max = 50, message = "Status type must not exceed 50 characters")
    private String type;
    
    @Size(max = 100, message = "Status name must not exceed 100 characters")
    private String name;
    
    @Size(max = 255, message = "Description must not exceed 255 characters")
    private String description;
    
    // Constructors
    public UpdateStatusRequestDTO() {}
    
    // Getters and Setters
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
}