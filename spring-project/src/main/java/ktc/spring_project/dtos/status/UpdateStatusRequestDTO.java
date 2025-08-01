package ktc.spring_project.dtos.status;

import jakarta.validation.constraints.*;

/**
 * DTO for updating existing status
 */
public class UpdateStatusRequestDTO {
    
    @Size(max = 50, message = "Status type must not exceed 50 characters")
    private String type;
    
    @Size(max = 20, message = "Status code must not exceed 20 characters")
    private String code;
    
    @Size(max = 255, message = "Description must not exceed 255 characters")
    private String description;
    
    // Constructors
    public UpdateStatusRequestDTO() {}
    
    // Getters and Setters
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }
    
    public String getCode() { return code; }
    public void setCode(String code) { this.code = code; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
}