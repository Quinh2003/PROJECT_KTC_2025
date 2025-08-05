package ktc.spring_project.dtos.status;

import jakarta.validation.constraints.*;

/**
 * DTO for creating new status
 */
public class CreateStatusRequestDTO {
    
    @NotBlank(message = "Status type is required")
    @Size(max = 50, message = "Status type must not exceed 50 characters")
    private String type;
    
    @NotBlank(message = "Status code is required")
    @Size(max = 20, message = "Status code must not exceed 20 characters")
    private String code;
    
    @NotBlank(message = "Description is required")
    @Size(max = 255, message = "Description must not exceed 255 characters")
    private String description;
    
    // Constructors
    public CreateStatusRequestDTO() {}
    
    public CreateStatusRequestDTO(String type, String code, String description) {
        this.type = type;
        this.code = code;
        this.description = description;
    }
    
    // Getters and Setters
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }
    
    public String getCode() { return code; }
    public void setCode(String code) { this.code = code; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
}