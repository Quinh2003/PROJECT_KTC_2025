package ktc.spring_project.dtos.status;

import java.sql.Timestamp;

/**
 * DTO for status response data
 */
public class StatusResponseDTO {
    
    private Long id;
    private String type;
    private String code;
    private String description;
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public StatusResponseDTO() {}
    
    public StatusResponseDTO(Long id, String type, String code, String description) {
        this.id = id;
        this.type = type;
        this.code = code;
        this.description = description;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }
    
    public String getCode() { return code; }
    public void setCode(String code) { this.code = code; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public String getDisplayName() {
        return description != null ? description : code;
    }
    
    public String getFullName() {
        return String.format("%s - %s", code, description);
    }
}