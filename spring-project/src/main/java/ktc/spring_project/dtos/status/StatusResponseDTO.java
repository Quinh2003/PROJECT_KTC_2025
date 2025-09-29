package ktc.spring_project.dtos.status;

import java.sql.Timestamp;

/**
 * DTO for status response data
 */
public class StatusResponseDTO {
    
    private Short id;
    private String type;
    private String name;
    private String description;
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public StatusResponseDTO() {}
    
    public StatusResponseDTO(Short id, String type, String name, String description) {
        this.id = id;
        this.type = type;
        this.name = name;
        this.description = description;
    }
    
    // Getters and Setters
    public Short getId() { return id; }
    public void setId(Short id) { this.id = id; }
    
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public String getDisplayName() {
        return description != null ? description : name;
    }
    
    public String getFullName() {
        return String.format("%s - %s", name, description);
    }
}