package ktc.spring_project.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * FUTURE IMPLEMENTATION - System Configuration Management
 * This entity is not yet included in the current SQL schema but will be added in Phase 2
 * Handles dynamic system configuration and feature toggles
 */
@Entity
@Table(name = "system_configs")
public class SystemConfig {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "config_key", unique = true, nullable = false)
    private String configKey;
    
    @Column(name = "config_value", columnDefinition = "TEXT")
    private String configValue;
    
    @Column(name = "config_type")
    private String configType; // STRING, INTEGER, BOOLEAN, JSON
    
    @Column(columnDefinition = "TEXT")
    private String description;
    
    @Column(name = "is_active")
    private Boolean isActive = true;
    
    @Column(name = "is_system")
    private Boolean isSystem = false; // Cannot be deleted if true
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
    
    @ManyToOne
    @JoinColumn(name = "created_by")
    private User createdBy;
    
    @ManyToOne
    @JoinColumn(name = "updated_by")
    private User updatedBy;
    
    // Constructors
    public SystemConfig() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }
    
    public SystemConfig(String configKey, String configValue, String configType) {
        this();
        this.configKey = configKey;
        this.configValue = configValue;
        this.configType = configType;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getConfigKey() { return configKey; }
    public void setConfigKey(String configKey) { 
        this.configKey = configKey;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getConfigValue() { return configValue; }
    public void setConfigValue(String configValue) { 
        this.configValue = configValue;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getConfigType() { return configType; }
    public void setConfigType(String configType) { 
        this.configType = configType;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { 
        this.description = description;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { 
        this.isActive = isActive;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Boolean getIsSystem() { return isSystem; }
    public void setIsSystem(Boolean isSystem) { 
        this.isSystem = isSystem;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    
    // Relationship getters/setters
    public User getCreatedBy() { return createdBy; }
    public void setCreatedBy(User createdBy) { this.createdBy = createdBy; }
    
    public User getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(User updatedBy) { 
        this.updatedBy = updatedBy;
        this.updatedAt = LocalDateTime.now();
    }
    
    // Utility methods
    public Integer getIntValue() {
        try {
            return Integer.parseInt(configValue);
        } catch (NumberFormatException e) {
            return null;
        }
    }
    
    public Boolean getBooleanValue() {
        return Boolean.parseBoolean(configValue);
    }
    
    public Double getDoubleValue() {
        try {
            return Double.parseDouble(configValue);
        } catch (NumberFormatException e) {
            return null;
        }
    }
}
