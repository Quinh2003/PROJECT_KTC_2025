package ktc.spring_project.dtos.route;

import ktc.spring_project.dtos.user.UserResponseDTO;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for route response data
 */
public class RouteResponseDTO {
    
    private Long id;
    private String name;
    private String waypoints;
    private BigDecimal estimatedDistance;
    private Integer estimatedDuration;
    private BigDecimal estimatedCost;
    private Boolean aiOptimized;
    private String notes;
    
    // Created by user information
    private UserResponseDTO createdBy;
    private Long createdByUserId;
    private String createdByUserName;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public RouteResponseDTO() {}
    
    public RouteResponseDTO(Long id, String name, BigDecimal estimatedDistance, 
                           Integer estimatedDuration, Boolean aiOptimized) {
        this.id = id;
        this.name = name;
        this.estimatedDistance = estimatedDistance;
        this.estimatedDuration = estimatedDuration;
        this.aiOptimized = aiOptimized;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getWaypoints() { return waypoints; }
    public void setWaypoints(String waypoints) { this.waypoints = waypoints; }
    
    public BigDecimal getEstimatedDistance() { return estimatedDistance; }
    public void setEstimatedDistance(BigDecimal estimatedDistance) { this.estimatedDistance = estimatedDistance; }
    
    public Integer getEstimatedDuration() { return estimatedDuration; }
    public void setEstimatedDuration(Integer estimatedDuration) { this.estimatedDuration = estimatedDuration; }
    
    public BigDecimal getEstimatedCost() { return estimatedCost; }
    public void setEstimatedCost(BigDecimal estimatedCost) { this.estimatedCost = estimatedCost; }
    
    public Boolean getAiOptimized() { return aiOptimized; }
    public void setAiOptimized(Boolean aiOptimized) { this.aiOptimized = aiOptimized; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public UserResponseDTO getCreatedBy() { return createdBy; }
    public void setCreatedBy(UserResponseDTO createdBy) { this.createdBy = createdBy; }
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
    
    public String getCreatedByUserName() { return createdByUserName; }
    public void setCreatedByUserName(String createdByUserName) { this.createdByUserName = createdByUserName; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public String getDisplayName() {
        return name + (aiOptimized ? " (AI Optimized)" : "");
    }
    
    public boolean hasEstimatedValues() {
        return estimatedDistance != null && estimatedDuration != null;
    }
    
    public String getFormattedDuration() {
        if (estimatedDuration == null) return null;
        
        int hours = estimatedDuration / 60;
        int minutes = estimatedDuration % 60;
        
        if (hours > 0) {
            return String.format("%dh %dm", hours, minutes);
        } else {
            return String.format("%dm", minutes);
        }
    }
    
    public String getFormattedDistance() {
        if (estimatedDistance == null) return null;
        return String.format("%.2f km", estimatedDistance);
    }
    
    public boolean isAiOptimized() {
        return Boolean.TRUE.equals(aiOptimized);
    }
}