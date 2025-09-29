package ktc.spring_project.dtos.route;

import jakarta.validation.constraints.*;
import java.math.BigDecimal;

/**
 * DTO for creating new routes
 */
public class CreateRouteRequestDTO {
    
    @NotBlank(message = "Route name is required")
    @Size(max = 100, message = "Route name must not exceed 100 characters")
    private String name;
    
    @Size(max = 1000, message = "Waypoints must not exceed 1000 characters")
    private String waypoints;
    
    @DecimalMin(value = "0.0", message = "Estimated distance must be positive")
    private BigDecimal estimatedDistance;
    
    @Min(value = 0, message = "Estimated duration must be positive")
    private Integer estimatedDuration;
    
    @DecimalMin(value = "0.0", message = "Estimated cost must be positive")
    private BigDecimal estimatedCost;
    
    private Boolean aiOptimized = false;
    
    @Size(max = 500, message = "Notes must not exceed 500 characters")
    private String notes;
    
    @NotNull(message = "Created by user ID is required")
    private Long createdByUserId;
    
    // Constructors
    public CreateRouteRequestDTO() {}
    
    public CreateRouteRequestDTO(String name, String waypoints, Long createdByUserId) {
        this.name = name;
        this.waypoints = waypoints;
        this.createdByUserId = createdByUserId;
        this.aiOptimized = false;
    }
    
    // Getters and Setters
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
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
}