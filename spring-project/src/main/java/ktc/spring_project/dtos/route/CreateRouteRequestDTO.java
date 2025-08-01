package ktc.spring_project.dtos.route;

import jakarta.validation.constraints.*;
import java.math.BigDecimal;
import java.util.List;

/**
 * DTO for Route creation requests
 * Used when creating a new optimized route
 */
public class CreateRouteRequestDTO {
    
    @NotBlank(message = "Route name is required")
    @Size(min = 2, max = 255, message = "Route name must be between 2 and 255 characters")
    private String name;
    
    @NotNull(message = "Waypoints are required")
    @Size(min = 2, message = "At least 2 waypoints are required for a route")
    private List<WaypointDTO> waypoints;
    
    @DecimalMin(value = "0.01", message = "Estimated distance must be greater than 0")
    @Digits(integer = 8, fraction = 2, message = "Estimated distance must have at most 8 integer digits and 2 decimal places")
    private BigDecimal estimatedDistance; // Total distance in km
    
    @Min(value = 1, message = "Estimated duration must be at least 1 minute")
    private Integer estimatedDuration; // Duration in minutes
    
    @DecimalMin(value = "0.01", message = "Estimated cost must be greater than 0")
    @Digits(integer = 8, fraction = 2, message = "Estimated cost must have at most 8 integer digits and 2 decimal places")
    private BigDecimal estimatedCost; // Estimated shipping cost in VND
    
    private Boolean aiOptimized = false; // Whether route is AI optimized
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;
    
    // Nested DTO for waypoints
    public static class WaypointDTO {
        @NotBlank(message = "Address is required")
        @Size(min = 5, max = 500, message = "Address must be between 5 and 500 characters")
        private String address;
        
        @NotNull(message = "Latitude is required")
        @DecimalMin(value = "-90", message = "Latitude must be between -90 and 90")
        @DecimalMax(value = "90", message = "Latitude must be between -90 and 90")
        private Double latitude;
        
        @NotNull(message = "Longitude is required")
        @DecimalMin(value = "-180", message = "Longitude must be between -180 and 180")
        @DecimalMax(value = "180", message = "Longitude must be between -180 and 180")
        private Double longitude;
        
        @Size(max = 50, message = "Terrain type must not exceed 50 characters")
        private String terrainType; // URBAN, RURAL, HIGHWAY, etc.
        
        @DecimalMin(value = "0", message = "Distance to next waypoint cannot be negative")
        private BigDecimal distanceToNext; // Distance to next waypoint in km
        
        private String completedAt; // Timestamp when waypoint was completed (ISO format)
        
        // Constructors
        public WaypointDTO() {}
        
        public WaypointDTO(String address, Double latitude, Double longitude) {
            this.address = address;
            this.latitude = latitude;
            this.longitude = longitude;
        }
        
        // Getters and Setters
        public String getAddress() {
            return address;
        }
        
        public void setAddress(String address) {
            this.address = address;
        }
        
        public Double getLatitude() {
            return latitude;
        }
        
        public void setLatitude(Double latitude) {
            this.latitude = latitude;
        }
        
        public Double getLongitude() {
            return longitude;
        }
        
        public void setLongitude(Double longitude) {
            this.longitude = longitude;
        }
        
        public String getTerrainType() {
            return terrainType;
        }
        
        public void setTerrainType(String terrainType) {
            this.terrainType = terrainType;
        }
        
        public BigDecimal getDistanceToNext() {
            return distanceToNext;
        }
        
        public void setDistanceToNext(BigDecimal distanceToNext) {
            this.distanceToNext = distanceToNext;
        }
        
        public String getCompletedAt() {
            return completedAt;
        }
        
        public void setCompletedAt(String completedAt) {
            this.completedAt = completedAt;
        }
    }
    
    // Constructors
    public CreateRouteRequestDTO() {}
    
    // Getters and Setters
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public List<WaypointDTO> getWaypoints() {
        return waypoints;
    }
    
    public void setWaypoints(List<WaypointDTO> waypoints) {
        this.waypoints = waypoints;
    }
    
    public BigDecimal getEstimatedDistance() {
        return estimatedDistance;
    }
    
    public void setEstimatedDistance(BigDecimal estimatedDistance) {
        this.estimatedDistance = estimatedDistance;
    }
    
    public Integer getEstimatedDuration() {
        return estimatedDuration;
    }
    
    public void setEstimatedDuration(Integer estimatedDuration) {
        this.estimatedDuration = estimatedDuration;
    }
    
    public BigDecimal getEstimatedCost() {
        return estimatedCost;
    }
    
    public void setEstimatedCost(BigDecimal estimatedCost) {
        this.estimatedCost = estimatedCost;
    }
    
    public Boolean getAiOptimized() {
        return aiOptimized;
    }
    
    public void setAiOptimized(Boolean aiOptimized) {
        this.aiOptimized = aiOptimized;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    @Override
    public String toString() {
        return "CreateRouteRequestDTO{" +
                "name='" + name + '\'' +
                ", waypoints=" + (waypoints != null ? waypoints.size() : 0) + " waypoints" +
                ", estimatedDistance=" + estimatedDistance +
                ", estimatedDuration=" + estimatedDuration +
                ", estimatedCost=" + estimatedCost +
                ", aiOptimized=" + aiOptimized +
                '}';
    }
}