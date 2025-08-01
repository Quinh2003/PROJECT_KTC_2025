package ktc.spring_project.dtos.route;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO for Route response data
 * Used when returning route information to clients
 */
public class RouteResponseDTO {
    
    private Long id;
    private String name;
    private List<WaypointDTO> waypoints; // Parsed from JSON
    private BigDecimal estimatedDistance; // Total distance in km
    private Integer estimatedDuration; // Duration in minutes
    private BigDecimal estimatedCost; // Estimated shipping cost in VND
    private Boolean aiOptimized; // Whether route is AI optimized
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private String createdByName; // Name of user who created this route
    private String notes;
    
    // Additional business info
    private Integer assignedOrdersCount; // Number of orders using this route
    private BigDecimal efficiencyRating; // Cost per km
    private Boolean isActive; // Whether route is currently in use
    
    // Nested DTO for waypoints (same as CreateRouteRequestDTO.WaypointDTO)
    public static class WaypointDTO {
        private String address;
        private Double latitude;
        private Double longitude;
        private String terrainType;
        private BigDecimal distanceToNext;
        private String completedAt;
        
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
    public RouteResponseDTO() {}
    
    private void calculateDerivedFields() {
        if (estimatedDistance != null && estimatedCost != null && estimatedDistance.compareTo(BigDecimal.ZERO) > 0) {
            this.efficiencyRating = estimatedCost.divide(estimatedDistance, 2, BigDecimal.ROUND_HALF_UP);
        }
        this.isActive = assignedOrdersCount != null && assignedOrdersCount > 0;
    }
    
    // Getters and Setters
    public Long getId() {
        return id;
    }
    
    public void setId(Long id) {
        this.id = id;
    }
    
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
        calculateDerivedFields();
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
        calculateDerivedFields();
    }
    
    public Boolean getAiOptimized() {
        return aiOptimized;
    }
    
    public void setAiOptimized(Boolean aiOptimized) {
        this.aiOptimized = aiOptimized;
    }
    
    public LocalDateTime getCreatedAt() {
        return createdAt;
    }
    
    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }
    
    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }
    
    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
    
    public String getCreatedByName() {
        return createdByName;
    }
    
    public void setCreatedByName(String createdByName) {
        this.createdByName = createdByName;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    public Integer getAssignedOrdersCount() {
        return assignedOrdersCount;
    }
    
    public void setAssignedOrdersCount(Integer assignedOrdersCount) {
        this.assignedOrdersCount = assignedOrdersCount;
        calculateDerivedFields();
    }
    
    public BigDecimal getEfficiencyRating() {
        return efficiencyRating;
    }
    
    public Boolean getIsActive() {
        return isActive;
    }
    
    @Override
    public String toString() {
        return "RouteResponseDTO{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", waypoints=" + (waypoints != null ? waypoints.size() : 0) + " waypoints" +
                ", estimatedDistance=" + estimatedDistance +
                ", estimatedDuration=" + estimatedDuration +
                ", estimatedCost=" + estimatedCost +
                ", aiOptimized=" + aiOptimized +
                ", assignedOrdersCount=" + assignedOrdersCount +
                ", efficiencyRating=" + efficiencyRating +
                '}';
    }
}