package ktc.spring_project.entities;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "routes")
public class Route {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "name", nullable = false, length = 255)
    private String name;
    
    @Column(name = "waypoints", nullable = false, columnDefinition = "JSON")
    private String waypoints; // JSON array of waypoints with terrain_type, distance_to_next, completed_at
    
    @Column(name = "estimated_distance", precision = 10, scale = 2)
    private BigDecimal estimatedDistance; // Total distance in km
    
    @Column(name = "estimated_duration")
    private Integer estimatedDuration; // Duration in minutes
    
    @Column(name = "estimated_cost", precision = 10, scale = 2)
    private BigDecimal estimatedCost; // Estimated shipping cost in VND
    
    @Column(name = "ai_optimized")
    private Boolean aiOptimized = false; // Được tối ưu bởi AI
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by", foreignKey = @ForeignKey(name = "FK_ROUTE_CREATED_BY"))
    private User createdBy;
    
    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;
    
    // Relationships
    @OneToMany(mappedBy = "route", fetch = FetchType.LAZY)
    private List<Order> orders;
    
    // Constructors
    public Route() {}
    
    public Route(String name, String waypoints, User createdBy) {
        this.name = name;
        this.waypoints = waypoints;
        this.createdBy = createdBy;
    }
    
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }
    
    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
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
    
    public String getWaypoints() { 
        return waypoints; 
    }
    
    public void setWaypoints(String waypoints) { 
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
    
    public User getCreatedBy() { 
        return createdBy; 
    }
    
    public void setCreatedBy(User createdBy) { 
        this.createdBy = createdBy; 
    }
    
    public String getNotes() { 
        return notes; 
    }
    
    public void setNotes(String notes) { 
        this.notes = notes; 
    }
    
    public List<Order> getOrders() { 
        return orders; 
    }
    
    public void setOrders(List<Order> orders) { 
        this.orders = orders; 
    }
    
    @Override
    public String toString() {
        return "Route{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", estimatedDistance=" + estimatedDistance +
                ", estimatedDuration=" + estimatedDuration +
                ", estimatedCost=" + estimatedCost +
                ", aiOptimized=" + aiOptimized +
                ", createdAt=" + createdAt +
                ", updatedAt=" + updatedAt +
                ", createdBy=" + (createdBy != null ? createdBy.getName() : "null") +
                ", notes='" + notes + '\'' +
                '}';
    }
}
