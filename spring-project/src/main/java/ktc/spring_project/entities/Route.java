package ktc.spring_project.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "routes")
public class Route {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;
    
    @Column(name = "start_address", nullable = false)
    private String startAddress;
    
    @Column(name = "end_address", nullable = false)
    private String endAddress;
    
    @Column(name = "start_latitude")
    private Double startLatitude;
    
    @Column(name = "start_longitude")
    private Double startLongitude;
    
    @Column(name = "end_latitude")
    private Double endLatitude;
    
    @Column(name = "end_longitude")
    private Double endLongitude;
    
    @Column(name = "estimated_distance") // in km
    private Double estimatedDistance;
    
    @Column(name = "estimated_duration") // in minutes
    private Integer estimatedDuration;
    
    @Column(columnDefinition = "TEXT")
    private String waypoints; // JSON string of waypoints
    
    @Column(name = "is_optimized")
    private Boolean isOptimized = false;
    
    @Column(name = "optimization_score")
    private Double optimizationScore;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
    
    @ManyToOne
    @JoinColumn(name = "created_by")
    private User createdBy;
    
    @OneToMany(mappedBy = "route", cascade = CascadeType.ALL)
    private List<DeliveryOrder> deliveryOrders;
    
    // Constructors
    public Route() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }
    
    public Route(String name, String startAddress, String endAddress) {
        this();
        this.name = name;
        this.startAddress = startAddress;
        this.endAddress = endAddress;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getName() { return name; }
    public void setName(String name) { 
        this.name = name;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getStartAddress() { return startAddress; }
    public void setStartAddress(String startAddress) { 
        this.startAddress = startAddress;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getEndAddress() { return endAddress; }
    public void setEndAddress(String endAddress) { 
        this.endAddress = endAddress;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Double getStartLatitude() { return startLatitude; }
    public void setStartLatitude(Double startLatitude) { 
        this.startLatitude = startLatitude;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Double getStartLongitude() { return startLongitude; }
    public void setStartLongitude(Double startLongitude) { 
        this.startLongitude = startLongitude;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Double getEndLatitude() { return endLatitude; }
    public void setEndLatitude(Double endLatitude) { 
        this.endLatitude = endLatitude;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Double getEndLongitude() { return endLongitude; }
    public void setEndLongitude(Double endLongitude) { 
        this.endLongitude = endLongitude;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Double getEstimatedDistance() { return estimatedDistance; }
    public void setEstimatedDistance(Double estimatedDistance) { 
        this.estimatedDistance = estimatedDistance;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Integer getEstimatedDuration() { return estimatedDuration; }
    public void setEstimatedDuration(Integer estimatedDuration) { 
        this.estimatedDuration = estimatedDuration;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getWaypoints() { return waypoints; }
    public void setWaypoints(String waypoints) { 
        this.waypoints = waypoints;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Boolean getIsOptimized() { return isOptimized; }
    public void setIsOptimized(Boolean isOptimized) { 
        this.isOptimized = isOptimized;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Double getOptimizationScore() { return optimizationScore; }
    public void setOptimizationScore(Double optimizationScore) { 
        this.optimizationScore = optimizationScore;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    
    // Relationship getters/setters
    public User getCreatedBy() { return createdBy; }
    public void setCreatedBy(User createdBy) { this.createdBy = createdBy; }
    
    public List<DeliveryOrder> getDeliveryOrders() { return deliveryOrders; }
    public void setDeliveryOrders(List<DeliveryOrder> deliveryOrders) { this.deliveryOrders = deliveryOrders; }
}
