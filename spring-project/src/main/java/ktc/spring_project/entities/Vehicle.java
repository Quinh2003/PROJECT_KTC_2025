package ktc.spring_project.entities;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "vehicles")
public class Vehicle {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "license_plate", unique = true, nullable = false, length = 20)
    private String licensePlate;
    
    @Column(name = "vehicle_type", length = 100)
    private String vehicleType;
    
    @Column(name = "capacity", precision = 10, scale = 2)
    private BigDecimal capacity; // Tải trọng (tấn)
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "status_id", nullable = false, foreignKey = @ForeignKey(name = "FK_VEHICLE_STATUS"))
    private Status status;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
    
    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;

    // Relationships
    @OneToMany(mappedBy = "vehicle", fetch = FetchType.LAZY)
    private List<Order> orders;

    @OneToMany(mappedBy = "vehicle", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<DeliveryTracking> deliveryTrackings;
    
    // Constructors
    public Vehicle() {}
    
    public Vehicle(String licensePlate, String vehicleType, BigDecimal capacity, Status status) {
        this.licensePlate = licensePlate;
        this.vehicleType = vehicleType;
        this.capacity = capacity;
        this.status = status;
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
    
    public String getLicensePlate() { 
        return licensePlate; 
    }
    
    public void setLicensePlate(String licensePlate) { 
        this.licensePlate = licensePlate;
    }
    
    public String getVehicleType() { 
        return vehicleType; 
    }
    
    public void setVehicleType(String vehicleType) { 
        this.vehicleType = vehicleType;
    }
    
    public BigDecimal getCapacity() { 
        return capacity; 
    }
    
    public void setCapacity(BigDecimal capacity) { 
        this.capacity = capacity;
    }
    
    public Status getStatus() { 
        return status; 
    }
    
    public void setStatus(Status status) { 
        this.status = status;
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
    
    public List<DeliveryTracking> getDeliveryTrackings() { 
        return deliveryTrackings; 
    }
    
    public void setDeliveryTrackings(List<DeliveryTracking> deliveryTrackings) { 
        this.deliveryTrackings = deliveryTrackings; 
    }
    
    @Override
    public String toString() {
        return "Vehicle{" +
                "id=" + id +
                ", licensePlate='" + licensePlate + '\'' +
                ", vehicleType='" + vehicleType + '\'' +
                ", capacity=" + capacity +
                ", status=" + (status != null ? status.getCode() : "null") +
                ", createdAt=" + createdAt +
                ", updatedAt=" + updatedAt +
                ", notes='" + notes + '\'' +
                '}';
    }
}
