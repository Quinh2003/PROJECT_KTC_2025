package java.ktc.spring_project.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "vehicle_maintenances")
public class VehicleMaintenance {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(columnDefinition = "TEXT")
    private String description;
    
    @Column(name = "maintenance_date")
    private LocalDateTime maintenanceDate;
    
    @Column(name = "maintenance_type")
    private String maintenanceType; // SCHEDULED, EMERGENCY, REPAIR
    
    private Double cost;
    
    @Column(name = "next_maintenance_date")
    private LocalDateTime nextMaintenanceDate;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @ManyToOne
    @JoinColumn(name = "vehicle_id")
    private Vehicle vehicle;

    @ManyToOne
    @JoinColumn(name = "creator_id")
    private User creator;
    
    // Constructors
    public VehicleMaintenance() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }
    
    public VehicleMaintenance(String description, Vehicle vehicle, User creator) {
        this();
        this.description = description;
        this.vehicle = vehicle;
        this.creator = creator;
        this.maintenanceDate = LocalDateTime.now();
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { 
        this.description = description;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getMaintenanceDate() { return maintenanceDate; }
    public void setMaintenanceDate(LocalDateTime maintenanceDate) { 
        this.maintenanceDate = maintenanceDate;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getMaintenanceType() { return maintenanceType; }
    public void setMaintenanceType(String maintenanceType) { 
        this.maintenanceType = maintenanceType;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Double getCost() { return cost; }
    public void setCost(Double cost) { 
        this.cost = cost;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getNextMaintenanceDate() { return nextMaintenanceDate; }
    public void setNextMaintenanceDate(LocalDateTime nextMaintenanceDate) { 
        this.nextMaintenanceDate = nextMaintenanceDate;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    
    // Relationship getters/setters
    public Vehicle getVehicle() { return vehicle; }
    public void setVehicle(Vehicle vehicle) { this.vehicle = vehicle; }
    
    public User getCreator() { return creator; }
    public void setCreator(User creator) { this.creator = creator; }
}
