package ktc.spring_project.entities;

import ktc.spring_project.enums.VehicleStatus;
import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "vehicles")
public class Vehicle {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "license_plate", unique = true, nullable = false)
    private String licensePlate;
    
    @Column(name = "vehicle_type")
    private String vehicleType;
    
    private Double capacity; // tải trọng (tấn)
    
    @Column(name = "fuel_type")
    private String fuelType;
    
    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private VehicleStatus status = VehicleStatus.AVAILABLE;
    
    @Column(name = "registration_date")
    private LocalDateTime registrationDate;
    
    @Column(name = "last_maintenance_date")
    private LocalDateTime lastMaintenanceDate;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @OneToMany(mappedBy = "vehicle", cascade = CascadeType.ALL)
    private List<DeliveryOrder> deliveryOrders;

    @OneToMany(mappedBy = "vehicle", cascade = CascadeType.ALL)
    private List<VehicleMaintenance> maintenances;

    @OneToMany(mappedBy = "vehicle", cascade = CascadeType.ALL)
    private List<DispatchAssignment> dispatchAssignments;
    
    // Constructors
    public Vehicle() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }
    
    public Vehicle(String licensePlate, String vehicleType) {
        this();
        this.licensePlate = licensePlate;
        this.vehicleType = vehicleType;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getLicensePlate() { return licensePlate; }
    public void setLicensePlate(String licensePlate) { 
        this.licensePlate = licensePlate;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getVehicleType() { return vehicleType; }
    public void setVehicleType(String vehicleType) { 
        this.vehicleType = vehicleType;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Double getCapacity() { return capacity; }
    public void setCapacity(Double capacity) { 
        this.capacity = capacity;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getFuelType() { return fuelType; }
    public void setFuelType(String fuelType) { 
        this.fuelType = fuelType;
        this.updatedAt = LocalDateTime.now();
    }
    
    public VehicleStatus getStatus() { return status; }
    public void setStatus(VehicleStatus status) { 
        this.status = status;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getRegistrationDate() { return registrationDate; }
    public void setRegistrationDate(LocalDateTime registrationDate) { 
        this.registrationDate = registrationDate;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getLastMaintenanceDate() { return lastMaintenanceDate; }
    public void setLastMaintenanceDate(LocalDateTime lastMaintenanceDate) { 
        this.lastMaintenanceDate = lastMaintenanceDate;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    
    // Relationship getters/setters
    public List<DeliveryOrder> getDeliveryOrders() { return deliveryOrders; }
    public void setDeliveryOrders(List<DeliveryOrder> deliveryOrders) { this.deliveryOrders = deliveryOrders; }
    
    public List<VehicleMaintenance> getMaintenances() { return maintenances; }
    public void setMaintenances(List<VehicleMaintenance> maintenances) { this.maintenances = maintenances; }
    
    public List<DispatchAssignment> getDispatchAssignments() { return dispatchAssignments; }
    public void setDispatchAssignments(List<DispatchAssignment> dispatchAssignments) { this.dispatchAssignments = dispatchAssignments; }
}
