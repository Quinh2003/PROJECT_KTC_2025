package ktc.spring_project.dtos.vehicle;

import ktc.spring_project.enums.VehicleStatus;
import java.time.LocalDateTime;

/**
 * DTO for vehicle response data
 */
public class VehicleResponseDTO {
    
    private Long id;
    private String licensePlate;
    private String vehicleType;
    private Double capacity;
    private String fuelType;
    private VehicleStatus status;
    private LocalDateTime registrationDate;
    private LocalDateTime lastMaintenanceDate;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    
    // Statistics
    private Integer totalDeliveries;
    private Integer activeDeliveries;
    private Boolean isMaintenanceDue;
    
    // Constructors
    public VehicleResponseDTO() {}
    
    public VehicleResponseDTO(Long id, String licensePlate, String vehicleType, VehicleStatus status) {
        this.id = id;
        this.licensePlate = licensePlate;
        this.vehicleType = vehicleType;
        this.status = status;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getLicensePlate() { return licensePlate; }
    public void setLicensePlate(String licensePlate) { this.licensePlate = licensePlate; }
    
    public String getVehicleType() { return vehicleType; }
    public void setVehicleType(String vehicleType) { this.vehicleType = vehicleType; }
    
    public Double getCapacity() { return capacity; }
    public void setCapacity(Double capacity) { this.capacity = capacity; }
    
    public String getFuelType() { return fuelType; }
    public void setFuelType(String fuelType) { this.fuelType = fuelType; }
    
    public VehicleStatus getStatus() { return status; }
    public void setStatus(VehicleStatus status) { this.status = status; }
    
    public LocalDateTime getRegistrationDate() { return registrationDate; }
    public void setRegistrationDate(LocalDateTime registrationDate) { this.registrationDate = registrationDate; }
    
    public LocalDateTime getLastMaintenanceDate() { return lastMaintenanceDate; }
    public void setLastMaintenanceDate(LocalDateTime lastMaintenanceDate) { this.lastMaintenanceDate = lastMaintenanceDate; }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
    
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    
    public Integer getTotalDeliveries() { return totalDeliveries; }
    public void setTotalDeliveries(Integer totalDeliveries) { this.totalDeliveries = totalDeliveries; }
    
    public Integer getActiveDeliveries() { return activeDeliveries; }
    public void setActiveDeliveries(Integer activeDeliveries) { this.activeDeliveries = activeDeliveries; }
    
    public Boolean getIsMaintenanceDue() { return isMaintenanceDue; }
    public void setIsMaintenanceDue(Boolean isMaintenanceDue) { this.isMaintenanceDue = isMaintenanceDue; }
    
    // Utility methods
    public String getStatusDisplayName() {
        return status != null ? status.getDisplayName() : null;
    }
    
    public boolean isAvailable() {
        return VehicleStatus.AVAILABLE.equals(status);
    }
    
    public boolean isInUse() {
        return VehicleStatus.IN_USE.equals(status);
    }
    
    public boolean isUnderMaintenance() {
        return VehicleStatus.MAINTENANCE.equals(status);
    }
}
