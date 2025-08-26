package ktc.spring_project.dtos.vehicle;

import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for vehicle response data
 */
public class VehicleResponseDTO {
    
    private Long id;
    private String licensePlate;
    private String vehicleType;
    private BigDecimal capacity;
    private String notes;
    
    // Status information
    private Long statusId;
    private String statusCode;
    private String statusDescription;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public VehicleResponseDTO() {}
    
    public VehicleResponseDTO(Long id, String licensePlate, String vehicleType, BigDecimal capacity) {
        this.id = id;
        this.licensePlate = licensePlate;
        this.vehicleType = vehicleType;
        this.capacity = capacity;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getLicensePlate() { return licensePlate; }
    public void setLicensePlate(String licensePlate) { this.licensePlate = licensePlate; }
    
    public String getVehicleType() { return vehicleType; }
    public void setVehicleType(String vehicleType) { this.vehicleType = vehicleType; }
    
    public BigDecimal getCapacity() { return capacity; }
    public void setCapacity(BigDecimal capacity) { this.capacity = capacity; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public String getStatusCode() { return statusCode; }
    public void setStatusCode(String statusCode) { this.statusCode = statusCode; }
    
    public String getStatusDescription() { return statusDescription; }
    public void setStatusDescription(String statusDescription) { this.statusDescription = statusDescription; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public String getDisplayName() {
        return licensePlate + " - " + vehicleType;
    }
    
    public boolean hasCapacity() {
        return capacity != null && capacity.compareTo(BigDecimal.ZERO) > 0;
    }
}
