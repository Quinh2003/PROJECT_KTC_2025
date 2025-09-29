package ktc.spring_project.dtos.tracking;

import ktc.spring_project.dtos.vehicle.VehicleResponseDTO;
import java.sql.Timestamp;

/**
 * DTO for delivery tracking response data
 */
public class OrderTrackingResponseDTO {
    
    private Long id;
    private Double latitude;
    private Double longitude;
    private String location;
    private Timestamp arrivedAt;
    private String notes;
    
    // Vehicle information
    private VehicleResponseDTO vehicle;
    private Long vehicleId;
    private String vehicleLicensePlate;
    
    // Status information
    private Long statusId;
    private String statusCode;
    private String statusDescription;
    
    private Timestamp timestamp;
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public OrderTrackingResponseDTO() {}
    
    public OrderTrackingResponseDTO(Long id, Double latitude, Double longitude, String location, Timestamp timestamp) {
        this.id = id;
        this.latitude = latitude;
        this.longitude = longitude;
        this.location = location;
        this.timestamp = timestamp;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public Double getLatitude() { return latitude; }
    public void setLatitude(Double latitude) { this.latitude = latitude; }
    
    public Double getLongitude() { return longitude; }
    public void setLongitude(Double longitude) { this.longitude = longitude; }
    
    public String getLocation() { return location; }
    public void setLocation(String location) { this.location = location; }
    
    public Timestamp getArrivedAt() { return arrivedAt; }
    public void setArrivedAt(Timestamp arrivedAt) { this.arrivedAt = arrivedAt; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public VehicleResponseDTO getVehicle() { return vehicle; }
    public void setVehicle(VehicleResponseDTO vehicle) { this.vehicle = vehicle; }
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public String getVehicleLicensePlate() { return vehicleLicensePlate; }
    public void setVehicleLicensePlate(String vehicleLicensePlate) { this.vehicleLicensePlate = vehicleLicensePlate; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public String getStatusCode() { return statusCode; }
    public void setStatusCode(String statusCode) { this.statusCode = statusCode; }
    
    public String getStatusDescription() { return statusDescription; }
    public void setStatusDescription(String statusDescription) { this.statusDescription = statusDescription; }
    
    public Timestamp getTimestamp() { return timestamp; }
    public void setTimestamp(Timestamp timestamp) { this.timestamp = timestamp; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public boolean hasGPSCoordinates() {
        return latitude != null && longitude != null;
    }
    
    public String getCoordinatesString() {
        if (hasGPSCoordinates()) {
            return String.format("%.6f, %.6f", latitude, longitude);
        }
        return null;
    }
    
    public boolean hasArrived() {
        return arrivedAt != null;
    }
}
