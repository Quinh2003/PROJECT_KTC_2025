package ktc.spring_project.dtos.tracking;

import jakarta.validation.constraints.*;
import java.sql.Timestamp;

/**
 * DTO for creating delivery tracking updates
 */
public class CreateOrderTrackingRequestDTO {
    
    @DecimalMin(value = "-90.0", message = "Latitude must be between -90 and 90")
    @DecimalMax(value = "90.0", message = "Latitude must be between -90 and 90")
    private Double latitude;
    
    @DecimalMin(value = "-180.0", message = "Longitude must be between -180 and 180")
    @DecimalMax(value = "180.0", message = "Longitude must be between -180 and 180")
    private Double longitude;
    
    @Size(max = 255, message = "Location must not exceed 255 characters")
    private String location;
    
    private Timestamp arrivedAt;
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;
    
    @NotNull(message = "Vehicle ID is required")
    private Long vehicleId;
    
    private Long statusId;
    
    private Timestamp timestamp;
    
    // Constructors
    public CreateOrderTrackingRequestDTO() {}
    
    public CreateOrderTrackingRequestDTO(Long vehicleId, Double latitude, Double longitude, String location) {
        this.vehicleId = vehicleId;
        this.latitude = latitude;
        this.longitude = longitude;
        this.location = location;
    }
    
    // Getters and Setters
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
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public Timestamp getTimestamp() { return timestamp; }
    public void setTimestamp(Timestamp timestamp) { this.timestamp = timestamp; }
}
