package java.ktc.spring_project.dtos.tracking;

import java.ktc.spring_project.enums.TrackingStatus;
import jakarta.validation.constraints.*;
import java.time.LocalDateTime;

/**
 * DTO for creating order tracking updates
 */
public class CreateOrderTrackingRequestDTO {
    
    @NotNull(message = "Tracking status is required")
    private TrackingStatus status;
    
    @DecimalMin(value = "-90.0", message = "Latitude must be between -90 and 90")
    @DecimalMax(value = "90.0", message = "Latitude must be between -90 and 90")
    private Double latitude;
    
    @DecimalMin(value = "-180.0", message = "Longitude must be between -180 and 180")
    @DecimalMax(value = "180.0", message = "Longitude must be between -180 and 180")
    private Double longitude;
    
    @Size(max = 255, message = "Location must not exceed 255 characters")
    private String location;
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;
    
    @NotNull(message = "Delivery order ID is required")
    private Long deliveryOrderId;
    
    // Constructors
    public CreateOrderTrackingRequestDTO() {}
    
    public CreateOrderTrackingRequestDTO(TrackingStatus status, Long deliveryOrderId) {
        this.status = status;
        this.deliveryOrderId = deliveryOrderId;
    }
    
    public CreateOrderTrackingRequestDTO(TrackingStatus status, Double latitude, Double longitude, 
                                        String location, Long deliveryOrderId) {
        this.status = status;
        this.latitude = latitude;
        this.longitude = longitude;
        this.location = location;
        this.deliveryOrderId = deliveryOrderId;
    }
    
    // Getters and Setters
    public TrackingStatus getStatus() { return status; }
    public void setStatus(TrackingStatus status) { this.status = status; }
    
    public Double getLatitude() { return latitude; }
    public void setLatitude(Double latitude) { this.latitude = latitude; }
    
    public Double getLongitude() { return longitude; }
    public void setLongitude(Double longitude) { this.longitude = longitude; }
    
    public String getLocation() { return location; }
    public void setLocation(String location) { this.location = location; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getDeliveryOrderId() { return deliveryOrderId; }
    public void setDeliveryOrderId(Long deliveryOrderId) { this.deliveryOrderId = deliveryOrderId; }
}
