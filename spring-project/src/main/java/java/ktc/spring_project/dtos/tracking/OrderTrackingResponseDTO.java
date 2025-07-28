package java.ktc.spring_project.dtos.tracking;

import java.ktc.spring_project.enums.TrackingStatus;
import java.ktc.spring_project.dtos.user.UserResponseDTO;
import java.time.LocalDateTime;

/**
 * DTO for order tracking response data
 */
public class OrderTrackingResponseDTO {
    
    private Long id;
    private TrackingStatus status;
    private Double latitude;
    private Double longitude;
    private String location;
    private String notes;
    private LocalDateTime timestamp;
    private LocalDateTime createdAt;
    
    // Related info
    private Long deliveryOrderId;
    private String orderCode;
    private UserResponseDTO updatedBy;
    
    // Constructors
    public OrderTrackingResponseDTO() {}
    
    public OrderTrackingResponseDTO(Long id, TrackingStatus status, LocalDateTime timestamp) {
        this.id = id;
        this.status = status;
        this.timestamp = timestamp;
    }
    
    public OrderTrackingResponseDTO(Long id, TrackingStatus status, Double latitude, 
                                   Double longitude, String location, LocalDateTime timestamp) {
        this.id = id;
        this.status = status;
        this.latitude = latitude;
        this.longitude = longitude;
        this.location = location;
        this.timestamp = timestamp;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
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
    
    public LocalDateTime getTimestamp() { return timestamp; }
    public void setTimestamp(LocalDateTime timestamp) { this.timestamp = timestamp; }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
    
    public Long getDeliveryOrderId() { return deliveryOrderId; }
    public void setDeliveryOrderId(Long deliveryOrderId) { this.deliveryOrderId = deliveryOrderId; }
    
    public String getOrderCode() { return orderCode; }
    public void setOrderCode(String orderCode) { this.orderCode = orderCode; }
    
    public UserResponseDTO getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(UserResponseDTO updatedBy) { this.updatedBy = updatedBy; }
    
    // Utility methods
    public String getStatusDisplayName() {
        return status != null ? status.getDisplayName() : null;
    }
    
    public boolean hasGPSCoordinates() {
        return latitude != null && longitude != null;
    }
    
    public String getCoordinatesString() {
        if (hasGPSCoordinates()) {
            return String.format("%.6f, %.6f", latitude, longitude);
        }
        return null;
    }
}
