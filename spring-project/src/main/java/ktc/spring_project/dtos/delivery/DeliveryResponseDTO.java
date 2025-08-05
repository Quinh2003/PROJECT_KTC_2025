package ktc.spring_project.dtos.delivery;

import ktc.spring_project.enums.TransportMode;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.enums.DeliveryStatus;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for delivery response data
 */
public class DeliveryResponseDTO {
    
    private Long id;
    
    // Order information
    private Long orderId;
    private String orderNumber;
    private String orderDescription;
    
    private BigDecimal deliveryFee;
    private TransportMode transportMode;
    private ServiceType serviceType;
    private Timestamp pickupDate;
    private Timestamp scheduleDeliveryTime;
    private Timestamp actualDeliveryTime;
    private Boolean lateDeliveryRisk;
    private DeliveryStatus deliveryStatus;
    private Integer deliveryAttempts;
    private String deliveryNotes;
    private Timestamp orderDate;
    
    // Vehicle information
    private Long vehicleId;
    private String vehicleLicensePlate;
    private String vehicleType;
    
    // Driver information
    private Long driverId;
    private String driverName;
    private String driverPhone;
    
    // Tracking information
    private Long trackingId;
    private String currentLocation;
    
    // Route information
    private Long routeId;
    private String routeName;
    private BigDecimal estimatedDistance;
    private Integer estimatedDuration;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public DeliveryResponseDTO() {}
    
    public DeliveryResponseDTO(Long id, Long orderId, DeliveryStatus deliveryStatus, 
                              Timestamp scheduleDeliveryTime, String vehicleLicensePlate) {
        this.id = id;
        this.orderId = orderId;
        this.deliveryStatus = deliveryStatus;
        this.scheduleDeliveryTime = scheduleDeliveryTime;
        this.vehicleLicensePlate = vehicleLicensePlate;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public String getOrderNumber() { return orderNumber; }
    public void setOrderNumber(String orderNumber) { this.orderNumber = orderNumber; }
    
    public String getOrderDescription() { return orderDescription; }
    public void setOrderDescription(String orderDescription) { this.orderDescription = orderDescription; }
    
    public BigDecimal getDeliveryFee() { return deliveryFee; }
    public void setDeliveryFee(BigDecimal deliveryFee) { this.deliveryFee = deliveryFee; }
    
    public TransportMode getTransportMode() { return transportMode; }
    public void setTransportMode(TransportMode transportMode) { this.transportMode = transportMode; }
    
    public ServiceType getServiceType() { return serviceType; }
    public void setServiceType(ServiceType serviceType) { this.serviceType = serviceType; }
    
    public Timestamp getPickupDate() { return pickupDate; }
    public void setPickupDate(Timestamp pickupDate) { this.pickupDate = pickupDate; }
    
    public Timestamp getScheduleDeliveryTime() { return scheduleDeliveryTime; }
    public void setScheduleDeliveryTime(Timestamp scheduleDeliveryTime) { this.scheduleDeliveryTime = scheduleDeliveryTime; }
    
    public Timestamp getActualDeliveryTime() { return actualDeliveryTime; }
    public void setActualDeliveryTime(Timestamp actualDeliveryTime) { this.actualDeliveryTime = actualDeliveryTime; }
    
    public Boolean getLateDeliveryRisk() { return lateDeliveryRisk; }
    public void setLateDeliveryRisk(Boolean lateDeliveryRisk) { this.lateDeliveryRisk = lateDeliveryRisk; }
    
    public DeliveryStatus getDeliveryStatus() { return deliveryStatus; }
    public void setDeliveryStatus(DeliveryStatus deliveryStatus) { this.deliveryStatus = deliveryStatus; }
    
    public Integer getDeliveryAttempts() { return deliveryAttempts; }
    public void setDeliveryAttempts(Integer deliveryAttempts) { this.deliveryAttempts = deliveryAttempts; }
    
    public String getDeliveryNotes() { return deliveryNotes; }
    public void setDeliveryNotes(String deliveryNotes) { this.deliveryNotes = deliveryNotes; }
    
    public Timestamp getOrderDate() { return orderDate; }
    public void setOrderDate(Timestamp orderDate) { this.orderDate = orderDate; }
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public String getVehicleLicensePlate() { return vehicleLicensePlate; }
    public void setVehicleLicensePlate(String vehicleLicensePlate) { this.vehicleLicensePlate = vehicleLicensePlate; }
    
    public String getVehicleType() { return vehicleType; }
    public void setVehicleType(String vehicleType) { this.vehicleType = vehicleType; }
    
    public Long getDriverId() { return driverId; }
    public void setDriverId(Long driverId) { this.driverId = driverId; }
    
    public String getDriverName() { return driverName; }
    public void setDriverName(String driverName) { this.driverName = driverName; }
    
    public String getDriverPhone() { return driverPhone; }
    public void setDriverPhone(String driverPhone) { this.driverPhone = driverPhone; }
    
    public Long getTrackingId() { return trackingId; }
    public void setTrackingId(Long trackingId) { this.trackingId = trackingId; }
    
    public String getCurrentLocation() { return currentLocation; }
    public void setCurrentLocation(String currentLocation) { this.currentLocation = currentLocation; }
    
    public Long getRouteId() { return routeId; }
    public void setRouteId(Long routeId) { this.routeId = routeId; }
    
    public String getRouteName() { return routeName; }
    public void setRouteName(String routeName) { this.routeName = routeName; }
    
    public BigDecimal getEstimatedDistance() { return estimatedDistance; }
    public void setEstimatedDistance(BigDecimal estimatedDistance) { this.estimatedDistance = estimatedDistance; }
    
    public Integer getEstimatedDuration() { return estimatedDuration; }
    public void setEstimatedDuration(Integer estimatedDuration) { this.estimatedDuration = estimatedDuration; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public boolean isCompleted() {
        return DeliveryStatus.DELIVERED.equals(deliveryStatus);
    }
    
    public boolean isPending() {
        return DeliveryStatus.PENDING.equals(deliveryStatus);
    }
    
    public boolean isInTransit() {
        return DeliveryStatus.IN_TRANSIT.equals(deliveryStatus);
    }
    
    public boolean isDelayed() {
        return actualDeliveryTime != null && scheduleDeliveryTime != null &&
               actualDeliveryTime.after(scheduleDeliveryTime);
    }
    
    public boolean isOnTime() {
        return actualDeliveryTime != null && scheduleDeliveryTime != null &&
               !actualDeliveryTime.after(scheduleDeliveryTime);
    }
    
    public boolean hasMultipleAttempts() {
        return deliveryAttempts != null && deliveryAttempts > 1;
    }
    
    public boolean isHighRisk() {
        return Boolean.TRUE.equals(lateDeliveryRisk);
    }
    
    public boolean hasDriver() {
        return driverId != null;
    }
    
    public boolean hasTracking() {
        return trackingId != null;
    }
    
    public boolean hasRoute() {
        return routeId != null;
    }
    
    public String getStatusDisplay() {
        return deliveryStatus != null ? deliveryStatus.name().replace("_", " ") : "Unknown";
    }
    
    public String getServiceTypeDisplay() {
        return serviceType != null ? serviceType.name().replace("_", " ") : "Standard";
    }
    
    public String getTransportModeDisplay() {
        return transportMode != null ? transportMode.name().replace("_", " ") : "Road";
    }
    
    public String getFormattedFee() {
        return deliveryFee != null ? String.format("$%.2f", deliveryFee) : "No fee";
    }
    
    public String getVehicleDisplay() {
        if (vehicleLicensePlate != null) {
            return vehicleType != null ? 
                String.format("%s (%s)", vehicleLicensePlate, vehicleType) : 
                vehicleLicensePlate;
        }
        return "No vehicle assigned";
    }
    
    public String getDriverDisplay() {
        if (driverName != null) {
            return driverPhone != null ? 
                String.format("%s (%s)", driverName, driverPhone) : 
                driverName;
        }
        return "No driver assigned";
    }
    
    public String getEstimatedInfo() {
        StringBuilder info = new StringBuilder();
        if (estimatedDistance != null) {
            info.append(String.format("%.1f km", estimatedDistance));
        }
        if (estimatedDuration != null) {
            if (info.length() > 0) info.append(" | ");
            info.append(String.format("%d min", estimatedDuration));
        }
        return info.length() > 0 ? info.toString() : "No estimate";
    }
}