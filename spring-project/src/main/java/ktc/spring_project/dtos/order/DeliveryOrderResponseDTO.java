package ktc.spring_project.dtos.order;

import ktc.spring_project.enums.TransportMode;
import ktc.spring_project.dtos.user.UserResponseDTO;
import ktc.spring_project.dtos.tracking.OrderTrackingResponseDTO;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for delivery order response data
 */
public class DeliveryOrderResponseDTO {
    
    private Long id;
    private String orderCode;
    
    // Location information
    private String pickupAddress;
    private Double pickupLatitude;
    private Double pickupLongitude;
    private String deliveryAddress;
    private Double deliveryLatitude;
    private Double deliveryLongitude;
    
    // Recipient information
    private String recipientName;
    private String recipientPhone;
    private String description;
    
    // Financial information
    private BigDecimal totalAmount;
    private BigDecimal shippingFee;
    private TransportMode transportMode;
    
    // Time information
    private Timestamp scheduledTime;
    private Timestamp actualDeliveryTime;
    private String notes;
    
    // Status information
    private Long statusId;
    private String statusCode;
    private String statusDescription;
    
    // Related entities (basic info only)
    private UserResponseDTO customer;
    private UserResponseDTO createdBy;
    private UserResponseDTO driver;
    
    private Long vehicleId;
    private String vehicleLicensePlate;
    private String vehicleType;
    
    private Long routeId;
    private String routeName;
    
    private OrderTrackingResponseDTO tracking;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public DeliveryOrderResponseDTO() {}
    
    public DeliveryOrderResponseDTO(Long id, String orderCode, String deliveryAddress, 
                                   String recipientName, Timestamp scheduledTime) {
        this.id = id;
        this.orderCode = orderCode;
        this.deliveryAddress = deliveryAddress;
        this.recipientName = recipientName;
        this.scheduledTime = scheduledTime;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getOrderCode() { return orderCode; }
    public void setOrderCode(String orderCode) { this.orderCode = orderCode; }
    
    public String getPickupAddress() { return pickupAddress; }
    public void setPickupAddress(String pickupAddress) { this.pickupAddress = pickupAddress; }
    
    public Double getPickupLatitude() { return pickupLatitude; }
    public void setPickupLatitude(Double pickupLatitude) { this.pickupLatitude = pickupLatitude; }
    
    public Double getPickupLongitude() { return pickupLongitude; }
    public void setPickupLongitude(Double pickupLongitude) { this.pickupLongitude = pickupLongitude; }
    
    public String getDeliveryAddress() { return deliveryAddress; }
    public void setDeliveryAddress(String deliveryAddress) { this.deliveryAddress = deliveryAddress; }
    
    public Double getDeliveryLatitude() { return deliveryLatitude; }
    public void setDeliveryLatitude(Double deliveryLatitude) { this.deliveryLatitude = deliveryLatitude; }
    
    public Double getDeliveryLongitude() { return deliveryLongitude; }
    public void setDeliveryLongitude(Double deliveryLongitude) { this.deliveryLongitude = deliveryLongitude; }
    
    public String getRecipientName() { return recipientName; }
    public void setRecipientName(String recipientName) { this.recipientName = recipientName; }
    
    public String getRecipientPhone() { return recipientPhone; }
    public void setRecipientPhone(String recipientPhone) { this.recipientPhone = recipientPhone; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public BigDecimal getTotalAmount() { return totalAmount; }
    public void setTotalAmount(BigDecimal totalAmount) { this.totalAmount = totalAmount; }
    
    public BigDecimal getShippingFee() { return shippingFee; }
    public void setShippingFee(BigDecimal shippingFee) { this.shippingFee = shippingFee; }
    
    public TransportMode getTransportMode() { return transportMode; }
    public void setTransportMode(TransportMode transportMode) { this.transportMode = transportMode; }
    
    public Timestamp getScheduledTime() { return scheduledTime; }
    public void setScheduledTime(Timestamp scheduledTime) { this.scheduledTime = scheduledTime; }
    
    public Timestamp getActualDeliveryTime() { return actualDeliveryTime; }
    public void setActualDeliveryTime(Timestamp actualDeliveryTime) { this.actualDeliveryTime = actualDeliveryTime; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public String getStatusCode() { return statusCode; }
    public void setStatusCode(String statusCode) { this.statusCode = statusCode; }
    
    public String getStatusDescription() { return statusDescription; }
    public void setStatusDescription(String statusDescription) { this.statusDescription = statusDescription; }
    
    public UserResponseDTO getCustomer() { return customer; }
    public void setCustomer(UserResponseDTO customer) { this.customer = customer; }
    
    public UserResponseDTO getCreatedBy() { return createdBy; }
    public void setCreatedBy(UserResponseDTO createdBy) { this.createdBy = createdBy; }
    
    public UserResponseDTO getDriver() { return driver; }
    public void setDriver(UserResponseDTO driver) { this.driver = driver; }
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public String getVehicleLicensePlate() { return vehicleLicensePlate; }
    public void setVehicleLicensePlate(String vehicleLicensePlate) { this.vehicleLicensePlate = vehicleLicensePlate; }
    
    public String getVehicleType() { return vehicleType; }
    public void setVehicleType(String vehicleType) { this.vehicleType = vehicleType; }
    
    public Long getRouteId() { return routeId; }
    public void setRouteId(Long routeId) { this.routeId = routeId; }
    
    public String getRouteName() { return routeName; }
    public void setRouteName(String routeName) { this.routeName = routeName; }
    
    public OrderTrackingResponseDTO getTracking() { return tracking; }
    public void setTracking(OrderTrackingResponseDTO tracking) { this.tracking = tracking; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public String getTransportModeDisplayName() {
        return transportMode != null ? transportMode.getDisplayName() : null;
    }
    
    public boolean hasLocation() {
        return pickupLatitude != null && pickupLongitude != null 
            && deliveryLatitude != null && deliveryLongitude != null;
    }
    
    public boolean isScheduled() {
        return scheduledTime != null;
    }
    
    public boolean isDelivered() {
        return actualDeliveryTime != null;
    }
}
