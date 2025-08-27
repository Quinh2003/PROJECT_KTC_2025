package ktc.spring_project.dtos.order;

import jakarta.validation.constraints.*;
import ktc.spring_project.enums.TransportMode;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for creating new delivery orders
 */
public class CreateDeliveryOrderRequestDTO {
    
    @NotBlank(message = "Order code is required")
    @Size(max = 50, message = "Order code must not exceed 50 characters")
    private String orderCode;
    
    @NotBlank(message = "Pickup address is required")
    @Size(max = 255, message = "Pickup address must not exceed 255 characters")
    private String pickupAddress;
    
    private Double pickupLatitude;
    private Double pickupLongitude;
    
    @NotBlank(message = "Delivery address is required")
    @Size(max = 255, message = "Delivery address must not exceed 255 characters")
    private String deliveryAddress;
    
    private Double deliveryLatitude;
    private Double deliveryLongitude;
    
    @NotBlank(message = "Recipient name is required")
    @Size(max = 100, message = "Recipient name must not exceed 100 characters")
    private String recipientName;
    
    @Pattern(regexp = "^[+]?[0-9\\s\\-\\(\\)]{0,20}$", message = "Invalid phone number format")
    private String recipientPhone;
    
    @Size(max = 1000, message = "Description must not exceed 1000 characters")
    private String description;
    
    @DecimalMin(value = "0.0", inclusive = false, message = "Total amount must be greater than 0")
    private BigDecimal totalAmount;
    
    @DecimalMin(value = "0.0", inclusive = true, message = "Shipping fee must be 0 or greater")
    private BigDecimal shippingFee;
    
    private TransportMode transportMode;
    
    private Timestamp scheduledTime;
    
    @Size(max = 500, message = "Notes must not exceed 500 characters")
    private String notes;
    
    @NotNull(message = "Customer ID is required")
    private Long customerId;
    
    private Long vehicleId;
    private Long driverId;
    private Long routeId;
    
    // Constructors
    public CreateDeliveryOrderRequestDTO() {}
    
    public CreateDeliveryOrderRequestDTO(String orderCode, String pickupAddress, String deliveryAddress, 
                                       String recipientName, BigDecimal totalAmount, Long customerId) {
        this.orderCode = orderCode;
        this.pickupAddress = pickupAddress;
        this.deliveryAddress = deliveryAddress;
        this.recipientName = recipientName;
        this.totalAmount = totalAmount;
        this.customerId = customerId;
    }
    
    // Getters and Setters
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
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getCustomerId() { return customerId; }
    public void setCustomerId(Long customerId) { this.customerId = customerId; }
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public Long getDriverId() { return driverId; }
    public void setDriverId(Long driverId) { this.driverId = driverId; }
    
    public Long getRouteId() { return routeId; }
    public void setRouteId(Long routeId) { this.routeId = routeId; }
}
