package ktc.spring_project.dtos.order;

import ktc.spring_project.enums.OrderStatus;
import ktc.spring_project.dtos.user.UserResponseDTO;
import java.time.LocalDateTime;

/**
 * DTO for delivery order response data
 */
public class DeliveryOrderResponseDTO {
    
    private Long id;
    private String orderCode;
    private OrderStatus status;
    private String pickupAddress;
    private String deliveryAddress;
    private String customerName;
    private String customerPhone;
    private String description;
    private LocalDateTime scheduledTime;
    private LocalDateTime actualDeliveryTime;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    
    // Related entities (basic info only)
    private UserResponseDTO createdBy;
    private Long customerId;
    private String customerDisplayName;
    private Long vehicleId;
    private String vehicleLicensePlate;
    private Long routeId;
    private String routeName;
    
    // Constructors
    public DeliveryOrderResponseDTO() {}
    
    public DeliveryOrderResponseDTO(Long id, String orderCode, OrderStatus status, 
                                   String deliveryAddress, LocalDateTime scheduledTime) {
        this.id = id;
        this.orderCode = orderCode;
        this.status = status;
        this.deliveryAddress = deliveryAddress;
        this.scheduledTime = scheduledTime;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getOrderCode() { return orderCode; }
    public void setOrderCode(String orderCode) { this.orderCode = orderCode; }
    
    public OrderStatus getStatus() { return status; }
    public void setStatus(OrderStatus status) { this.status = status; }
    
    public String getPickupAddress() { return pickupAddress; }
    public void setPickupAddress(String pickupAddress) { this.pickupAddress = pickupAddress; }
    
    public String getDeliveryAddress() { return deliveryAddress; }
    public void setDeliveryAddress(String deliveryAddress) { this.deliveryAddress = deliveryAddress; }
    
    public String getCustomerName() { return customerName; }
    public void setCustomerName(String customerName) { this.customerName = customerName; }
    
    public String getCustomerPhone() { return customerPhone; }
    public void setCustomerPhone(String customerPhone) { this.customerPhone = customerPhone; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public LocalDateTime getScheduledTime() { return scheduledTime; }
    public void setScheduledTime(LocalDateTime scheduledTime) { this.scheduledTime = scheduledTime; }
    
    public LocalDateTime getActualDeliveryTime() { return actualDeliveryTime; }
    public void setActualDeliveryTime(LocalDateTime actualDeliveryTime) { this.actualDeliveryTime = actualDeliveryTime; }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
    
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    
    public UserResponseDTO getCreatedBy() { return createdBy; }
    public void setCreatedBy(UserResponseDTO createdBy) { this.createdBy = createdBy; }
    
    public Long getCustomerId() { return customerId; }
    public void setCustomerId(Long customerId) { this.customerId = customerId; }
    
    public String getCustomerDisplayName() { return customerDisplayName; }
    public void setCustomerDisplayName(String customerDisplayName) { this.customerDisplayName = customerDisplayName; }
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public String getVehicleLicensePlate() { return vehicleLicensePlate; }
    public void setVehicleLicensePlate(String vehicleLicensePlate) { this.vehicleLicensePlate = vehicleLicensePlate; }
    
    public Long getRouteId() { return routeId; }
    public void setRouteId(Long routeId) { this.routeId = routeId; }
    
    public String getRouteName() { return routeName; }
    public void setRouteName(String routeName) { this.routeName = routeName; }
    
    // Utility methods
    public String getStatusDisplayName() {
        return status != null ? status.getDisplayName() : null;
    }
    
    public boolean isDelivered() {
        return OrderStatus.DELIVERED.equals(status);
    }
    
    public boolean isInProgress() {
        return OrderStatus.READY.equals(status) || OrderStatus.ON_DELIVERY.equals(status);
    }
}
