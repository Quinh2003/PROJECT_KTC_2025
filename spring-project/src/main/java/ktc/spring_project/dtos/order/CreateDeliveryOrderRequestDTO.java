package ktc.spring_project.dtos.order;

import jakarta.validation.constraints.*;
import java.time.LocalDateTime;

/**
 * DTO for creating new delivery orders
 */
public class CreateDeliveryOrderRequestDTO {
    
    @NotBlank(message = "Order code is required")
    @Size(max = 50, message = "Order code must not exceed 50 characters")
    private String orderCode;
    
    @Size(max = 255, message = "Pickup address must not exceed 255 characters")
    private String pickupAddress;
    
    @NotBlank(message = "Delivery address is required")
    @Size(max = 255, message = "Delivery address must not exceed 255 characters")
    private String deliveryAddress;
    
    @Size(max = 100, message = "Customer name must not exceed 100 characters")
    private String customerName;
    
    @Pattern(regexp = "^[+]?[0-9\\s\\-\\(\\)]{0,20}$", message = "Invalid phone number format")
    private String customerPhone;
    
    @Size(max = 1000, message = "Description must not exceed 1000 characters")
    private String description;
    
    private LocalDateTime scheduledTime;
    
    private Long customerId;
    private Long vehicleId;
    private Long routeId;
    
    // Constructors
    public CreateDeliveryOrderRequestDTO() {}
    
    public CreateDeliveryOrderRequestDTO(String orderCode, String deliveryAddress, String customerName) {
        this.orderCode = orderCode;
        this.deliveryAddress = deliveryAddress;
        this.customerName = customerName;
    }
    
    // Getters and Setters
    public String getOrderCode() { return orderCode; }
    public void setOrderCode(String orderCode) { this.orderCode = orderCode; }
    
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
    
    public Long getCustomerId() { return customerId; }
    public void setCustomerId(Long customerId) { this.customerId = customerId; }
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public Long getRouteId() { return routeId; }
    public void setRouteId(Long routeId) { this.routeId = routeId; }
}
