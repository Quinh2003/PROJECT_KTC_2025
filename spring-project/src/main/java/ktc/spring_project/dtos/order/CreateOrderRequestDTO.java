package ktc.spring_project.dtos.order;

import ktc.spring_project.enums.TransportMode;
import jakarta.validation.constraints.*;
import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO for Order creation requests (replaces CreateDeliveryOrderRequestDTO)
 * Used when creating a new delivery order
 */
public class CreateOrderRequestDTO {
    
    @NotBlank(message = "Pickup address is required")
    @Size(min = 10, max = 500, message = "Pickup address must be between 10 and 500 characters")
    private String pickupAddress;
    
    private Double pickupLatitude; // For AI route optimization
    private Double pickupLongitude; // For AI route optimization
    
    @NotBlank(message = "Delivery address is required")
    @Size(min = 10, max = 500, message = "Delivery address must be between 10 and 500 characters")
    private String deliveryAddress;
    
    private Double deliveryLatitude; // For AI route optimization
    private Double deliveryLongitude; // For AI route optimization
    
    @NotBlank(message = "Recipient name is required")
    @Size(min = 2, max = 255, message = "Recipient name must be between 2 and 255 characters")
    private String recipientName;
    
    @NotBlank(message = "Recipient phone is required")
    @Pattern(regexp = "^[+]?[0-9\\s\\-\\(\\)]{8,20}$", message = "Invalid phone number format")
    private String recipientPhone;
    
    @Size(max = 1000, message = "Description must not exceed 1000 characters")
    private String description;
    
    @NotNull(message = "Transport mode is required")
    private TransportMode transportMode = TransportMode.STANDARD;
    
    private LocalDateTime scheduledTime; // When to deliver
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;
    
    // Order items (products and quantities)
    @NotNull(message = "Order items are required")
    @Size(min = 1, message = "At least one order item is required")
    private List<OrderItemDTO> orderItems;
    
    // Optional assignments
    private Long customerId; // Customer who placed the order
    private Long driverId; // Assigned driver
    private Long vehicleId; // Assigned vehicle
    private Long routeId; // Planned route
    
    // Nested DTO for order items
    public static class OrderItemDTO {
        @NotNull(message = "Product ID is required")
        private Long productId;
        
        @NotNull(message = "Quantity is required")
        @Min(value = 1, message = "Quantity must be at least 1")
        private Integer quantity;
        
        @Size(max = 500, message = "Notes must not exceed 500 characters")
        private String notes;
        
        // Constructors
        public OrderItemDTO() {}
        
        public OrderItemDTO(Long productId, Integer quantity, String notes) {
            this.productId = productId;
            this.quantity = quantity;
            this.notes = notes;
        }
        
        // Getters and Setters
        public Long getProductId() {
            return productId;
        }
        
        public void setProductId(Long productId) {
            this.productId = productId;
        }
        
        public Integer getQuantity() {
            return quantity;
        }
        
        public void setQuantity(Integer quantity) {
            this.quantity = quantity;
        }
        
        public String getNotes() {
            return notes;
        }
        
        public void setNotes(String notes) {
            this.notes = notes;
        }
    }
    
    // Constructors
    public CreateOrderRequestDTO() {}
    
    // Getters and Setters
    public String getPickupAddress() {
        return pickupAddress;
    }
    
    public void setPickupAddress(String pickupAddress) {
        this.pickupAddress = pickupAddress;
    }
    
    public Double getPickupLatitude() {
        return pickupLatitude;
    }
    
    public void setPickupLatitude(Double pickupLatitude) {
        this.pickupLatitude = pickupLatitude;
    }
    
    public Double getPickupLongitude() {
        return pickupLongitude;
    }
    
    public void setPickupLongitude(Double pickupLongitude) {
        this.pickupLongitude = pickupLongitude;
    }
    
    public String getDeliveryAddress() {
        return deliveryAddress;
    }
    
    public void setDeliveryAddress(String deliveryAddress) {
        this.deliveryAddress = deliveryAddress;
    }
    
    public Double getDeliveryLatitude() {
        return deliveryLatitude;
    }
    
    public void setDeliveryLatitude(Double deliveryLatitude) {
        this.deliveryLatitude = deliveryLatitude;
    }
    
    public Double getDeliveryLongitude() {
        return deliveryLongitude;
    }
    
    public void setDeliveryLongitude(Double deliveryLongitude) {
        this.deliveryLongitude = deliveryLongitude;
    }
    
    public String getRecipientName() {
        return recipientName;
    }
    
    public void setRecipientName(String recipientName) {
        this.recipientName = recipientName;
    }
    
    public String getRecipientPhone() {
        return recipientPhone;
    }
    
    public void setRecipientPhone(String recipientPhone) {
        this.recipientPhone = recipientPhone;
    }
    
    public String getDescription() {
        return description;
    }
    
    public void setDescription(String description) {
        this.description = description;
    }
    
    public TransportMode getTransportMode() {
        return transportMode;
    }
    
    public void setTransportMode(TransportMode transportMode) {
        this.transportMode = transportMode;
    }
    
    public LocalDateTime getScheduledTime() {
        return scheduledTime;
    }
    
    public void setScheduledTime(LocalDateTime scheduledTime) {
        this.scheduledTime = scheduledTime;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    public List<OrderItemDTO> getOrderItems() {
        return orderItems;
    }
    
    public void setOrderItems(List<OrderItemDTO> orderItems) {
        this.orderItems = orderItems;
    }
    
    public Long getCustomerId() {
        return customerId;
    }
    
    public void setCustomerId(Long customerId) {
        this.customerId = customerId;
    }
    
    public Long getDriverId() {
        return driverId;
    }
    
    public void setDriverId(Long driverId) {
        this.driverId = driverId;
    }
    
    public Long getVehicleId() {
        return vehicleId;
    }
    
    public void setVehicleId(Long vehicleId) {
        this.vehicleId = vehicleId;
    }
    
    public Long getRouteId() {
        return routeId;
    }
    
    public void setRouteId(Long routeId) {
        this.routeId = routeId;
    }
    
    @Override
    public String toString() {
        return "CreateOrderRequestDTO{" +
                "pickupAddress='" + pickupAddress + '\'' +
                ", deliveryAddress='" + deliveryAddress + '\'' +
                ", recipientName='" + recipientName + '\'' +
                ", recipientPhone='" + recipientPhone + '\'' +
                ", transportMode=" + transportMode +
                ", orderItems=" + (orderItems != null ? orderItems.size() : 0) + " items" +
                '}';
    }
}