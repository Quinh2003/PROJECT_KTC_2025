package ktc.spring_project.dtos.order;

import ktc.spring_project.enums.TransportMode;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO for Order response data (replaces DeliveryOrderResponseDTO)
 * Used when returning order information to clients
 */
public class OrderResponseDTO {
    
    private Long id;
    private String orderCode;
    private String statusCode; // From Status entity
    private String statusDescription;
    
    // Addresses and coordinates
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
    private BigDecimal totalAmount; // Total order value
    private BigDecimal shippingFee; // Shipping fee
    private TransportMode transportMode;
    private LocalDateTime scheduledTime;
    private LocalDateTime actualDeliveryTime;
    private String notes;
    
    // Timestamps
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    
    // User information
    private String createdByName;
    private String customerName;
    private String driverName;
    
    // Vehicle and route information
    private Long vehicleId;
    private String vehicleLicensePlate;
    private Long routeId;
    private String routeName;
    private Long trackingId;
    
    // Order items
    private List<OrderItemDTO> orderItems;
    
    // Business indicators
    private Boolean isOverdue;
    private Boolean canBeCancelled;
    private String transportModeDisplayName;
    
    // Nested DTO for order items
    public static class OrderItemDTO {
        private Long id;
        private Long productId;
        private String productCode;
        private String productName;
        private Integer quantity;
        private BigDecimal unitPrice;
        private BigDecimal subtotal;
        private BigDecimal weightKg;
        private BigDecimal shippingFee;
        private String notes;
        
        // Constructors
        public OrderItemDTO() {}
        
        // Getters and Setters
        public Long getId() {
            return id;
        }
        
        public void setId(Long id) {
            this.id = id;
        }
        
        public Long getProductId() {
            return productId;
        }
        
        public void setProductId(Long productId) {
            this.productId = productId;
        }
        
        public String getProductCode() {
            return productCode;
        }
        
        public void setProductCode(String productCode) {
            this.productCode = productCode;
        }
        
        public String getProductName() {
            return productName;
        }
        
        public void setProductName(String productName) {
            this.productName = productName;
        }
        
        public Integer getQuantity() {
            return quantity;
        }
        
        public void setQuantity(Integer quantity) {
            this.quantity = quantity;
        }
        
        public BigDecimal getUnitPrice() {
            return unitPrice;
        }
        
        public void setUnitPrice(BigDecimal unitPrice) {
            this.unitPrice = unitPrice;
        }
        
        public BigDecimal getSubtotal() {
            return subtotal;
        }
        
        public void setSubtotal(BigDecimal subtotal) {
            this.subtotal = subtotal;
        }
        
        public BigDecimal getWeightKg() {
            return weightKg;
        }
        
        public void setWeightKg(BigDecimal weightKg) {
            this.weightKg = weightKg;
        }
        
        public BigDecimal getShippingFee() {
            return shippingFee;
        }
        
        public void setShippingFee(BigDecimal shippingFee) {
            this.shippingFee = shippingFee;
        }
        
        public String getNotes() {
            return notes;
        }
        
        public void setNotes(String notes) {
            this.notes = notes;
        }
    }
    
    // Constructors
    public OrderResponseDTO() {}
    
    // Calculate derived fields
    private void calculateDerivedFields() {
        this.isOverdue = scheduledTime != null && scheduledTime.isBefore(LocalDateTime.now()) 
                        && !"DELIVERED".equals(statusCode) && !"CANCELLED".equals(statusCode);
        this.canBeCancelled = "PENDING".equals(statusCode) || "READY".equals(statusCode);
        this.transportModeDisplayName = transportMode != null ? transportMode.getDisplayName() : null;
    }
    
    // Getters and Setters
    public Long getId() {
        return id;
    }
    
    public void setId(Long id) {
        this.id = id;
    }
    
    public String getOrderCode() {
        return orderCode;
    }
    
    public void setOrderCode(String orderCode) {
        this.orderCode = orderCode;
    }
    
    public String getStatusCode() {
        return statusCode;
    }
    
    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
        calculateDerivedFields();
    }
    
    public String getStatusDescription() {
        return statusDescription;
    }
    
    public void setStatusDescription(String statusDescription) {
        this.statusDescription = statusDescription;
    }
    
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
    
    public BigDecimal getTotalAmount() {
        return totalAmount;
    }
    
    public void setTotalAmount(BigDecimal totalAmount) {
        this.totalAmount = totalAmount;
    }
    
    public BigDecimal getShippingFee() {
        return shippingFee;
    }
    
    public void setShippingFee(BigDecimal shippingFee) {
        this.shippingFee = shippingFee;
    }
    
    public TransportMode getTransportMode() {
        return transportMode;
    }
    
    public void setTransportMode(TransportMode transportMode) {
        this.transportMode = transportMode;
        calculateDerivedFields();
    }
    
    public LocalDateTime getScheduledTime() {
        return scheduledTime;
    }
    
    public void setScheduledTime(LocalDateTime scheduledTime) {
        this.scheduledTime = scheduledTime;
        calculateDerivedFields();
    }
    
    public LocalDateTime getActualDeliveryTime() {
        return actualDeliveryTime;
    }
    
    public void setActualDeliveryTime(LocalDateTime actualDeliveryTime) {
        this.actualDeliveryTime = actualDeliveryTime;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    public LocalDateTime getCreatedAt() {
        return createdAt;
    }
    
    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }
    
    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }
    
    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
    
    public String getCreatedByName() {
        return createdByName;
    }
    
    public void setCreatedByName(String createdByName) {
        this.createdByName = createdByName;
    }
    
    public String getCustomerName() {
        return customerName;
    }
    
    public void setCustomerName(String customerName) {
        this.customerName = customerName;
    }
    
    public String getDriverName() {
        return driverName;
    }
    
    public void setDriverName(String driverName) {
        this.driverName = driverName;
    }
    
    public Long getVehicleId() {
        return vehicleId;
    }
    
    public void setVehicleId(Long vehicleId) {
        this.vehicleId = vehicleId;
    }
    
    public String getVehicleLicensePlate() {
        return vehicleLicensePlate;
    }
    
    public void setVehicleLicensePlate(String vehicleLicensePlate) {
        this.vehicleLicensePlate = vehicleLicensePlate;
    }
    
    public Long getRouteId() {
        return routeId;
    }
    
    public void setRouteId(Long routeId) {
        this.routeId = routeId;
    }
    
    public String getRouteName() {
        return routeName;
    }
    
    public void setRouteName(String routeName) {
        this.routeName = routeName;
    }
    
    public Long getTrackingId() {
        return trackingId;
    }
    
    public void setTrackingId(Long trackingId) {
        this.trackingId = trackingId;
    }
    
    public List<OrderItemDTO> getOrderItems() {
        return orderItems;
    }
    
    public void setOrderItems(List<OrderItemDTO> orderItems) {
        this.orderItems = orderItems;
    }
    
    public Boolean getIsOverdue() {
        return isOverdue;
    }
    
    public Boolean getCanBeCancelled() {
        return canBeCancelled;
    }
    
    public String getTransportModeDisplayName() {
        return transportModeDisplayName;
    }
    
    @Override
    public String toString() {
        return "OrderResponseDTO{" +
                "id=" + id +
                ", orderCode='" + orderCode + '\'' +
                ", statusCode='" + statusCode + '\'' +
                ", deliveryAddress='" + deliveryAddress + '\'' +
                ", recipientName='" + recipientName + '\'' +
                ", totalAmount=" + totalAmount +
                ", transportMode=" + transportMode +
                ", isOverdue=" + isOverdue +
                '}';
    }
}