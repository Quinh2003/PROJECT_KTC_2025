package ktc.spring_project.dtos.order;

import jakarta.validation.constraints.*;
import ktc.spring_project.enums.TransportMode;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;

/**
 * DTO for creating new delivery orders
 */
public class CreateDeliveryOrderRequestDTO {
    // ...existing code...
    private String orderCode;
    private String pickupAddress;
    private Double pickupLatitude;
    private Double pickupLongitude;
    private String deliveryAddress;
    private Double deliveryLatitude;
    private Double deliveryLongitude;
    private String recipientName;
    private String recipientPhone;
    private String description;
    private BigDecimal totalAmount;
    private BigDecimal shippingFee;
    private TransportMode transportMode;
    private Timestamp scheduledTime;
    private String notes;
    private Long vehicleId;
    private Long driverId;
    private Long routeId;
    
    // Thêm trường ngày lấy hàng và thời gian buổi lấy hàng
    private LocalDate pickupDate;
    private String pickupTimePeriod;

    // Thay các trường id bằng object
    private ktc.spring_project.entities.Store store;
    private ktc.spring_project.entities.Address address;
    private ktc.spring_project.entities.Status status;
    private ktc.spring_project.entities.User createdBy;
    
    // Constructors
    public CreateDeliveryOrderRequestDTO() {}
    
    public CreateDeliveryOrderRequestDTO(String orderCode, String pickupAddress, String deliveryAddress, 
                                       String recipientName, BigDecimal totalAmount) {
        this.orderCode = orderCode;
        this.pickupAddress = pickupAddress;
        this.deliveryAddress = deliveryAddress;
        this.recipientName = recipientName;
        this.totalAmount = totalAmount;
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
    
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public Long getDriverId() { return driverId; }
    public void setDriverId(Long driverId) { this.driverId = driverId; }
    
    public Long getRouteId() { return routeId; }
    public void setRouteId(Long routeId) { this.routeId = routeId; }
    
    public LocalDate getPickupDate() { return pickupDate; }
    public void setPickupDate(LocalDate pickupDate) { this.pickupDate = pickupDate; }
    
    public String getPickupTimePeriod() { return pickupTimePeriod; }
    public void setPickupTimePeriod(String pickupTimePeriod) { this.pickupTimePeriod = pickupTimePeriod; }
    
    public ktc.spring_project.entities.Store getStore() { return store; }
    public void setStore(ktc.spring_project.entities.Store store) { this.store = store; }

    public ktc.spring_project.entities.Address getAddress() { return address; }
    public void setAddress(ktc.spring_project.entities.Address address) { this.address = address; }

    public ktc.spring_project.entities.Status getStatus() { return status; }
    public void setStatus(ktc.spring_project.entities.Status status) { this.status = status; }

    public ktc.spring_project.entities.User getCreatedBy() { return createdBy; }
    public void setCreatedBy(ktc.spring_project.entities.User createdBy) { this.createdBy = createdBy; }
}
