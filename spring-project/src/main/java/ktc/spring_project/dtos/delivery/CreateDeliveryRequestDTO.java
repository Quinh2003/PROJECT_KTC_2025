
package ktc.spring_project.dtos.delivery;

import ktc.spring_project.enums.TransportMode;
import ktc.spring_project.enums.ServiceType;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;
import jakarta.validation.constraints.*;
import com.fasterxml.jackson.annotation.JsonFormat;

/**
 * DTO for creating a new delivery
 */
public class CreateDeliveryRequestDTO {
    
    @NotNull(message = "Order ID is required")
    private Long orderId;

    @DecimalMin(value = "0.0", inclusive = true, message = "Delivery fee must be >= 0")
    private BigDecimal deliveryFee;

    private TransportMode transportMode = TransportMode.ROAD;

    @NotNull(message = "Service type is required")
    private ServiceType serviceType = ServiceType.STANDARD;
    
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate pickupDate;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Timestamp scheduleDeliveryTime;

    private Boolean lateDeliveryRisk = false;
    @Size(max = 500, message = "Delivery notes must not exceed 500 characters")
    private String deliveryNotes;
    
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Timestamp orderDate;
    private Long vehicleId;

    private Long driverId;
    private Long routeId;
    
    // Constructors
    public CreateDeliveryRequestDTO() {}
    
    public CreateDeliveryRequestDTO(Long orderId, Long vehicleId, Timestamp scheduleDeliveryTime) {
        this.orderId = orderId;
        this.vehicleId = vehicleId;
        this.scheduleDeliveryTime = scheduleDeliveryTime;
    }
    
    // Getters and Setters
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public BigDecimal getDeliveryFee() { return deliveryFee; }
    public void setDeliveryFee(BigDecimal deliveryFee) { this.deliveryFee = deliveryFee; }
    
    public TransportMode getTransportMode() { return transportMode; }
    public void setTransportMode(TransportMode transportMode) { this.transportMode = transportMode; }
    
    public ServiceType getServiceType() { return serviceType; }
    public void setServiceType(ServiceType serviceType) { this.serviceType = serviceType; }
    
    public LocalDate getPickupDate() { return pickupDate; }
    public void setPickupDate(LocalDate pickupDate) { this.pickupDate = pickupDate; }
    
    public Timestamp getScheduleDeliveryTime() { return scheduleDeliveryTime; }
    public void setScheduleDeliveryTime(Timestamp scheduleDeliveryTime) { this.scheduleDeliveryTime = scheduleDeliveryTime; }
    
    public Boolean getLateDeliveryRisk() { return lateDeliveryRisk; }
    public void setLateDeliveryRisk(Boolean lateDeliveryRisk) { this.lateDeliveryRisk = lateDeliveryRisk; }
    
    public String getDeliveryNotes() { return deliveryNotes; }
    public void setDeliveryNotes(String deliveryNotes) { this.deliveryNotes = deliveryNotes; }
    
    public Timestamp getOrderDate() { return orderDate; }
    public void setOrderDate(Timestamp orderDate) { this.orderDate = orderDate; }
    
    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    
    public Long getDriverId() { return driverId; }
    public void setDriverId(Long driverId) { this.driverId = driverId; }
    
    public Long getRouteId() { return routeId; }
    public void setRouteId(Long routeId) { this.routeId = routeId; }
    
    // Validation methods
    public boolean isValid() {
        return orderId != null && vehicleId != null && scheduleDeliveryTime != null;
    }
    
    public boolean hasFee() {
        return deliveryFee != null && deliveryFee.compareTo(BigDecimal.ZERO) > 0;
    }
    
    public boolean hasPickupDate() {
        return pickupDate != null;
    }
    
    public boolean hasDriver() {
        return driverId != null;
    }
    
    public boolean hasRoute() {
        return routeId != null;
    }
    
    public boolean isHighRisk() {
        return Boolean.TRUE.equals(lateDeliveryRisk);
    }
    
    public boolean isExpressService() {
        return ServiceType.EXPRESS.equals(serviceType);
    }
    
    public boolean isPriorityService() {
        return ServiceType.PRIORITY.equals(serviceType);
    }
    
    public String getFormattedFee() {
        if (hasFee()) {
            return String.format("$%.2f", deliveryFee);
        }
        return "No fee specified";
    }
}
