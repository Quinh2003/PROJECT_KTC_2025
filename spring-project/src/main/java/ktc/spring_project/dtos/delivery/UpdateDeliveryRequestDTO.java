package ktc.spring_project.dtos.delivery;

import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.enums.TransportMode;

import java.math.BigDecimal;
import java.sql.Timestamp;
import com.fasterxml.jackson.annotation.JsonFormat;

/**
 * DTO for updating an existing delivery without using DeliveryStatus enum
 */
public class UpdateDeliveryRequestDTO {

    private BigDecimal deliveryFee;
    private TransportMode transportMode;
    private ServiceType serviceType;
    
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Timestamp pickupDate;
    
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Timestamp scheduleDeliveryTime;
    
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Timestamp actualDeliveryTime;
    private Boolean lateDeliveryRisk;
    private String deliveryStatus;  // Sử dụng String thay cho enum DeliveryStatus
    private Integer deliveryAttempts;
    private String deliveryNotes;
    private Long vehicleId;
    private Long driverId;
    private Long routeId;

    // Constructors
    public UpdateDeliveryRequestDTO() {}

    public UpdateDeliveryRequestDTO(String deliveryStatus) {
        this.deliveryStatus = deliveryStatus;
    }

    // Getters & Setters
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

    public String getDeliveryStatus() { return deliveryStatus; }
    public void setDeliveryStatus(String deliveryStatus) { this.deliveryStatus = deliveryStatus; }

    public Integer getDeliveryAttempts() { return deliveryAttempts; }
    public void setDeliveryAttempts(Integer deliveryAttempts) { this.deliveryAttempts = deliveryAttempts; }

    public String getDeliveryNotes() { return deliveryNotes; }
    public void setDeliveryNotes(String deliveryNotes) { this.deliveryNotes = deliveryNotes; }

    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }

    public Long getDriverId() { return driverId; }
    public void setDriverId(Long driverId) { this.driverId = driverId; }

    public Long getRouteId() { return routeId; }
    public void setRouteId(Long routeId) { this.routeId = routeId; }

    // Validation & Helper Methods
    public boolean hasValidFee() {
        return deliveryFee != null && deliveryFee.compareTo(BigDecimal.ZERO) >= 0;
    }

    public boolean hasScheduleUpdate() {
        return scheduleDeliveryTime != null || pickupDate != null;
    }

    public boolean hasVehicleChange() {
        return vehicleId != null;
    }

    public boolean hasDriverChange() {
        return driverId != null;
    }

    public boolean hasRouteChange() {
        return routeId != null;
    }

    public boolean isCompletingDelivery() {
        return "DELIVERED".equalsIgnoreCase(deliveryStatus);
    }

    public boolean isCancellingDelivery() {
        return "CANCELLED".equalsIgnoreCase(deliveryStatus);
    }

    public boolean isMarkingAsInTransit() {
        return "IN_TRANSIT".equalsIgnoreCase(deliveryStatus);
    }

    public boolean isMarkingAsDelayed() {
        return "DELAYED".equalsIgnoreCase(deliveryStatus);
    }

    public boolean isAddingAttempt() {
        return deliveryAttempts != null && deliveryAttempts > 0;
    }

    public boolean isMarkingHighRisk() {
        return Boolean.TRUE.equals(lateDeliveryRisk);
    }

    public boolean isRemovingRisk() {
        return Boolean.FALSE.equals(lateDeliveryRisk);
    }

    public boolean isServiceUpgrade() {
        return ServiceType.EXPRESS.equals(serviceType) || ServiceType.PRIORITY.equals(serviceType);
    }

    public boolean hasActualDeliveryTime() {
        return actualDeliveryTime != null;
    }

    public boolean isDeliveryDelayed() {
        return hasActualDeliveryTime() && scheduleDeliveryTime != null &&
               actualDeliveryTime.after(scheduleDeliveryTime);
    }

    public boolean hasNotes() {
        return deliveryNotes != null && !deliveryNotes.trim().isEmpty();
    }
}
