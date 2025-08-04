package ktc.spring_project.entities;

import jakarta.persistence.*;
import ktc.spring_project.enums.TransportMode;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.enums.DeliveryStatus;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import java.math.BigDecimal;
import java.sql.Timestamp;

@Entity
@Table(name = "deliveries")
public class Delivery {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "order_id")
    private Order order;

    private BigDecimal deliveryFee;

    @Enumerated(EnumType.STRING)
    private TransportMode transportMode;

    @Enumerated(EnumType.STRING)
    private ServiceType serviceType;

    private Timestamp pickupDate;

    private Timestamp scheduleDeliveryTime;

    private Timestamp actualDeliveryTime;

    private Boolean lateDeliveryRisk;

    @Enumerated(EnumType.STRING)
    private DeliveryStatus deliveryStatus;

    private Integer deliveryAttempts;

    private String deliveryNotes;

    private Timestamp orderDate;

    @ManyToOne
    @JoinColumn(name = "vehicle_id")
    private Vehicle vehicle;

    @ManyToOne
    @JoinColumn(name = "driver_id")
    private User driver;

    @ManyToOne
    @JoinColumn(name = "tracking_id")
    private DeliveryTracking tracking;

    @ManyToOne
    @JoinColumn(name = "route_id")
    private Route route;

    @CreationTimestamp
    private Timestamp createdAt;

    @UpdateTimestamp
    private Timestamp updatedAt;

    public Delivery() {}

    // Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public Order getOrder() { return order; }
    public void setOrder(Order order) { this.order = order; }

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

    public Vehicle getVehicle() { return vehicle; }
    public void setVehicle(Vehicle vehicle) { this.vehicle = vehicle; }

    public User getDriver() { return driver; }
    public void setDriver(User driver) { this.driver = driver; }

    public DeliveryTracking getTracking() { return tracking; }
    public void setTracking(DeliveryTracking tracking) { this.tracking = tracking; }

    public Route getRoute() { return route; }
    public void setRoute(Route route) { this.route = route; }

    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }

    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
}
