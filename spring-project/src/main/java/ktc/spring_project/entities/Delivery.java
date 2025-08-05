package ktc.spring_project.entities;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

@Entity
@Table(name = "deliveries")
public class Delivery {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "order_id")
    private Order order;

    @Column(name = "delivery_fee")
    private BigDecimal deliveryFee;

    @Column(name = "transport_mode", length = 50)
    private String transportMode;

    @Column(name = "service_type", length = 50)
    private String serviceType;

    @Column(name = "pickup_date")
    private Timestamp pickupDate;

    @Column(name = "schedule_delivery_time")
    private Timestamp scheduleDeliveryTime;

    @Column(name = "actual_delivery_time")
    private Timestamp actualDeliveryTime;

    @Column(name = "late_delivery_risk", nullable = false)
    private Integer lateDeliveryRisk;

    @Column(name = "delivery_attempts")
    private Integer deliveryAttempts;

    @Column(name = "delivery_notes", columnDefinition = "TEXT")
    private String deliveryNotes;

    @Column(name = "order_date", nullable = false)
    private Timestamp orderDate;

    @ManyToOne
    @JoinColumn(name = "vehicle_id", nullable = false)
    private Vehicle vehicle;

    @ManyToOne
    @JoinColumn(name = "driver_id")
    private User driver;

    @OneToMany(mappedBy = "delivery", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<DeliveryTracking> trackingPoints;

    @ManyToOne
    @JoinColumn(name = "route_id")
    private Route route;

    @CreationTimestamp
    @Column(name = "created_at")
    private Timestamp createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private Timestamp updatedAt;    

    public Delivery() {}

    // Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public Order getOrder() { return order; }
    public void setOrder(Order order) { this.order = order; }

    public BigDecimal getDeliveryFee() { return deliveryFee; }
    public void setDeliveryFee(BigDecimal deliveryFee) { this.deliveryFee = deliveryFee; }

    public String getTransportMode() { return transportMode; }
    public void setTransportMode(String transportMode) { this.transportMode = transportMode; }

    public String getServiceType() { return serviceType; }
    public void setServiceType(String serviceType) { this.serviceType = serviceType; }

    public Timestamp getPickupDate() { return pickupDate; }
    public void setPickupDate(Timestamp pickupDate) { this.pickupDate = pickupDate; }

    public Timestamp getScheduleDeliveryTime() { return scheduleDeliveryTime; }
    public void setScheduleDeliveryTime(Timestamp scheduleDeliveryTime) { this.scheduleDeliveryTime = scheduleDeliveryTime; }

    public Timestamp getActualDeliveryTime() { return actualDeliveryTime; }
    public void setActualDeliveryTime(Timestamp actualDeliveryTime) { this.actualDeliveryTime = actualDeliveryTime; }

    public Integer getLateDeliveryRisk() { return lateDeliveryRisk; }
    public void setLateDeliveryRisk(Integer lateDeliveryRisk) { this.lateDeliveryRisk = lateDeliveryRisk; }

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

    public List<DeliveryTracking> getTrackingPoints() { return trackingPoints; }
    public void setTrackingPoints(List<DeliveryTracking> trackingPoints) { this.trackingPoints = trackingPoints; }

    public Route getRoute() { return route; }
    public void setRoute(Route route) { this.route = route; }

    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }

    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
}
