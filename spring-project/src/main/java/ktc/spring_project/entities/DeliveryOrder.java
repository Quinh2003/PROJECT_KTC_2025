package ktc.spring_project.entities;

import ktc.spring_project.enums.OrderStatus;
import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "delivery_orders")
public class DeliveryOrder {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "order_code", unique = true, nullable = false)
    private String orderCode;
    
    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private OrderStatus status = OrderStatus.PENDING;
    
    @Column(name = "pickup_address")
    private String pickupAddress;
    
    @Column(name = "delivery_address", nullable = false)
    private String deliveryAddress;
    
    @Column(name = "customer_name")
    private String customerName;
    
    @Column(name = "customer_phone")
    private String customerPhone;
    
    private String description;
    
    @Column(name = "scheduled_time")
    private LocalDateTime scheduledTime;
    
    @Column(name = "actual_delivery_time")
    private LocalDateTime actualDeliveryTime;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @ManyToOne
    @JoinColumn(name = "created_by")
    private User createdBy;

    @ManyToOne
    @JoinColumn(name = "vehicle_id")
    private Vehicle vehicle;
    
    @ManyToOne
    @JoinColumn(name = "customer_id")
    private Customer customer;
    
    @ManyToOne
    @JoinColumn(name = "route_id")
    private Route route;

    @OneToMany(mappedBy = "deliveryOrder", cascade = CascadeType.ALL)
    private List<OrderTracking> trackings;

    @OneToMany(mappedBy = "deliveryOrder", cascade = CascadeType.ALL)
    private List<DispatchAssignment> dispatchAssignments;
    
    @OneToMany(mappedBy = "deliveryOrder", cascade = CascadeType.ALL)
    private List<DeliveryProof> deliveryProofs;
    
    // Constructors
    public DeliveryOrder() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getOrderCode() { return orderCode; }
    public void setOrderCode(String orderCode) { 
        this.orderCode = orderCode;
        this.updatedAt = LocalDateTime.now();
    }
    
    public OrderStatus getStatus() { return status; }
    public void setStatus(OrderStatus status) { 
        this.status = status;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getPickupAddress() { return pickupAddress; }
    public void setPickupAddress(String pickupAddress) { 
        this.pickupAddress = pickupAddress;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getDeliveryAddress() { return deliveryAddress; }
    public void setDeliveryAddress(String deliveryAddress) { 
        this.deliveryAddress = deliveryAddress;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getCustomerName() { return customerName; }
    public void setCustomerName(String customerName) { 
        this.customerName = customerName;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getCustomerPhone() { return customerPhone; }
    public void setCustomerPhone(String customerPhone) { 
        this.customerPhone = customerPhone;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { 
        this.description = description;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getScheduledTime() { return scheduledTime; }
    public void setScheduledTime(LocalDateTime scheduledTime) { 
        this.scheduledTime = scheduledTime;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getActualDeliveryTime() { return actualDeliveryTime; }
    public void setActualDeliveryTime(LocalDateTime actualDeliveryTime) { 
        this.actualDeliveryTime = actualDeliveryTime;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    
    // Relationship getters/setters
    public User getCreatedBy() { return createdBy; }
    public void setCreatedBy(User createdBy) { this.createdBy = createdBy; }
    
    public Vehicle getVehicle() { return vehicle; }
    public void setVehicle(Vehicle vehicle) { this.vehicle = vehicle; }
    
    public Customer getCustomer() { return customer; }
    public void setCustomer(Customer customer) { this.customer = customer; }
    
    public Route getRoute() { return route; }
    public void setRoute(Route route) { this.route = route; }
    
    public List<OrderTracking> getTrackings() { return trackings; }
    public void setTrackings(List<OrderTracking> trackings) { this.trackings = trackings; }
    
    public List<DispatchAssignment> getDispatchAssignments() { return dispatchAssignments; }
    public void setDispatchAssignments(List<DispatchAssignment> dispatchAssignments) { this.dispatchAssignments = dispatchAssignments; }
    
    public List<DeliveryProof> getDeliveryProofs() { return deliveryProofs; }
    public void setDeliveryProofs(List<DeliveryProof> deliveryProofs) { this.deliveryProofs = deliveryProofs; }
}
