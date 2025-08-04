package ktc.spring_project.entities;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.math.BigDecimal;
import java.sql.Timestamp;

@Entity
@Table(name = "orders")
public class Order {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String orderId;

    private String orderCustomerId;

    private String description;

    private BigDecimal totalAmount;

    private BigDecimal benefitPerOrder;

    private BigDecimal orderProfitPerOrder;


    @ManyToOne
    @JoinColumn(name = "status_id")
    private Status status;

    @ManyToOne
    @JoinColumn(name = "store_id")
    private Store store;

    @ManyToOne
    @JoinColumn(name = "created_by")
    private User createdBy;

    @CreationTimestamp
    private Timestamp createdAt;

    @UpdateTimestamp
    private Timestamp updatedAt;

    public Order() {
    }

    // Getters and Setters

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }


    public String getOrderId() { return orderId; }
    public void setOrderId(String orderId) { this.orderId = orderId; }

    public String getOrderCustomerId() { return orderCustomerId; }
    public void setOrderCustomerId(String orderCustomerId) { this.orderCustomerId = orderCustomerId; }


    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }

    public BigDecimal getTotalAmount() { return totalAmount; }
    public void setTotalAmount(BigDecimal totalAmount) { this.totalAmount = totalAmount; }



    public BigDecimal getBenefitPerOrder() { return benefitPerOrder; }
    public void setBenefitPerOrder(BigDecimal benefitPerOrder) { this.benefitPerOrder = benefitPerOrder; }

    public BigDecimal getOrderProfitPerOrder() { return orderProfitPerOrder; }
    public void setOrderProfitPerOrder(BigDecimal orderProfitPerOrder) { this.orderProfitPerOrder = orderProfitPerOrder; }

    public Status getStatus() { return status; }
    public void setStatus(Status status) { this.status = status; }

    public Store getStore() { return store; }
    public void setStore(Store store) { this.store = store; }

    public User getCreatedBy() { return createdBy; }
    public void setCreatedBy(User createdBy) { this.createdBy = createdBy; }

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
