
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

    // Có thể null khi đơn hàng chưa gán xe
    @ManyToOne
    @JoinColumn(name = "vehicle_id", nullable = true)
    private Vehicle vehicle;

    @ManyToOne
    @JoinColumn(name = "address_id")
    private Address address;

    @Column(columnDefinition = "TEXT")
    private String description;

    @Column(name = "total_amount", precision = 15, scale = 2)
    private BigDecimal totalAmount;

    @Column(name = "benefit_per_order", precision = 15, scale = 2)
    private BigDecimal benefitPerOrder;

    @Column(name = "order_profit_per_order", precision = 15, scale = 2)
    private BigDecimal orderProfitPerOrder;

    @Column(columnDefinition = "TEXT")
    private String notes;

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
    @Column(name = "created_at")
    private Timestamp createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private Timestamp updatedAt;

    public Order() {
    }

    // Getters and Setters

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public Address getAddress() { return address; }
    public void setAddress(Address address) { this.address = address; }

    public Vehicle getVehicle() { return vehicle; }
    public void setVehicle(Vehicle vehicle) { this.vehicle = vehicle; }

    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }

    public BigDecimal getTotalAmount() { return totalAmount; }
    public void setTotalAmount(BigDecimal totalAmount) { this.totalAmount = totalAmount; }

    public BigDecimal getBenefitPerOrder() { return benefitPerOrder; }
    public void setBenefitPerOrder(BigDecimal benefitPerOrder) { this.benefitPerOrder = benefitPerOrder; }

    public BigDecimal getOrderProfitPerOrder() { return orderProfitPerOrder; }
    public void setOrderProfitPerOrder(BigDecimal orderProfitPerOrder) { this.orderProfitPerOrder = orderProfitPerOrder; }

    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }

    public Status getStatus() { return status; }
    public void setStatus(Status status) { this.status = status; }

    public Store getStore() { return store; }
    public void setStore(Store store) { this.store = store; }

    public User getCreatedBy() { return createdBy; }
    public void setCreatedBy(User createdBy) { this.createdBy = createdBy; }

    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }

    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
}
