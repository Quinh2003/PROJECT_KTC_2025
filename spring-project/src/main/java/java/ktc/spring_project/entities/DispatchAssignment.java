package java.ktc.spring_project.entities;

import java.ktc.spring_project.enums.AssignmentStatus;
import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "dispatch_assignments")
public class DispatchAssignment {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "assigned_at")
    private LocalDateTime assignedAt;
    
    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private AssignmentStatus status = AssignmentStatus.ASSIGNED;
    
    @Column(name = "expected_completion_time")
    private LocalDateTime expectedCompletionTime;
    
    @Column(name = "actual_completion_time")
    private LocalDateTime actualCompletionTime;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @ManyToOne
    @JoinColumn(name = "delivery_order_id")
    private DeliveryOrder deliveryOrder;

    @ManyToOne
    @JoinColumn(name = "vehicle_id")
    private Vehicle vehicle;

    @ManyToOne
    @JoinColumn(name = "driver_id")
    private User driver; // role = DRIVER
    
    // Constructors
    public DispatchAssignment() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
        this.assignedAt = LocalDateTime.now();
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public LocalDateTime getAssignedAt() { return assignedAt; }
    public void setAssignedAt(LocalDateTime assignedAt) { 
        this.assignedAt = assignedAt;
        this.updatedAt = LocalDateTime.now();
    }
    
    public AssignmentStatus getStatus() { return status; }
    public void setStatus(AssignmentStatus status) { 
        this.status = status;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getExpectedCompletionTime() { return expectedCompletionTime; }
    public void setExpectedCompletionTime(LocalDateTime expectedCompletionTime) { 
        this.expectedCompletionTime = expectedCompletionTime;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getActualCompletionTime() { return actualCompletionTime; }
    public void setActualCompletionTime(LocalDateTime actualCompletionTime) { 
        this.actualCompletionTime = actualCompletionTime;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    
    // Relationship getters/setters
    public DeliveryOrder getDeliveryOrder() { return deliveryOrder; }
    public void setDeliveryOrder(DeliveryOrder deliveryOrder) { this.deliveryOrder = deliveryOrder; }
    
    public Vehicle getVehicle() { return vehicle; }
    public void setVehicle(Vehicle vehicle) { this.vehicle = vehicle; }
    
    public User getDriver() { return driver; }
    public void setDriver(User driver) { this.driver = driver; }
}
