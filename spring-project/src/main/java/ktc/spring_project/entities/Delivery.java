package ktc.spring_project.entities;

import jakarta.persistence.*;
import ktc.spring_project.enums.DeliveryStatus;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.enums.TransportMode;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(name = "deliveries")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Delivery {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id", nullable = false)
    private Order order;
    
    @Column(name = "delivery_fee", precision = 15, scale = 2)
    private BigDecimal deliveryFee = BigDecimal.ZERO;
    
    @Enumerated(EnumType.STRING)
    @Column(name = "transport_mode", length = 50)
    private TransportMode transportMode = TransportMode.ROAD;
    
    @Enumerated(EnumType.STRING)
    @Column(name = "service_type", length = 50)
    private ServiceType serviceType = ServiceType.STANDARD;
    
    @Column(name = "order_date", nullable = false)
    private LocalDateTime orderDate;
    
    @Column(name = "pickup_date")
    private LocalDateTime pickupDate;
    
    @Column(name = "schedule_delivery_time")
    private LocalDateTime scheduleDeliveryTime;
    
    @Column(name = "actual_delivery_time")
    private LocalDateTime actualDeliveryTime;
    
    @Column(name = "late_delivery_risk", nullable = false)
    private Boolean lateDeliveryRisk = false;
    
    @Enumerated(EnumType.STRING)
    @Column(name = "delivery_status", length = 50)
    private DeliveryStatus deliveryStatus = DeliveryStatus.PENDING;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_id", nullable = false)
    private Vehicle vehicle;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "driver_id")
    private User driver;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tracking_id")
    private DeliveryTracking tracking;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "route_id")
    private Route route;
    
    @Column(name = "delivery_attempts")
    private Integer deliveryAttempts = 0;
    
    @Column(name = "delivery_notes", columnDefinition = "TEXT")
    private String deliveryNotes;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
    
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }
    
    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }
}
