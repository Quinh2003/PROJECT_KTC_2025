package ktc.spring_project.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.time.LocalDate;

@Entity
@Table(name = "kpi_metrics")
public class KPIMetrics {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "metric_name", nullable = false)
    private String metricName; // DELIVERY_SUCCESS_RATE, AVG_DELIVERY_TIME, etc.
    
    @Column(name = "metric_value", nullable = false)
    private Double metricValue;
    
    @Column(name = "metric_unit")
    private String metricUnit; // PERCENTAGE, MINUTES, COUNT, etc.
    
    @Column(name = "calculation_date", nullable = false)
    private LocalDate calculationDate;
    
    @Column(name = "period_type")
    private String periodType; // DAILY, WEEKLY, MONTHLY, YEARLY
    
    @Column(name = "period_start")
    private LocalDate periodStart;
    
    @Column(name = "period_end")
    private LocalDate periodEnd;
    
    @Column(columnDefinition = "TEXT")
    private String details; // JSON details of calculation
    
    @Column(name = "calculated_at")
    private LocalDateTime calculatedAt;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @ManyToOne
    @JoinColumn(name = "user_id") // For user-specific metrics (driver performance)
    private User user;
    
    @ManyToOne
    @JoinColumn(name = "vehicle_id") // For vehicle-specific metrics
    private Vehicle vehicle;
    
    // Constructors
    public KPIMetrics() {
        this.createdAt = LocalDateTime.now();
        this.calculatedAt = LocalDateTime.now();
        this.calculationDate = LocalDate.now();
    }
    
    public KPIMetrics(String metricName, Double metricValue, String metricUnit) {
        this();
        this.metricName = metricName;
        this.metricValue = metricValue;
        this.metricUnit = metricUnit;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getMetricName() { return metricName; }
    public void setMetricName(String metricName) { this.metricName = metricName; }
    
    public Double getMetricValue() { return metricValue; }
    public void setMetricValue(Double metricValue) { this.metricValue = metricValue; }
    
    public String getMetricUnit() { return metricUnit; }
    public void setMetricUnit(String metricUnit) { this.metricUnit = metricUnit; }
    
    public LocalDate getCalculationDate() { return calculationDate; }
    public void setCalculationDate(LocalDate calculationDate) { this.calculationDate = calculationDate; }
    
    public String getPeriodType() { return periodType; }
    public void setPeriodType(String periodType) { this.periodType = periodType; }
    
    public LocalDate getPeriodStart() { return periodStart; }
    public void setPeriodStart(LocalDate periodStart) { this.periodStart = periodStart; }
    
    public LocalDate getPeriodEnd() { return periodEnd; }
    public void setPeriodEnd(LocalDate periodEnd) { this.periodEnd = periodEnd; }
    
    public String getDetails() { return details; }
    public void setDetails(String details) { this.details = details; }
    
    public LocalDateTime getCalculatedAt() { return calculatedAt; }
    public void setCalculatedAt(LocalDateTime calculatedAt) { this.calculatedAt = calculatedAt; }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    
    // Relationship getters/setters
    public User getUser() { return user; }
    public void setUser(User user) { this.user = user; }
    
    public Vehicle getVehicle() { return vehicle; }
    public void setVehicle(Vehicle vehicle) { this.vehicle = vehicle; }
}
