package ktc.spring_project.dtos.dashboard;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * DTO for KPI metrics displayed on dashboard
 */
public class KPIMetricsDTO {
    
    // Delivery KPIs
    private Double deliverySuccessRate;
    private Double averageDeliveryTime; // in hours
    private Integer totalDeliveries;
    private Integer completedDeliveries;
    private Integer pendingDeliveries;
    private Integer failedDeliveries;
    
    // Vehicle KPIs
    private Integer totalVehicles;
    private Integer availableVehicles;
    private Integer vehiclesInUse;
    private Integer vehiclesUnderMaintenance;
    
    // Driver KPIs
    private Integer totalDrivers;
    private Integer activeDrivers;
    private Double averageDriverRating;
    
    // Time period info
    private String periodType; // DAILY, WEEKLY, MONTHLY
    private LocalDateTime calculatedAt;
    private Map<String, Object> additionalMetrics;
    
    // Constructors
    public KPIMetricsDTO() {
        this.calculatedAt = LocalDateTime.now();
    }
    
    // Getters and Setters
    public Double getDeliverySuccessRate() { return deliverySuccessRate; }
    public void setDeliverySuccessRate(Double deliverySuccessRate) { this.deliverySuccessRate = deliverySuccessRate; }
    
    public Double getAverageDeliveryTime() { return averageDeliveryTime; }
    public void setAverageDeliveryTime(Double averageDeliveryTime) { this.averageDeliveryTime = averageDeliveryTime; }
    
    public Integer getTotalDeliveries() { return totalDeliveries; }
    public void setTotalDeliveries(Integer totalDeliveries) { this.totalDeliveries = totalDeliveries; }
    
    public Integer getCompletedDeliveries() { return completedDeliveries; }
    public void setCompletedDeliveries(Integer completedDeliveries) { this.completedDeliveries = completedDeliveries; }
    
    public Integer getPendingDeliveries() { return pendingDeliveries; }
    public void setPendingDeliveries(Integer pendingDeliveries) { this.pendingDeliveries = pendingDeliveries; }
    
    public Integer getFailedDeliveries() { return failedDeliveries; }
    public void setFailedDeliveries(Integer failedDeliveries) { this.failedDeliveries = failedDeliveries; }
    
    public Integer getTotalVehicles() { return totalVehicles; }
    public void setTotalVehicles(Integer totalVehicles) { this.totalVehicles = totalVehicles; }
    
    public Integer getAvailableVehicles() { return availableVehicles; }
    public void setAvailableVehicles(Integer availableVehicles) { this.availableVehicles = availableVehicles; }
    
    public Integer getVehiclesInUse() { return vehiclesInUse; }
    public void setVehiclesInUse(Integer vehiclesInUse) { this.vehiclesInUse = vehiclesInUse; }
    
    public Integer getVehiclesUnderMaintenance() { return vehiclesUnderMaintenance; }
    public void setVehiclesUnderMaintenance(Integer vehiclesUnderMaintenance) { this.vehiclesUnderMaintenance = vehiclesUnderMaintenance; }
    
    public Integer getTotalDrivers() { return totalDrivers; }
    public void setTotalDrivers(Integer totalDrivers) { this.totalDrivers = totalDrivers; }
    
    public Integer getActiveDrivers() { return activeDrivers; }
    public void setActiveDrivers(Integer activeDrivers) { this.activeDrivers = activeDrivers; }
    
    public Double getAverageDriverRating() { return averageDriverRating; }
    public void setAverageDriverRating(Double averageDriverRating) { this.averageDriverRating = averageDriverRating; }
    
    public String getPeriodType() { return periodType; }
    public void setPeriodType(String periodType) { this.periodType = periodType; }
    
    public LocalDateTime getCalculatedAt() { return calculatedAt; }
    public void setCalculatedAt(LocalDateTime calculatedAt) { this.calculatedAt = calculatedAt; }
    
    public Map<String, Object> getAdditionalMetrics() { return additionalMetrics; }
    public void setAdditionalMetrics(Map<String, Object> additionalMetrics) { this.additionalMetrics = additionalMetrics; }
    
    // Utility methods
    public Double getVehicleUtilizationRate() {
        if (totalVehicles == null || totalVehicles == 0) return 0.0;
        return (vehiclesInUse != null ? vehiclesInUse.doubleValue() : 0.0) / totalVehicles * 100;
    }
    
    public Double getDriverUtilizationRate() {
        if (totalDrivers == null || totalDrivers == 0) return 0.0;
        return (activeDrivers != null ? activeDrivers.doubleValue() : 0.0) / totalDrivers * 100;
    }
}
