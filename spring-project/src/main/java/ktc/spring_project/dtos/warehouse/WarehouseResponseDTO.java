package ktc.spring_project.dtos.warehouse;

import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for warehouse response data
 */
public class WarehouseResponseDTO {
    
    private Long id;

    private String name;
    private String address;
    private BigDecimal latitude;
    private BigDecimal longitude;
    private BigDecimal capacityM3;
    private Boolean isActive;
    private String notes;
    
    // Creator information
    private Long createdByUserId;
    private String createdByUserName;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Operational metrics
    private Integer totalProducts;
    private Integer totalTransactions;
    private BigDecimal currentUtilization;
    private BigDecimal utilizationPercentage;
    
    // Constructors
    public WarehouseResponseDTO() {}
    
    public WarehouseResponseDTO(Long id, String name, String address, Boolean isActive) {
        this.id = id;
        this.name = name;
        this.address = address;
        this.isActive = isActive;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    

    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getAddress() { return address; }
    public void setAddress(String address) { this.address = address; }
    
    public BigDecimal getLatitude() { return latitude; }
    public void setLatitude(BigDecimal latitude) { this.latitude = latitude; }
    
    public BigDecimal getLongitude() { return longitude; }
    public void setLongitude(BigDecimal longitude) { this.longitude = longitude; }
    
    public BigDecimal getCapacityM3() { return capacityM3; }
    public void setCapacityM3(BigDecimal capacityM3) { this.capacityM3 = capacityM3; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
    
    public String getCreatedByUserName() { return createdByUserName; }
    public void setCreatedByUserName(String createdByUserName) { this.createdByUserName = createdByUserName; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    public Integer getTotalProducts() { return totalProducts; }
    public void setTotalProducts(Integer totalProducts) { this.totalProducts = totalProducts; }
    
    public Integer getTotalTransactions() { return totalTransactions; }
    public void setTotalTransactions(Integer totalTransactions) { this.totalTransactions = totalTransactions; }
    
    public BigDecimal getCurrentUtilization() { return currentUtilization; }
    public void setCurrentUtilization(BigDecimal currentUtilization) { this.currentUtilization = currentUtilization; }
    
    public BigDecimal getUtilizationPercentage() { return utilizationPercentage; }
    public void setUtilizationPercentage(BigDecimal utilizationPercentage) { this.utilizationPercentage = utilizationPercentage; }
    
    // Utility methods
    public String getDisplayName() {
        return name;
    }
    
    public boolean isActiveWarehouse() {
        return Boolean.TRUE.equals(isActive);
    }
    
    public boolean hasCoordinates() {
        return latitude != null && longitude != null;
    }
    
    public String getCoordinatesString() {
        if (hasCoordinates()) {
            return String.format("%.6f, %.6f", latitude, longitude);
        }
        return null;
    }
    
    public boolean hasCapacity() {
        return capacityM3 != null && capacityM3.compareTo(BigDecimal.ZERO) > 0;
    }
    
    public String getFormattedCapacity() {
        if (hasCapacity()) {
            return String.format("%.2f m³", capacityM3);
        }
        return "No capacity specified";
    }
    
    public String getStatusDisplay() {
        return isActiveWarehouse() ? "Active" : "Inactive";
    }
    
    public boolean hasProducts() {
        return totalProducts != null && totalProducts > 0;
    }
    
    public boolean hasTransactions() {
        return totalTransactions != null && totalTransactions > 0;
    }
    
    public String getUtilizationStatus() {
        if (utilizationPercentage == null) {
            return "Unknown";
        }
        
        double percentage = utilizationPercentage.doubleValue();
        if (percentage >= 90) {
            return "High Utilization";
        } else if (percentage >= 70) {
            return "Medium Utilization";
        } else if (percentage >= 50) {
            return "Low Utilization";
        } else {
            return "Very Low Utilization";
        }
    }
    
    public String getFormattedUtilization() {
        if (currentUtilization != null && capacityM3 != null) {
            return String.format("%.2f/%.2f m³ (%.1f%%)", 
                currentUtilization, capacityM3, 
                utilizationPercentage != null ? utilizationPercentage : BigDecimal.ZERO);
        }
        return "No utilization data";
    }
    
    public boolean isNearCapacity() {
        return utilizationPercentage != null && utilizationPercentage.compareTo(new BigDecimal("90")) >= 0;
    }
    
    public BigDecimal getAvailableCapacity() {
        if (hasCapacity() && currentUtilization != null) {
            return capacityM3.subtract(currentUtilization);
        }
        return null;
    }
    
    public String getFormattedAvailableCapacity() {
        BigDecimal available = getAvailableCapacity();
        if (available != null) {
            return String.format("%.2f m³ available", available);
        }
        return "Capacity data unavailable";
    }
}