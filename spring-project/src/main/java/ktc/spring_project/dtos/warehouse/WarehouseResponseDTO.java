package ktc.spring_project.dtos.warehouse;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * DTO for Warehouse response data
 * Used when returning warehouse information to clients
 */
public class WarehouseResponseDTO {
    
    private Long id;
    private String name;
    private String address;
    private BigDecimal capacity; // Storage capacity in m³
    private BigDecimal maxWeight; // Maximum weight capacity in tons
    private Boolean isActive;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private String createdByName; // Name of user who created this warehouse
    private String notes;
    
    // Additional business info
    private Integer productCount; // Number of products stored
    private BigDecimal currentUtilization; // Current capacity utilization percentage
    
    // Constructors
    public WarehouseResponseDTO() {}
    
    public WarehouseResponseDTO(Long id, String name, String address, BigDecimal capacity,
                              BigDecimal maxWeight, Boolean isActive, LocalDateTime createdAt,
                              LocalDateTime updatedAt, String createdByName, String notes) {
        this.id = id;
        this.name = name;
        this.address = address;
        this.capacity = capacity;
        this.maxWeight = maxWeight;
        this.isActive = isActive;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
        this.createdByName = createdByName;
        this.notes = notes;
    }
    
    // Getters and Setters
    public Long getId() {
        return id;
    }
    
    public void setId(Long id) {
        this.id = id;
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getAddress() {
        return address;
    }
    
    public void setAddress(String address) {
        this.address = address;
    }
    
    public BigDecimal getCapacity() {
        return capacity;
    }
    
    public void setCapacity(BigDecimal capacity) {
        this.capacity = capacity;
    }
    
    public BigDecimal getMaxWeight() {
        return maxWeight;
    }
    
    public void setMaxWeight(BigDecimal maxWeight) {
        this.maxWeight = maxWeight;
    }
    
    public Boolean getIsActive() {
        return isActive;
    }
    
    public void setIsActive(Boolean isActive) {
        this.isActive = isActive;
    }
    
    public LocalDateTime getCreatedAt() {
        return createdAt;
    }
    
    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }
    
    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }
    
    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
    
    public String getCreatedByName() {
        return createdByName;
    }
    
    public void setCreatedByName(String createdByName) {
        this.createdByName = createdByName;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    public Integer getProductCount() {
        return productCount;
    }
    
    public void setProductCount(Integer productCount) {
        this.productCount = productCount;
    }
    
    public BigDecimal getCurrentUtilization() {
        return currentUtilization;
    }
    
    public void setCurrentUtilization(BigDecimal currentUtilization) {
        this.currentUtilization = currentUtilization;
    }
    
    @Override
    public String toString() {
        return "WarehouseResponseDTO{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", address='" + address + '\'' +
                ", capacity=" + capacity +
                ", maxWeight=" + maxWeight +
                ", isActive=" + isActive +
                ", createdAt=" + createdAt +
                ", updatedAt=" + updatedAt +
                ", createdByName='" + createdByName + '\'' +
                ", notes='" + notes + '\'' +
                ", productCount=" + productCount +
                ", currentUtilization=" + currentUtilization +
                '}';
    }
}