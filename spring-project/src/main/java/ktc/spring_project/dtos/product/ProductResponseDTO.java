package ktc.spring_project.dtos.product;

import ktc.spring_project.dtos.user.UserResponseDTO;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for product response data
 */
public class ProductResponseDTO {
    
    private Long id;

    private String name;
    private String description;
    private BigDecimal unitPrice;
    private BigDecimal weight;
    private BigDecimal volume;
    private Boolean fragile;
    private Integer stockQuantity;
    private Boolean temporary;
    private String notes;
    
    // Warehouse information
    private Long warehouseId;
    private String warehouseName;
    private String warehouseLocation;
    
    // Created by information
    private UserResponseDTO createdBy;
    private Long createdByUserId;
    private String createdByUserName;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public ProductResponseDTO() {}
    
    public ProductResponseDTO(Long id, String name, BigDecimal unitPrice, Integer stockQuantity) {
        this.id = id;
        this.name = name;
        this.unitPrice = unitPrice;
        this.stockQuantity = stockQuantity;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    

    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public BigDecimal getUnitPrice() { return unitPrice; }
    public void setUnitPrice(BigDecimal unitPrice) { this.unitPrice = unitPrice; }
    
    public BigDecimal getWeight() { return weight; }
    public void setWeight(BigDecimal weight) { this.weight = weight; }

    public BigDecimal getVolume() { return volume; }
    public void setVolume(BigDecimal volume) { this.volume = volume; }
    
    public Boolean getFragile() { return fragile; }
    public void setFragile(Boolean fragile) { this.fragile = fragile; }
    
    public Integer getStockQuantity() { return stockQuantity; }
    public void setStockQuantity(Integer stockQuantity) { this.stockQuantity = stockQuantity; }
    
    public Boolean getTemporary() { return temporary; }
    public void setTemporary(Boolean temporary) { this.temporary = temporary; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getWarehouseId() { return warehouseId; }
    public void setWarehouseId(Long warehouseId) { this.warehouseId = warehouseId; }
    
    public String getWarehouseName() { return warehouseName; }
    public void setWarehouseName(String warehouseName) { this.warehouseName = warehouseName; }
    
    public String getWarehouseLocation() { return warehouseLocation; }
    public void setWarehouseLocation(String warehouseLocation) { this.warehouseLocation = warehouseLocation; }
    
    public UserResponseDTO getCreatedBy() { return createdBy; }
    public void setCreatedBy(UserResponseDTO createdBy) { this.createdBy = createdBy; }
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
    
    public String getCreatedByUserName() { return createdByUserName; }
    public void setCreatedByUserName(String createdByUserName) { this.createdByUserName = createdByUserName; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public String getDisplayName() {
        return name;
    }
    
    public boolean isInStock() {
        return stockQuantity != null && stockQuantity > 0;
    }
    
    public boolean isFragileItem() {
        return Boolean.TRUE.equals(fragile);
    }
    
    public boolean isTemporaryItem() {
        return Boolean.TRUE.equals(temporary);
    }
    
    public String getStockStatus() {
        if (stockQuantity == null || stockQuantity == 0) {
            return "Out of Stock";
        } else if (stockQuantity < 10) {
            return "Low Stock";
        } else {
            return "In Stock";
        }
    }
    
    public String getFormattedPrice() {
        return unitPrice != null ? String.format("$%.2f", unitPrice) : null;
    }
    
    public String getFormattedWeight() {
        return weight != null ? String.format("%.2f kg", weight) : null;
    }
}