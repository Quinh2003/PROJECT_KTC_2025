package ktc.spring_project.dtos.product;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * DTO for Product response data
 * Used when returning product information to clients
 */
public class ProductResponseDTO {
    
    private Long id;
    private String productCode;
    private String name;
    private String description;
    private BigDecimal unitPrice; // Price per unit in VND
    private BigDecimal weight; // Weight in kg
    private Boolean fragile; // Is the product fragile?
    private Integer stockQuantity; // Current stock in inventory
    private Long warehouseId; // Default warehouse ID
    private String warehouseName; // Default warehouse name
    private Boolean temporary; // True if created by customer
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private String createdByName; // Name of user who created this product
    private String notes;
    
    // Additional business info
    private BigDecimal totalValue; // stockQuantity * unitPrice
    private Boolean lowStock; // True if stock is below threshold
    
    // Constructors
    public ProductResponseDTO() {}
    
    public ProductResponseDTO(Long id, String productCode, String name, String description,
                            BigDecimal unitPrice, BigDecimal weight, Boolean fragile,
                            Integer stockQuantity, Long warehouseId, String warehouseName,
                            Boolean temporary, LocalDateTime createdAt, LocalDateTime updatedAt,
                            String createdByName, String notes) {
        this.id = id;
        this.productCode = productCode;
        this.name = name;
        this.description = description;
        this.unitPrice = unitPrice;
        this.weight = weight;
        this.fragile = fragile;
        this.stockQuantity = stockQuantity;
        this.warehouseId = warehouseId;
        this.warehouseName = warehouseName;
        this.temporary = temporary;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
        this.createdByName = createdByName;
        this.notes = notes;
        // Calculate derived fields
        calculateDerivedFields();
    }
    
    private void calculateDerivedFields() {
        if (unitPrice != null && stockQuantity != null) {
            this.totalValue = unitPrice.multiply(BigDecimal.valueOf(stockQuantity));
        }
        this.lowStock = stockQuantity != null && stockQuantity < 10; // Threshold can be configurable
    }
    
    // Getters and Setters
    public Long getId() {
        return id;
    }
    
    public void setId(Long id) {
        this.id = id;
    }
    
    public String getProductCode() {
        return productCode;
    }
    
    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getDescription() {
        return description;
    }
    
    public void setDescription(String description) {
        this.description = description;
    }
    
    public BigDecimal getUnitPrice() {
        return unitPrice;
    }
    
    public void setUnitPrice(BigDecimal unitPrice) {
        this.unitPrice = unitPrice;
        calculateDerivedFields();
    }
    
    public BigDecimal getWeight() {
        return weight;
    }
    
    public void setWeight(BigDecimal weight) {
        this.weight = weight;
    }
    
    public Boolean getFragile() {
        return fragile;
    }
    
    public void setFragile(Boolean fragile) {
        this.fragile = fragile;
    }
    
    public Integer getStockQuantity() {
        return stockQuantity;
    }
    
    public void setStockQuantity(Integer stockQuantity) {
        this.stockQuantity = stockQuantity;
        calculateDerivedFields();
    }
    
    public Long getWarehouseId() {
        return warehouseId;
    }
    
    public void setWarehouseId(Long warehouseId) {
        this.warehouseId = warehouseId;
    }
    
    public String getWarehouseName() {
        return warehouseName;
    }
    
    public void setWarehouseName(String warehouseName) {
        this.warehouseName = warehouseName;
    }
    
    public Boolean getTemporary() {
        return temporary;
    }
    
    public void setTemporary(Boolean temporary) {
        this.temporary = temporary;
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
    
    public BigDecimal getTotalValue() {
        return totalValue;
    }
    
    public Boolean getLowStock() {
        return lowStock;
    }
    
    @Override
    public String toString() {
        return "ProductResponseDTO{" +
                "id=" + id +
                ", productCode='" + productCode + '\'' +
                ", name='" + name + '\'' +
                ", unitPrice=" + unitPrice +
                ", weight=" + weight +
                ", fragile=" + fragile +
                ", stockQuantity=" + stockQuantity +
                ", warehouseId=" + warehouseId +
                ", warehouseName='" + warehouseName + '\'' +
                ", temporary=" + temporary +
                ", totalValue=" + totalValue +
                ", lowStock=" + lowStock +
                '}';
    }
}