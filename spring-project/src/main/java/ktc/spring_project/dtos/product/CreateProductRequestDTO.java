package ktc.spring_project.dtos.product;

import jakarta.validation.constraints.*;
import java.math.BigDecimal;

/**
 * DTO for creating new products
 */
public class CreateProductRequestDTO {
    

    
    @NotBlank(message = "Product name is required")
    @Size(max = 100, message = "Product name must not exceed 100 characters")
    private String name;
    
    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;
    
    // @NotNull(message = "Unit price is required")
    @DecimalMin(value = "0.0", inclusive = false, message = "Unit price must be greater than 0")
    private BigDecimal unitPrice;
    
    @DecimalMin(value = "0.0", message = "Weight must be positive")
    private BigDecimal weight;
    
    private Boolean fragile = false;
    
    @Min(value = 0, message = "Stock quantity must be positive")
    private Integer stockQuantity = 0;
    
    private Boolean temporary = false;
    
    @Size(max = 500, message = "Notes must not exceed 500 characters")
    private String notes;
    
    @NotNull(message = "Warehouse ID is required")
    private Long warehouseId;
    
    @NotNull(message = "Created by user ID is required")
    private Long createdByUserId;
    
    // Constructors
    public CreateProductRequestDTO() {}
    
    public CreateProductRequestDTO(String name, BigDecimal unitPrice, Long warehouseId, Long createdByUserId) {
        this.name = name;
        this.unitPrice = unitPrice;
        this.warehouseId = warehouseId;
        this.createdByUserId = createdByUserId;
        this.fragile = false;
        this.stockQuantity = 0;
        this.temporary = false;
    }
    
    // Getters and Setters
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public BigDecimal getUnitPrice() { return unitPrice; }
    public void setUnitPrice(BigDecimal unitPrice) { this.unitPrice = unitPrice; }
    
    public BigDecimal getWeight() { return weight; }
    public void setWeight(BigDecimal weight) { this.weight = weight; }
    
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
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
}