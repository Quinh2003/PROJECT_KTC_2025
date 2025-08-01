package ktc.spring_project.dtos.product;

import jakarta.validation.constraints.*;
import java.math.BigDecimal;

/**
 * DTO for Product creation requests
 * Used when creating a new product in the catalog
 */
public class CreateProductRequestDTO {
    
    @NotBlank(message = "Product code is required")
    @Size(min = 2, max = 50, message = "Product code must be between 2 and 50 characters")
    @Pattern(regexp = "^[A-Z0-9_-]+$", message = "Product code must contain only uppercase letters, numbers, underscores, and hyphens")
    private String productCode;
    
    @NotBlank(message = "Product name is required")
    @Size(min = 2, max = 255, message = "Product name must be between 2 and 255 characters")
    private String name;
    
    @Size(max = 1000, message = "Description must not exceed 1000 characters")
    private String description;
    
    @NotNull(message = "Unit price is required")
    @DecimalMin(value = "0.01", message = "Unit price must be greater than 0")
    @Digits(integer = 8, fraction = 2, message = "Unit price must have at most 8 integer digits and 2 decimal places")
    private BigDecimal unitPrice; // Price per unit in VND
    
    @DecimalMin(value = "0.01", message = "Weight must be greater than 0")
    @Digits(integer = 6, fraction = 2, message = "Weight must have at most 6 integer digits and 2 decimal places")
    private BigDecimal weight; // Weight in kg
    
    private Boolean fragile = false; // Is the product fragile?
    
    @Min(value = 0, message = "Stock quantity cannot be negative")
    private Integer stockQuantity = 0; // Initial stock quantity
    
    private Long warehouseId; // Default warehouse for the product
    
    private Boolean temporary = false; // True if created by customer
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;
    
    // Constructors
    public CreateProductRequestDTO() {}
    
    public CreateProductRequestDTO(String productCode, String name, String description,
                                 BigDecimal unitPrice, BigDecimal weight, Boolean fragile,
                                 Integer stockQuantity, Long warehouseId, Boolean temporary, String notes) {
        this.productCode = productCode;
        this.name = name;
        this.description = description;
        this.unitPrice = unitPrice;
        this.weight = weight;
        this.fragile = fragile;
        this.stockQuantity = stockQuantity;
        this.warehouseId = warehouseId;
        this.temporary = temporary;
        this.notes = notes;
    }
    
    // Getters and Setters
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
    }
    
    public Long getWarehouseId() {
        return warehouseId;
    }
    
    public void setWarehouseId(Long warehouseId) {
        this.warehouseId = warehouseId;
    }
    
    public Boolean getTemporary() {
        return temporary;
    }
    
    public void setTemporary(Boolean temporary) {
        this.temporary = temporary;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    @Override
    public String toString() {
        return "CreateProductRequestDTO{" +
                "productCode='" + productCode + '\'' +
                ", name='" + name + '\'' +
                ", description='" + description + '\'' +
                ", unitPrice=" + unitPrice +
                ", weight=" + weight +
                ", fragile=" + fragile +
                ", stockQuantity=" + stockQuantity +
                ", warehouseId=" + warehouseId +
                ", temporary=" + temporary +
                ", notes='" + notes + '\'' +
                '}';
    }
}