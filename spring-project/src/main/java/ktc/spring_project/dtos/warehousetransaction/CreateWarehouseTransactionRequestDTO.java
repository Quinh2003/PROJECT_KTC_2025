package ktc.spring_project.dtos.warehousetransaction;

import ktc.spring_project.enums.TransactionType;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for creating a new warehouse transaction
 */
public class CreateWarehouseTransactionRequestDTO {
    
    private Long productId;
    private Long warehouseId;
    private Long orderId;
    private Long statusId;
    private TransactionType transactionType;
    private Integer quantity;
    private BigDecimal unitCost;
    private Timestamp transactionDate;
    private String notes;
    
    // Constructors
    public CreateWarehouseTransactionRequestDTO() {}
    
    public CreateWarehouseTransactionRequestDTO(Long productId, Long warehouseId, 
                                               TransactionType transactionType, Integer quantity) {
        this.productId = productId;
        this.warehouseId = warehouseId;
        this.transactionType = transactionType;
        this.quantity = quantity;
    }
    
    // Getters and Setters
    public Long getProductId() { return productId; }
    public void setProductId(Long productId) { this.productId = productId; }
    
    public Long getWarehouseId() { return warehouseId; }
    public void setWarehouseId(Long warehouseId) { this.warehouseId = warehouseId; }
    
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public TransactionType getTransactionType() { return transactionType; }
    public void setTransactionType(TransactionType transactionType) { this.transactionType = transactionType; }
    
    public Integer getQuantity() { return quantity; }
    public void setQuantity(Integer quantity) { this.quantity = quantity; }
    
    public BigDecimal getUnitCost() { return unitCost; }
    public void setUnitCost(BigDecimal unitCost) { this.unitCost = unitCost; }
    
    public Timestamp getTransactionDate() { return transactionDate; }
    public void setTransactionDate(Timestamp transactionDate) { this.transactionDate = transactionDate; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    // Validation methods
    public boolean isValid() {
        return productId != null && warehouseId != null && 
               transactionType != null && quantity != null && quantity > 0;
    }
    
    public boolean hasUnitCost() {
        return unitCost != null && unitCost.compareTo(BigDecimal.ZERO) > 0;
    }
    
    public boolean hasOrder() {
        return orderId != null;
    }
    
    public boolean hasNotes() {
        return notes != null && !notes.trim().isEmpty();
    }
    
    public boolean isInTransaction() {
        return TransactionType.IN.equals(transactionType);
    }
    
    public boolean isOutTransaction() {
        return TransactionType.OUT.equals(transactionType);
    }
    
    public boolean isTransferTransaction() {
        return TransactionType.TRANSFER.equals(transactionType);
    }
    
    public boolean isAdjustmentTransaction() {
        return TransactionType.ADJUSTMENT.equals(transactionType);
    }
    
    public BigDecimal getTotalCost() {
        if (hasUnitCost() && quantity != null) {
            return unitCost.multiply(new BigDecimal(quantity));
        }
        return BigDecimal.ZERO;
    }
    
    public String getTransactionTypeDisplay() {
        return transactionType != null ? transactionType.name().replace("_", " ") : "Unknown";
    }
    
    public String getFormattedUnitCost() {
        return hasUnitCost() ? String.format("$%.2f", unitCost) : "No cost specified";
    }
    
    public String getFormattedTotalCost() {
        return String.format("$%.2f", getTotalCost());
    }
    
    public boolean isValidForInventoryUpdate() {
        return isValid() && (isInTransaction() || isOutTransaction() || isAdjustmentTransaction());
    }
}