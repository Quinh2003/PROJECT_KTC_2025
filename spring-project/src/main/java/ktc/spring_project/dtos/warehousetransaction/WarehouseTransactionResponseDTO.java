package ktc.spring_project.dtos.warehousetransaction;

import ktc.spring_project.enums.TransactionType;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for warehouse transaction response data
 */
public class WarehouseTransactionResponseDTO {
    
    private Long id;
    
    // Product information
    private Long productId;

    private String productName;
    
    // Warehouse information
    private Long warehouseId;

    private String warehouseName;
    
    // Order information
    private Long orderId;
    private String orderNumber;
    
    // Status information
    private Long statusId;
    private String statusName;
    private String statusType;
    
    // Transaction details
    private TransactionType transactionType;
    private Integer quantity;
    private BigDecimal unitCost;
    private BigDecimal totalCost;
    private Timestamp transactionDate;
    private String notes;
    
    // User information
    private Long createdByUserId;
    private String createdByUserName;
    
    private Timestamp createdAt;
    
    // Constructors
    public WarehouseTransactionResponseDTO() {}
    
    public WarehouseTransactionResponseDTO(Long id, TransactionType transactionType, 
                                          String productName, String warehouseName, 
                                          Integer quantity, Timestamp transactionDate) {
        this.id = id;
        this.transactionType = transactionType;
        this.productName = productName;
        this.warehouseName = warehouseName;
        this.quantity = quantity;
        this.transactionDate = transactionDate;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public Long getProductId() { return productId; }
    public void setProductId(Long productId) { this.productId = productId; }
    

    
    public String getProductName() { return productName; }
    public void setProductName(String productName) { this.productName = productName; }
    
    public Long getWarehouseId() { return warehouseId; }
    public void setWarehouseId(Long warehouseId) { this.warehouseId = warehouseId; }
    

    
    public String getWarehouseName() { return warehouseName; }
    public void setWarehouseName(String warehouseName) { this.warehouseName = warehouseName; }
    
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public String getOrderNumber() { return orderNumber; }
    public void setOrderNumber(String orderNumber) { this.orderNumber = orderNumber; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public String getStatusName() { return statusName; }
    public void setStatusName(String statusName) { this.statusName = statusName; }
    
    public String getStatusType() { return statusType; }
    public void setStatusType(String statusType) { this.statusType = statusType; }
    
    public TransactionType getTransactionType() { return transactionType; }
    public void setTransactionType(TransactionType transactionType) { this.transactionType = transactionType; }
    
    public Integer getQuantity() { return quantity; }
    public void setQuantity(Integer quantity) { this.quantity = quantity; }
    
    public BigDecimal getUnitCost() { return unitCost; }
    public void setUnitCost(BigDecimal unitCost) { this.unitCost = unitCost; }
    
    public BigDecimal getTotalCost() { return totalCost; }
    public void setTotalCost(BigDecimal totalCost) { this.totalCost = totalCost; }
    
    public Timestamp getTransactionDate() { return transactionDate; }
    public void setTransactionDate(Timestamp transactionDate) { this.transactionDate = transactionDate; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
    
    public String getCreatedByUserName() { return createdByUserName; }
    public void setCreatedByUserName(String createdByUserName) { this.createdByUserName = createdByUserName; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    // Utility methods
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
    
    public String getTransactionTypeDisplay() {
        return transactionType != null ? transactionType.name().replace("_", " ") : "Unknown";
    }
    
    public String getTransactionIcon() {
        if (isInTransaction()) return "📥";
        if (isOutTransaction()) return "📤";
        if (isTransferTransaction()) return "🔄";
        if (isAdjustmentTransaction()) return "⚙️";
        return "📦";
    }
    
    public String getProductDisplay() {
        return productName != null ? productName : "Unknown Product";
    }
    
    public String getWarehouseDisplay() {
        return warehouseName != null ? warehouseName : "Unknown Warehouse";
    }
    
    public String getFormattedUnitCost() {
        return hasUnitCost() ? String.format("$%.2f", unitCost) : "No cost";
    }
    
    public String getFormattedTotalCost() {
        return totalCost != null ? String.format("$%.2f", totalCost) : calculateTotalCostFormatted();
    }
    
    private String calculateTotalCostFormatted() {
        if (hasUnitCost() && quantity != null) {
            BigDecimal calculated = unitCost.multiply(new BigDecimal(quantity));
            return String.format("$%.2f", calculated);
        }
        return "$0.00";
    }
    
    public String getQuantityDisplay() {
        String sign = "";
        if (isInTransaction() || isAdjustmentTransaction()) {
            sign = "+";
        } else if (isOutTransaction()) {
            sign = "-";
        }
        return String.format("%s%d unit%s", sign, quantity, quantity > 1 ? "s" : "");
    }
    
    public String getTransactionSummary() {
        return String.format("%s %s %s at %s", 
            getTransactionTypeDisplay(),
            getQuantityDisplay(),
            getProductDisplay(),
            getWarehouseDisplay());
    }
    
    public boolean isSuccessfulTransaction() {
        return statusName != null && statusName.toLowerCase().contains("success");
    }
    
    public boolean isPendingTransaction() {
        return statusName != null && statusName.toLowerCase().contains("pending");
    }
    
    public boolean isFailedTransaction() {
        return statusName != null && statusName.toLowerCase().contains("failed");
    }
}