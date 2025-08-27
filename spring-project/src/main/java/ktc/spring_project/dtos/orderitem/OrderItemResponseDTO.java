package ktc.spring_project.dtos.orderitem;

import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for order item response data
 */
public class OrderItemResponseDTO {
    
    private Long id;
    
    // Order information
    private Long orderId;
    private String orderNumber;
    
    // Product information
    private Long productId;

    private String productName;
    private String productDescription;
    private String productImage;
    private Boolean isFragile;
    
    // Quantity and pricing
    private Integer quantity;
    private BigDecimal unitPrice;
    private BigDecimal subtotal;
    private BigDecimal shippingFee;
    private BigDecimal totalAmount;
    
    // Product attributes
    private BigDecimal productWeight;
    private BigDecimal productVolume;
    
    // Additional information
    private String notes;
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Stock information
    private Integer availableStock;
    private Boolean inStock;
    
    // Constructors
    public OrderItemResponseDTO() {}
    
    public OrderItemResponseDTO(Long id, Long productId, String productName, 
                               Integer quantity, BigDecimal unitPrice) {
        this.id = id;
        this.productId = productId;
        this.productName = productName;
        this.quantity = quantity;
        this.unitPrice = unitPrice;
        this.subtotal = calculateSubtotal();
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public String getOrderNumber() { return orderNumber; }
    public void setOrderNumber(String orderNumber) { this.orderNumber = orderNumber; }
    
    public Long getProductId() { return productId; }
    public void setProductId(Long productId) { this.productId = productId; }
    

    
    public String getProductName() { return productName; }
    public void setProductName(String productName) { this.productName = productName; }
    
    public String getProductDescription() { return productDescription; }
    public void setProductDescription(String productDescription) { this.productDescription = productDescription; }
    
    public String getProductImage() { return productImage; }
    public void setProductImage(String productImage) { this.productImage = productImage; }
    
    public Boolean getIsFragile() { return isFragile; }
    public void setIsFragile(Boolean isFragile) { this.isFragile = isFragile; }
    
    public Integer getQuantity() { return quantity; }
    public void setQuantity(Integer quantity) { this.quantity = quantity; }
    
    public BigDecimal getUnitPrice() { return unitPrice; }
    public void setUnitPrice(BigDecimal unitPrice) { this.unitPrice = unitPrice; }
    
    public BigDecimal getSubtotal() { return subtotal; }
    public void setSubtotal(BigDecimal subtotal) { this.subtotal = subtotal; }
    
    public BigDecimal getShippingFee() { return shippingFee; }
    public void setShippingFee(BigDecimal shippingFee) { this.shippingFee = shippingFee; }
    
    public BigDecimal getTotalAmount() { return totalAmount; }
    public void setTotalAmount(BigDecimal totalAmount) { this.totalAmount = totalAmount; }
    
    public BigDecimal getProductWeight() { return productWeight; }
    public void setProductWeight(BigDecimal productWeight) { this.productWeight = productWeight; }
    
    public BigDecimal getProductVolume() { return productVolume; }
    public void setProductVolume(BigDecimal productVolume) { this.productVolume = productVolume; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    public Integer getAvailableStock() { return availableStock; }
    public void setAvailableStock(Integer availableStock) { this.availableStock = availableStock; }
    
    public Boolean getInStock() { return inStock; }
    public void setInStock(Boolean inStock) { this.inStock = inStock; }
    
    // Utility methods
    private BigDecimal calculateSubtotal() {
        if (quantity != null && unitPrice != null) {
            return unitPrice.multiply(new BigDecimal(quantity));
        }
        return BigDecimal.ZERO;
    }
    
    public BigDecimal calculateTotalAmount() {
        BigDecimal calc = calculateSubtotal();
        if (shippingFee != null) {
            calc = calc.add(shippingFee);
        }
        return calc;
    }
    
    public boolean isFragileItem() {
        return Boolean.TRUE.equals(isFragile);
    }
    
    public boolean hasShippingFee() {
        return shippingFee != null && shippingFee.compareTo(BigDecimal.ZERO) > 0;
    }
    
    public boolean hasNotes() {
        return notes != null && !notes.trim().isEmpty();
    }
    
    public boolean isInStockItem() {
        return Boolean.TRUE.equals(inStock) && 
               (availableStock == null || availableStock >= quantity);
    }
    
    public boolean isOutOfStock() {
        return Boolean.FALSE.equals(inStock) || 
               (availableStock != null && availableStock < quantity);
    }
    
    public String getDisplayName() {
        return productName;
    }
    
    public String getFormattedUnitPrice() {
        return unitPrice != null ? String.format("$%.2f", unitPrice) : "$0.00";
    }
    
    public String getFormattedSubtotal() {
        return String.format("$%.2f", subtotal != null ? subtotal : calculateSubtotal());
    }
    
    public String getFormattedTotal() {
        return String.format("$%.2f", totalAmount != null ? totalAmount : calculateTotalAmount());
    }
    
    public String getFormattedShippingFee() {
        return hasShippingFee() ? String.format("$%.2f", shippingFee) : "Free";
    }
    
    public String getQuantityDisplay() {
        return String.format("%d unit%s", quantity, quantity > 1 ? "s" : "");
    }
    
    public String getStockStatus() {
        if (isOutOfStock()) {
            return "Out of Stock";
        } else if (availableStock != null && availableStock < 10) {
            return "Low Stock";
        } else {
            return "In Stock";
        }
    }
    
    public String getWeightDisplay() {
        if (productWeight != null && quantity != null) {
            BigDecimal totalWeight = productWeight.multiply(new BigDecimal(quantity));
            return String.format("%.2f kg", totalWeight);
        }
        return "Weight unknown";
    }
    
    public String getVolumeDisplay() {
        if (productVolume != null && quantity != null) {
            BigDecimal totalVolume = productVolume.multiply(new BigDecimal(quantity));
            return String.format("%.3f m³", totalVolume);
        }
        return "Volume unknown";
    }
    
    public String getFragileIndicator() {
        return isFragileItem() ? "⚠️ FRAGILE" : "";
    }
}