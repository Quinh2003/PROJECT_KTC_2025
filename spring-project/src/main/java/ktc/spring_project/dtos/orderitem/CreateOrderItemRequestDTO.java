package ktc.spring_project.dtos.orderitem;

import java.math.BigDecimal;

/**
 * DTO for creating a new order item
 */
public class CreateOrderItemRequestDTO {
    
    private Long orderId;
    private Long productId;
    private Integer quantity;
    private BigDecimal unitPrice;
    private BigDecimal shippingFee;
    private String notes;
    
    // Constructors
    public CreateOrderItemRequestDTO() {}
    
    public CreateOrderItemRequestDTO(Long orderId, Long productId, Integer quantity, BigDecimal unitPrice) {
        this.orderId = orderId;
        this.productId = productId;
        this.quantity = quantity;
        this.unitPrice = unitPrice;
    }
    
    // Getters and Setters
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public Long getProductId() { return productId; }
    public void setProductId(Long productId) { this.productId = productId; }
    
    public Integer getQuantity() { return quantity; }
    public void setQuantity(Integer quantity) { this.quantity = quantity; }
    
    public BigDecimal getUnitPrice() { return unitPrice; }
    public void setUnitPrice(BigDecimal unitPrice) { this.unitPrice = unitPrice; }
    
    public BigDecimal getShippingFee() { return shippingFee; }
    public void setShippingFee(BigDecimal shippingFee) { this.shippingFee = shippingFee; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    // Validation methods
    public boolean isValid() {
        return orderId != null && productId != null && 
               quantity != null && quantity > 0 &&
               unitPrice != null && unitPrice.compareTo(BigDecimal.ZERO) >= 0;
    }
    
    public boolean hasShippingFee() {
        return shippingFee != null && shippingFee.compareTo(BigDecimal.ZERO) > 0;
    }
    
    public boolean hasNotes() {
        return notes != null && !notes.trim().isEmpty();
    }
    
    public BigDecimal getSubtotal() {
        if (quantity != null && unitPrice != null) {
            return unitPrice.multiply(new BigDecimal(quantity));
        }
        return BigDecimal.ZERO;
    }
    
    public BigDecimal getTotalAmount() {
        BigDecimal subtotal = getSubtotal();
        if (hasShippingFee()) {
            return subtotal.add(shippingFee);
        }
        return subtotal;
    }
    
    public String getFormattedSubtotal() {
        return String.format("$%.2f", getSubtotal());
    }
    
    public String getFormattedTotal() {
        return String.format("$%.2f", getTotalAmount());
    }
    
    public String getFormattedUnitPrice() {
        return unitPrice != null ? String.format("$%.2f", unitPrice) : "$0.00";
    }
    
    public String getFormattedShippingFee() {
        return hasShippingFee() ? String.format("$%.2f", shippingFee) : "Free shipping";
    }
}