package ktc.spring_project.dtos.orderitem;

import java.math.BigDecimal;

/**
 * DTO for updating an existing order item
 */
public class UpdateOrderItemRequestDTO {
    
    private Integer quantity;
    private BigDecimal unitPrice;
    private BigDecimal shippingFee;
    private String notes;
    
    // Constructors
    public UpdateOrderItemRequestDTO() {}
    
    public UpdateOrderItemRequestDTO(Integer quantity) {
        this.quantity = quantity;
    }
    
    public UpdateOrderItemRequestDTO(Integer quantity, BigDecimal unitPrice) {
        this.quantity = quantity;
        this.unitPrice = unitPrice;
    }
    
    // Getters and Setters
    public Integer getQuantity() { return quantity; }
    public void setQuantity(Integer quantity) { this.quantity = quantity; }
    
    public BigDecimal getUnitPrice() { return unitPrice; }
    public void setUnitPrice(BigDecimal unitPrice) { this.unitPrice = unitPrice; }
    
    public BigDecimal getShippingFee() { return shippingFee; }
    public void setShippingFee(BigDecimal shippingFee) { this.shippingFee = shippingFee; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    // Validation methods
    public boolean hasValidQuantity() {
        return quantity != null && quantity > 0;
    }
    
    public boolean hasValidUnitPrice() {
        return unitPrice != null && unitPrice.compareTo(BigDecimal.ZERO) >= 0;
    }
    
    public boolean hasValidShippingFee() {
        return shippingFee == null || shippingFee.compareTo(BigDecimal.ZERO) >= 0;
    }
    
    public boolean hasQuantityUpdate() {
        return quantity != null;
    }
    
    public boolean hasPriceUpdate() {
        return unitPrice != null;
    }
    
    public boolean hasShippingFeeUpdate() {
        return shippingFee != null;
    }
    
    public boolean hasNotesUpdate() {
        return notes != null;
    }
    
    public boolean isIncreasingQuantity(Integer currentQuantity) {
        return quantity != null && currentQuantity != null && quantity > currentQuantity;
    }
    
    public boolean isDecreasingQuantity(Integer currentQuantity) {
        return quantity != null && currentQuantity != null && quantity < currentQuantity;
    }
    
    public boolean isPriceIncrease(BigDecimal currentPrice) {
        return unitPrice != null && currentPrice != null && 
               unitPrice.compareTo(currentPrice) > 0;
    }
    
    public boolean isPriceDecrease(BigDecimal currentPrice) {
        return unitPrice != null && currentPrice != null && 
               unitPrice.compareTo(currentPrice) < 0;
    }
    
    public boolean isRemovingShippingFee() {
        return shippingFee != null && shippingFee.compareTo(BigDecimal.ZERO) == 0;
    }
    
    public boolean isAddingShippingFee() {
        return shippingFee != null && shippingFee.compareTo(BigDecimal.ZERO) > 0;
    }
    
    public boolean isClearingNotes() {
        return notes != null && notes.trim().isEmpty();
    }
    
    public boolean isAddingNotes() {
        return notes != null && !notes.trim().isEmpty();
    }
    
    public BigDecimal getSubtotal() {
        if (quantity != null && unitPrice != null) {
            return unitPrice.multiply(new BigDecimal(quantity));
        }
        return null;
    }
    
    public BigDecimal getTotalAmount() {
        BigDecimal subtotal = getSubtotal();
        if (subtotal != null) {
            if (shippingFee != null) {
                return subtotal.add(shippingFee);
            }
            return subtotal;
        }
        return null;
    }
    
    public String getFormattedUnitPrice() {
        return unitPrice != null ? String.format("$%.2f", unitPrice) : null;
    }
    
    public String getFormattedShippingFee() {
        return shippingFee != null ? String.format("$%.2f", shippingFee) : null;
    }
    
    public String getFormattedSubtotal() {
        BigDecimal subtotal = getSubtotal();
        return subtotal != null ? String.format("$%.2f", subtotal) : null;
    }
    
    public String getFormattedTotal() {
        BigDecimal total = getTotalAmount();
        return total != null ? String.format("$%.2f", total) : null;
    }
}