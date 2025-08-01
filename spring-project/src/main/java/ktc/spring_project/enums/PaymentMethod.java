package ktc.spring_project.enums;

/**
 * Payment methods supported by the system
 * Used in payments table.payment_method
 */
public enum PaymentMethod {
    CASH("Cash", "Cash on delivery payment"),
    CREDIT_CARD("Credit Card", "Credit card payment"),
    BANK_TRANSFER("Bank Transfer", "Direct bank transfer"),
    STRIPE("Stripe", "Online payment via Stripe gateway");
    
    private final String displayName;
    private final String description;
    
    PaymentMethod(String displayName, String description) {
        this.displayName = displayName;
        this.description = description;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getDescription() {
        return description;
    }
    
    /**
     * Check if payment method requires online processing
     */
    public boolean isOnlinePayment() {
        return this == CREDIT_CARD || this == STRIPE;
    }
    
    /**
     * Check if payment method requires offline processing
     */
    public boolean isOfflinePayment() {
        return this == CASH || this == BANK_TRANSFER;
    }
}