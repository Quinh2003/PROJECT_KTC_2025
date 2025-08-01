package ktc.spring_project.dtos.payment;

import ktc.spring_project.enums.PaymentMethod;
import jakarta.validation.constraints.*;
import java.math.BigDecimal;

/**
 * DTO for Payment creation requests
 * Used when processing a payment for an order
 */
public class CreatePaymentRequestDTO {
    
    @NotNull(message = "Order ID is required")
    private Long orderId;
    
    @NotNull(message = "Amount is required")
    @DecimalMin(value = "0.01", message = "Amount must be greater than 0")
    @Digits(integer = 8, fraction = 2, message = "Amount must have at most 8 integer digits and 2 decimal places")
    private BigDecimal amount; // Payment amount in VND
    
    @NotNull(message = "Payment method is required")
    private PaymentMethod paymentMethod;
    
    @Size(max = 255, message = "Transaction ID must not exceed 255 characters")
    private String transactionId; // For online payments (Stripe, etc.)
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;
    
    // Additional fields for specific payment methods
    private String cardToken; // For credit card payments
    private String bankAccount; // For bank transfers
    private String stripePaymentIntentId; // For Stripe payments
    
    // Constructors
    public CreatePaymentRequestDTO() {}
    
    public CreatePaymentRequestDTO(Long orderId, BigDecimal amount, PaymentMethod paymentMethod,
                                 String transactionId, String notes) {
        this.orderId = orderId;
        this.amount = amount;
        this.paymentMethod = paymentMethod;
        this.transactionId = transactionId;
        this.notes = notes;
    }
    
    // Getters and Setters
    public Long getOrderId() {
        return orderId;
    }
    
    public void setOrderId(Long orderId) {
        this.orderId = orderId;
    }
    
    public BigDecimal getAmount() {
        return amount;
    }
    
    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }
    
    public PaymentMethod getPaymentMethod() {
        return paymentMethod;
    }
    
    public void setPaymentMethod(PaymentMethod paymentMethod) {
        this.paymentMethod = paymentMethod;
    }
    
    public String getTransactionId() {
        return transactionId;
    }
    
    public void setTransactionId(String transactionId) {
        this.transactionId = transactionId;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    public String getCardToken() {
        return cardToken;
    }
    
    public void setCardToken(String cardToken) {
        this.cardToken = cardToken;
    }
    
    public String getBankAccount() {
        return bankAccount;
    }
    
    public void setBankAccount(String bankAccount) {
        this.bankAccount = bankAccount;
    }
    
    public String getStripePaymentIntentId() {
        return stripePaymentIntentId;
    }
    
    public void setStripePaymentIntentId(String stripePaymentIntentId) {
        this.stripePaymentIntentId = stripePaymentIntentId;
    }
    
    @Override
    public String toString() {
        return "CreatePaymentRequestDTO{" +
                "orderId=" + orderId +
                ", amount=" + amount +
                ", paymentMethod=" + paymentMethod +
                ", transactionId='" + transactionId + '\'' +
                ", notes='" + notes + '\'' +
                '}';
    }
}