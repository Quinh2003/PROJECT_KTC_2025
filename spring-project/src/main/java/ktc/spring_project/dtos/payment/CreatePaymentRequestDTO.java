package ktc.spring_project.dtos.payment;

import ktc.spring_project.enums.PaymentMethod;
import jakarta.validation.constraints.*;
import java.math.BigDecimal;

/**
 * DTO for creating new payments
 */
public class CreatePaymentRequestDTO {
    
    @NotNull(message = "Payment amount is required")
    @DecimalMin(value = "0.0", inclusive = false, message = "Payment amount must be greater than 0")
    private BigDecimal amount;
    
    @NotNull(message = "Payment method is required")
    private PaymentMethod paymentMethod;
    
    @Size(max = 100, message = "Transaction ID must not exceed 100 characters")
    private String transactionId;
    
    @Size(max = 500, message = "Notes must not exceed 500 characters")
    private String notes;
    
    @NotNull(message = "Order ID is required")
    private Long orderId;
    
    private Long statusId;
    
    @NotNull(message = "Created by user ID is required")
    private Long createdByUserId;
    
    // Constructors
    public CreatePaymentRequestDTO() {}
    
    public CreatePaymentRequestDTO(BigDecimal amount, PaymentMethod paymentMethod, Long orderId, Long createdByUserId) {
        this.amount = amount;
        this.paymentMethod = paymentMethod;
        this.orderId = orderId;
        this.createdByUserId = createdByUserId;
    }
    
    // Getters and Setters
    public BigDecimal getAmount() { return amount; }
    public void setAmount(BigDecimal amount) { this.amount = amount; }
    
    public PaymentMethod getPaymentMethod() { return paymentMethod; }
    public void setPaymentMethod(PaymentMethod paymentMethod) { this.paymentMethod = paymentMethod; }
    
    public String getTransactionId() { return transactionId; }
    public void setTransactionId(String transactionId) { this.transactionId = transactionId; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
}