package ktc.spring_project.dtos.payment;

import ktc.spring_project.enums.PaymentMethod;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * DTO for Payment response data
 * Used when returning payment information to clients
 */
public class PaymentResponseDTO {
    
    private Long id;
    private Long orderId;
    private String orderCode; // For easy reference
    private BigDecimal amount; // Payment amount in VND
    private PaymentMethod paymentMethod;
    private String statusCode; // From Status entity
    private String statusDescription;
    private String transactionId; // Transaction ID from payment gateway
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private String createdByName; // Name of user who processed payment
    private String notes;
    
    // Additional business info
    private Boolean isSuccessful; // True if payment is completed
    private String paymentMethodDisplayName;
    
    // Constructors
    public PaymentResponseDTO() {}
    
    public PaymentResponseDTO(Long id, Long orderId, String orderCode, BigDecimal amount,
                            PaymentMethod paymentMethod, String statusCode, String statusDescription,
                            String transactionId, LocalDateTime createdAt, LocalDateTime updatedAt,
                            String createdByName, String notes) {
        this.id = id;
        this.orderId = orderId;
        this.orderCode = orderCode;
        this.amount = amount;
        this.paymentMethod = paymentMethod;
        this.statusCode = statusCode;
        this.statusDescription = statusDescription;
        this.transactionId = transactionId;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
        this.createdByName = createdByName;
        this.notes = notes;
        // Calculate derived fields
        calculateDerivedFields();
    }
    
    private void calculateDerivedFields() {
        this.isSuccessful = "COMPLETED".equals(statusCode);
        this.paymentMethodDisplayName = paymentMethod != null ? paymentMethod.getDisplayName() : null;
    }
    
    // Getters and Setters
    public Long getId() {
        return id;
    }
    
    public void setId(Long id) {
        this.id = id;
    }
    
    public Long getOrderId() {
        return orderId;
    }
    
    public void setOrderId(Long orderId) {
        this.orderId = orderId;
    }
    
    public String getOrderCode() {
        return orderCode;
    }
    
    public void setOrderCode(String orderCode) {
        this.orderCode = orderCode;
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
        calculateDerivedFields();
    }
    
    public String getStatusCode() {
        return statusCode;
    }
    
    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
        calculateDerivedFields();
    }
    
    public String getStatusDescription() {
        return statusDescription;
    }
    
    public void setStatusDescription(String statusDescription) {
        this.statusDescription = statusDescription;
    }
    
    public String getTransactionId() {
        return transactionId;
    }
    
    public void setTransactionId(String transactionId) {
        this.transactionId = transactionId;
    }
    
    public LocalDateTime getCreatedAt() {
        return createdAt;
    }
    
    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }
    
    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }
    
    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
    
    public String getCreatedByName() {
        return createdByName;
    }
    
    public void setCreatedByName(String createdByName) {
        this.createdByName = createdByName;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    public Boolean getIsSuccessful() {
        return isSuccessful;
    }
    
    public String getPaymentMethodDisplayName() {
        return paymentMethodDisplayName;
    }
    
    @Override
    public String toString() {
        return "PaymentResponseDTO{" +
                "id=" + id +
                ", orderId=" + orderId +
                ", orderCode='" + orderCode + '\'' +
                ", amount=" + amount +
                ", paymentMethod=" + paymentMethod +
                ", statusCode='" + statusCode + '\'' +
                ", transactionId='" + transactionId + '\'' +
                ", isSuccessful=" + isSuccessful +
                ", createdAt=" + createdAt +
                '}';
    }
}