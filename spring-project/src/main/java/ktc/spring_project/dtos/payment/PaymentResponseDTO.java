package ktc.spring_project.dtos.payment;

import ktc.spring_project.enums.PaymentMethod;
import ktc.spring_project.dtos.user.UserResponseDTO;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for payment response data
 */
public class PaymentResponseDTO {
    
    private Long id;
    private BigDecimal amount;
    private PaymentMethod paymentMethod;
    private String transactionId;
    private String notes;
    
    // Order information
    private Long orderId;
    private String orderCode;
    
    // Status information
    private Long statusId;
    private String statusCode;
    private String statusDescription;
    
    // Created by information
    private UserResponseDTO createdBy;
    private Long createdByUserId;
    private String createdByUserName;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public PaymentResponseDTO() {}
    
    public PaymentResponseDTO(Long id, BigDecimal amount, PaymentMethod paymentMethod, String orderCode) {
        this.id = id;
        this.amount = amount;
        this.paymentMethod = paymentMethod;
        this.orderCode = orderCode;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
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
    
    public String getOrderCode() { return orderCode; }
    public void setOrderCode(String orderCode) { this.orderCode = orderCode; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public String getStatusCode() { return statusCode; }
    public void setStatusCode(String statusCode) { this.statusCode = statusCode; }
    
    public String getStatusDescription() { return statusDescription; }
    public void setStatusDescription(String statusDescription) { this.statusDescription = statusDescription; }
    
    public UserResponseDTO getCreatedBy() { return createdBy; }
    public void setCreatedBy(UserResponseDTO createdBy) { this.createdBy = createdBy; }
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
    
    public String getCreatedByUserName() { return createdByUserName; }
    public void setCreatedByUserName(String createdByUserName) { this.createdByUserName = createdByUserName; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public String getPaymentMethodDisplayName() {
        return paymentMethod != null ? paymentMethod.getDisplayName() : null;
    }
    
    public String getFormattedAmount() {
        return amount != null ? String.format("$%.2f", amount) : null;
    }
    
    public String getDisplayInfo() {
        return String.format("%s - %s (%s)", 
            orderCode != null ? orderCode : "Order #" + orderId, 
            getFormattedAmount(), 
            getPaymentMethodDisplayName());
    }
    
    public boolean hasTransactionId() {
        return transactionId != null && !transactionId.trim().isEmpty();
    }
}