package ktc.spring_project.entities;

import jakarta.persistence.*;
import ktc.spring_project.enums.InvoiceStatus;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * Entity cho hóa đơn thanh toán
 * Quản lý thông tin hóa đơn thanh toán được xuất sau khi hoàn thành giao hàng
 */
@Entity
@Table(name = "electronic_invoices")
public class ElectronicInvoice {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id", nullable = false)
    private Order order;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "delivery_id", nullable = false)
    private Delivery delivery;

    @Column(name = "invoice_number", length = 50, nullable = false, unique = true)
    private String invoiceNumber;

    @Enumerated(EnumType.STRING)
    @Column(name = "invoice_status", length = 30, nullable = false)
    private InvoiceStatus invoiceStatus = InvoiceStatus.CREATED;

    @Column(name = "issued_at", nullable = false)
    private Timestamp issuedAt;

    @Column(name = "total_amount", precision = 15, scale = 2, nullable = false)
    private BigDecimal totalAmount;

    @Column(name = "tax_amount", precision = 15, scale = 2)
    private BigDecimal taxAmount = BigDecimal.ZERO;

    @Column(name = "net_amount", precision = 15, scale = 2, nullable = false)
    private BigDecimal netAmount;

    @Column(name = "pdf_file_path", length = 500)
    private String pdfFilePath;

    @Column(name = "pdf_file_name", length = 255)
    private String pdfFileName;

    @Column(name = "email_sent_at")
    private Timestamp emailSentAt;

    @Column(name = "customer_email", length = 255)
    private String customerEmail;

    @Column(name = "customer_name", length = 255)
    private String customerName;

    @Column(columnDefinition = "TEXT")
    private String notes;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by")
    private User createdBy;

    @CreationTimestamp
    @Column(name = "created_at")
    private Timestamp createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private Timestamp updatedAt;

    @Column(name = "cancelled_at")
    private Timestamp cancelledAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "cancelled_by")
    private User cancelledBy;

    @Column(name = "cancellation_reason", columnDefinition = "TEXT")
    private String cancellationReason;

    // Constructor mặc định
    public ElectronicInvoice() {}

    // Constructor tiện ích
    public ElectronicInvoice(Order order, Delivery delivery, String invoiceNumber, BigDecimal totalAmount, User createdBy) {
        this.order = order;
        this.delivery = delivery;
        this.invoiceNumber = invoiceNumber;
        this.totalAmount = totalAmount;
        this.netAmount = totalAmount; // Mặc định net = total, sẽ được tính lại khi có thuế
        this.createdBy = createdBy;
        this.issuedAt = new Timestamp(System.currentTimeMillis());
        this.invoiceStatus = InvoiceStatus.CREATED;
    }

    // Business Logic Methods

    /**
     * Kiểm tra xem hóa đơn có thể được hủy không
     */
    public boolean canBeCancelled() {
        return invoiceStatus != null && invoiceStatus.isCancellable();
    }

    /**
     * Kiểm tra xem hóa đơn đã hoàn thành không
     */
    public boolean isCompleted() {
        return invoiceStatus != null && invoiceStatus.isCompleted();
    }

    /**
     * Hủy hóa đơn với lý do
     */
    public void cancel(User cancelledByUser, String reason) {
        if (!canBeCancelled()) {
            throw new IllegalStateException("Không thể hủy hóa đơn ở trạng thái: " + invoiceStatus);
        }
        this.invoiceStatus = InvoiceStatus.CANCELLED;
        this.cancelledAt = new Timestamp(System.currentTimeMillis());
        this.cancelledBy = cancelledByUser;
        this.cancellationReason = reason;
    }

    /**
     * Đánh dấu hóa đơn đã được gửi
     */
    public void markAsSent(String emailAddress) {
        this.invoiceStatus = InvoiceStatus.SENT;
        this.emailSentAt = new Timestamp(System.currentTimeMillis());
        this.customerEmail = emailAddress;
    }

    /**
     * Tính toán thuế VAT (10%)
     */
    public void calculateTax() {
        if (totalAmount != null) {
            this.taxAmount = totalAmount.multiply(new BigDecimal("0.10")); // 10% VAT
            this.netAmount = totalAmount.subtract(taxAmount);
        }
    }

    /**
     * Sinh tên file PDF dựa trên số hóa đơn
     */
    public String generatePdfFileName() {
        return "invoice_" + invoiceNumber + "_" + System.currentTimeMillis() + ".pdf";
    }

    // Getters và Setters

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Order getOrder() {
        return order;
    }

    public void setOrder(Order order) {
        this.order = order;
    }

    public Delivery getDelivery() {
        return delivery;
    }

    public void setDelivery(Delivery delivery) {
        this.delivery = delivery;
    }

    public String getInvoiceNumber() {
        return invoiceNumber;
    }

    public void setInvoiceNumber(String invoiceNumber) {
        this.invoiceNumber = invoiceNumber;
    }

    public InvoiceStatus getInvoiceStatus() {
        return invoiceStatus;
    }

    public void setInvoiceStatus(InvoiceStatus invoiceStatus) {
        this.invoiceStatus = invoiceStatus;
    }

    public Timestamp getIssuedAt() {
        return issuedAt;
    }

    public void setIssuedAt(Timestamp issuedAt) {
        this.issuedAt = issuedAt;
    }

    public BigDecimal getTotalAmount() {
        return totalAmount;
    }

    public void setTotalAmount(BigDecimal totalAmount) {
        this.totalAmount = totalAmount;
    }

    public BigDecimal getTaxAmount() {
        return taxAmount;
    }

    public void setTaxAmount(BigDecimal taxAmount) {
        this.taxAmount = taxAmount;
    }

    public BigDecimal getNetAmount() {
        return netAmount;
    }

    public void setNetAmount(BigDecimal netAmount) {
        this.netAmount = netAmount;
    }

    public String getPdfFilePath() {
        return pdfFilePath;
    }

    public void setPdfFilePath(String pdfFilePath) {
        this.pdfFilePath = pdfFilePath;
    }

    public String getPdfFileName() {
        return pdfFileName;
    }

    public void setPdfFileName(String pdfFileName) {
        this.pdfFileName = pdfFileName;
    }

    public Timestamp getEmailSentAt() {
        return emailSentAt;
    }

    public void setEmailSentAt(Timestamp emailSentAt) {
        this.emailSentAt = emailSentAt;
    }

    public String getCustomerEmail() {
        return customerEmail;
    }

    public void setCustomerEmail(String customerEmail) {
        this.customerEmail = customerEmail;
    }

    public String getCustomerName() {
        return customerName;
    }

    public void setCustomerName(String customerName) {
        this.customerName = customerName;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public User getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(User createdBy) {
        this.createdBy = createdBy;
    }

    public Timestamp getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Timestamp createdAt) {
        this.createdAt = createdAt;
    }

    public Timestamp getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(Timestamp updatedAt) {
        this.updatedAt = updatedAt;
    }

    public Timestamp getCancelledAt() {
        return cancelledAt;
    }

    public void setCancelledAt(Timestamp cancelledAt) {
        this.cancelledAt = cancelledAt;
    }

    public User getCancelledBy() {
        return cancelledBy;
    }

    public void setCancelledBy(User cancelledBy) {
        this.cancelledBy = cancelledBy;
    }

    public String getCancellationReason() {
        return cancellationReason;
    }

    public void setCancellationReason(String cancellationReason) {
        this.cancellationReason = cancellationReason;
    }
}

