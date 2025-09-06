package ktc.spring_project.dtos.invoice;

import ktc.spring_project.entities.ElectronicInvoice;
import ktc.spring_project.enums.InvoiceStatus;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.time.format.DateTimeFormatter;

/**
 * DTO cho response hóa đơn thanh toán
 * Chứa đầy đủ thông tin hóa đơn để hiển thị
 */
public class InvoiceResponseDTO {

    private Long id;
    private Long orderId;
    private String invoiceNumber;
    private InvoiceStatus invoiceStatus;
    private String invoiceStatusDisplay;
    private Timestamp issuedAt;
    private String issuedAtFormatted;
    private BigDecimal totalAmount;
    private String totalAmountFormatted;
    private BigDecimal taxAmount;
    private String taxAmountFormatted;
    private BigDecimal netAmount;
    private String netAmountFormatted;
    private String pdfFilePath;
    private String pdfFileName;
    private Timestamp emailSentAt;
    private String emailSentAtFormatted;
    private String customerEmail;
    private String customerName;
    private String notes;
    private Long createdById;
    private String createdByName;
    private String createdByEmail;
    private Timestamp createdAt;
    private String createdAtFormatted;
    private Timestamp updatedAt;
    private Timestamp cancelledAt;
    private String cancelledAtFormatted;
    private Long cancelledById;
    private String cancelledByName;
    private String cancellationReason;

    // Order related information
    private String orderDescription;
    private String orderStatusName;
    private String storeInformation;

    // Constructor mặc định
    public InvoiceResponseDTO() {}

    // Constructor từ entity
    public InvoiceResponseDTO(ElectronicInvoice invoice) {
        this.id = invoice.getId();
        this.orderId = invoice.getOrder() != null ? invoice.getOrder().getId() : null;
        this.invoiceNumber = invoice.getInvoiceNumber();
        this.invoiceStatus = invoice.getInvoiceStatus();
        this.invoiceStatusDisplay = invoice.getInvoiceStatus() != null ? 
            invoice.getInvoiceStatus().getDisplayName() : "";
        this.issuedAt = invoice.getIssuedAt();
        this.totalAmount = invoice.getTotalAmount();
        this.taxAmount = invoice.getTaxAmount();
        this.netAmount = invoice.getNetAmount();
        this.pdfFilePath = invoice.getPdfFilePath();
        this.pdfFileName = invoice.getPdfFileName();
        this.emailSentAt = invoice.getEmailSentAt();
        this.customerEmail = invoice.getCustomerEmail();
        this.customerName = invoice.getCustomerName();
        this.notes = invoice.getNotes();
        this.createdAt = invoice.getCreatedAt();
        this.updatedAt = invoice.getUpdatedAt();
        this.cancelledAt = invoice.getCancelledAt();
        this.cancellationReason = invoice.getCancellationReason();

        // Set thông tin người tạo
        if (invoice.getCreatedBy() != null) {
            this.createdById = invoice.getCreatedBy().getId();
            this.createdByName = invoice.getCreatedBy().getFullName();
            this.createdByEmail = invoice.getCreatedBy().getEmail();
        }

        // Set thông tin người hủy
        if (invoice.getCancelledBy() != null) {
            this.cancelledById = invoice.getCancelledBy().getId();
            this.cancelledByName = invoice.getCancelledBy().getFullName();
        }

        // Set thông tin đơn hàng
        if (invoice.getOrder() != null) {
            this.orderDescription = invoice.getOrder().getDescription();
            if (invoice.getOrder().getStatus() != null) {
                this.orderStatusName = invoice.getOrder().getStatus().getName();
            }
            if (invoice.getOrder().getStore() != null) {
                this.storeInformation = invoice.getOrder().getStore().getStoreName();
            }
        }

        // Format các trường hiển thị
        formatDisplayFields();
    }

    // Business Logic Methods

    /**
     * Kiểm tra hóa đơn có thể được hủy không
     */
    public boolean isCancellable() {
        return invoiceStatus != null && invoiceStatus.isCancellable();
    }

    /**
     * Kiểm tra hóa đơn đã hoàn thành không
     */
    public boolean isCompleted() {
        return invoiceStatus != null && invoiceStatus.isCompleted();
    }

    /**
     * Kiểm tra có file PDF không
     */
    public boolean hasPdfFile() {
        return pdfFilePath != null && !pdfFilePath.isEmpty();
    }

    /**
     * Kiểm tra đã gửi email không
     */
    public boolean isEmailSent() {
        return emailSentAt != null;
    }

    /**
     * Lấy tên hiển thị ngắn gọn
     */
    public String getDisplayName() {
        return String.format("%s - %s", invoiceNumber, 
            customerName != null ? customerName : "N/A");
    }

    /**
     * Lấy thông tin tóm tắt
     */
    public String getSummary() {
        return String.format("Hóa đơn %s - %s - %s", 
            invoiceNumber, 
            totalAmountFormatted != null ? totalAmountFormatted : "0",
            invoiceStatusDisplay);
    }

    /**
     * Format các trường để hiển thị
     */
    private void formatDisplayFields() {
        DecimalFormat currencyFormat = new DecimalFormat("#,##0.00");
        DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm");

        // Format tiền tệ
        if (totalAmount != null) {
            totalAmountFormatted = currencyFormat.format(totalAmount) + " VND";
        }
        if (taxAmount != null) {
            taxAmountFormatted = currencyFormat.format(taxAmount) + " VND";
        }
        if (netAmount != null) {
            netAmountFormatted = currencyFormat.format(netAmount) + " VND";
        }

        // Format ngày tháng
        if (issuedAt != null) {
            issuedAtFormatted = issuedAt.toLocalDateTime().format(dateFormat);
        }
        if (emailSentAt != null) {
            emailSentAtFormatted = emailSentAt.toLocalDateTime().format(dateFormat);
        }
        if (createdAt != null) {
            createdAtFormatted = createdAt.toLocalDateTime().format(dateFormat);
        }
        if (cancelledAt != null) {
            cancelledAtFormatted = cancelledAt.toLocalDateTime().format(dateFormat);
        }
    }

    // Getters và Setters

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }

    public String getInvoiceNumber() { return invoiceNumber; }
    public void setInvoiceNumber(String invoiceNumber) { this.invoiceNumber = invoiceNumber; }

    public InvoiceStatus getInvoiceStatus() { return invoiceStatus; }
    public void setInvoiceStatus(InvoiceStatus invoiceStatus) { this.invoiceStatus = invoiceStatus; }

    public String getInvoiceStatusDisplay() { return invoiceStatusDisplay; }
    public void setInvoiceStatusDisplay(String invoiceStatusDisplay) { this.invoiceStatusDisplay = invoiceStatusDisplay; }

    public Timestamp getIssuedAt() { return issuedAt; }
    public void setIssuedAt(Timestamp issuedAt) { this.issuedAt = issuedAt; }

    public String getIssuedAtFormatted() { return issuedAtFormatted; }
    public void setIssuedAtFormatted(String issuedAtFormatted) { this.issuedAtFormatted = issuedAtFormatted; }

    public BigDecimal getTotalAmount() { return totalAmount; }
    public void setTotalAmount(BigDecimal totalAmount) { this.totalAmount = totalAmount; }

    public String getTotalAmountFormatted() { return totalAmountFormatted; }
    public void setTotalAmountFormatted(String totalAmountFormatted) { this.totalAmountFormatted = totalAmountFormatted; }

    public BigDecimal getTaxAmount() { return taxAmount; }
    public void setTaxAmount(BigDecimal taxAmount) { this.taxAmount = taxAmount; }

    public String getTaxAmountFormatted() { return taxAmountFormatted; }
    public void setTaxAmountFormatted(String taxAmountFormatted) { this.taxAmountFormatted = taxAmountFormatted; }

    public BigDecimal getNetAmount() { return netAmount; }
    public void setNetAmount(BigDecimal netAmount) { this.netAmount = netAmount; }

    public String getNetAmountFormatted() { return netAmountFormatted; }
    public void setNetAmountFormatted(String netAmountFormatted) { this.netAmountFormatted = netAmountFormatted; }

    public String getPdfFilePath() { return pdfFilePath; }
    public void setPdfFilePath(String pdfFilePath) { this.pdfFilePath = pdfFilePath; }

    public String getPdfFileName() { return pdfFileName; }
    public void setPdfFileName(String pdfFileName) { this.pdfFileName = pdfFileName; }

    public Timestamp getEmailSentAt() { return emailSentAt; }
    public void setEmailSentAt(Timestamp emailSentAt) { this.emailSentAt = emailSentAt; }

    public String getEmailSentAtFormatted() { return emailSentAtFormatted; }
    public void setEmailSentAtFormatted(String emailSentAtFormatted) { this.emailSentAtFormatted = emailSentAtFormatted; }

    public String getCustomerEmail() { return customerEmail; }
    public void setCustomerEmail(String customerEmail) { this.customerEmail = customerEmail; }

    public String getCustomerName() { return customerName; }
    public void setCustomerName(String customerName) { this.customerName = customerName; }

    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }

    public Long getCreatedById() { return createdById; }
    public void setCreatedById(Long createdById) { this.createdById = createdById; }

    public String getCreatedByName() { return createdByName; }
    public void setCreatedByName(String createdByName) { this.createdByName = createdByName; }

    public String getCreatedByEmail() { return createdByEmail; }
    public void setCreatedByEmail(String createdByEmail) { this.createdByEmail = createdByEmail; }

    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }

    public String getCreatedAtFormatted() { return createdAtFormatted; }
    public void setCreatedAtFormatted(String createdAtFormatted) { this.createdAtFormatted = createdAtFormatted; }

    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }

    public Timestamp getCancelledAt() { return cancelledAt; }
    public void setCancelledAt(Timestamp cancelledAt) { this.cancelledAt = cancelledAt; }

    public String getCancelledAtFormatted() { return cancelledAtFormatted; }
    public void setCancelledAtFormatted(String cancelledAtFormatted) { this.cancelledAtFormatted = cancelledAtFormatted; }

    public Long getCancelledById() { return cancelledById; }
    public void setCancelledById(Long cancelledById) { this.cancelledById = cancelledById; }

    public String getCancelledByName() { return cancelledByName; }
    public void setCancelledByName(String cancelledByName) { this.cancelledByName = cancelledByName; }

    public String getCancellationReason() { return cancellationReason; }
    public void setCancellationReason(String cancellationReason) { this.cancellationReason = cancellationReason; }

    public String getOrderDescription() { return orderDescription; }
    public void setOrderDescription(String orderDescription) { this.orderDescription = orderDescription; }

    public String getOrderStatusName() { return orderStatusName; }
    public void setOrderStatusName(String orderStatusName) { this.orderStatusName = orderStatusName; }

    public String getStoreInformation() { return storeInformation; }
    public void setStoreInformation(String storeInformation) { this.storeInformation = storeInformation; }
}

