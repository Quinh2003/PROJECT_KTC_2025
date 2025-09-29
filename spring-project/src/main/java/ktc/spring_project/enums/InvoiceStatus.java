package ktc.spring_project.enums;

/**
 * Enum cho trạng thái hóa đơn thanh toán
 * Định nghĩa các trạng thái trong vòng đời của hóa đơn thanh toán
 */
public enum InvoiceStatus {
    CREATED("Đã tạo", "Invoice has been created but not sent"),
    SENT("Đã gửi", "Invoice has been sent to customer"),
    DELIVERED("Đã nhận", "Invoice has been received by customer"),
    CANCELLED("Đã hủy", "Invoice has been cancelled"),
    EXPIRED("Đã hết hạn", "Invoice has expired without being sent");

    private final String displayName;
    private final String description;

    InvoiceStatus(String displayName, String description) {
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
     * Kiểm tra xem hóa đơn có thể được hủy không
     */
    public boolean isCancellable() {
        return this == CREATED || this == EXPIRED;
    }

    /**
     * Kiểm tra xem hóa đơn đã được hoàn thành không (gửi hoặc nhận)
     */
    public boolean isCompleted() {
        return this == SENT || this == DELIVERED;
    }
}

