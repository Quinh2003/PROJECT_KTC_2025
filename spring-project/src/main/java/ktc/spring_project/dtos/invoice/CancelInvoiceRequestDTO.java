package ktc.spring_project.dtos.invoice;

import jakarta.validation.constraints.*;

/**
 * DTO cho yêu cầu hủy hóa đơn điện tử
 */
public class CancelInvoiceRequestDTO {

    @NotBlank(message = "Cancellation reason is required")
    @Size(max = 1000, message = "Cancellation reason must not exceed 1000 characters")
    private String cancellationReason;

    // Constructor mặc định
    public CancelInvoiceRequestDTO() {}

    // Constructor với lý do
    public CancelInvoiceRequestDTO(String cancellationReason) {
        this.cancellationReason = cancellationReason;
    }

    // Validation methods

    /**
     * Kiểm tra DTO có hợp lệ không
     */
    public boolean isValid() {
        return cancellationReason != null && !cancellationReason.trim().isEmpty();
    }

    /**
     * Làm sạch dữ liệu đầu vào
     */
    public void sanitize() {
        if (cancellationReason != null) {
            cancellationReason = cancellationReason.trim();
        }
    }

    // Getters và Setters

    public String getCancellationReason() {
        return cancellationReason;
    }

    public void setCancellationReason(String cancellationReason) {
        this.cancellationReason = cancellationReason;
    }

    @Override
    public String toString() {
        return "CancelInvoiceRequestDTO{" +
                "cancellationReason='" + cancellationReason + '\'' +
                '}';
    }
}

