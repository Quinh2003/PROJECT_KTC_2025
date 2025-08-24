package ktc.spring_project.dtos.invoice;

import jakarta.validation.constraints.*;
import java.math.BigDecimal;

/**
 * DTO cho yêu cầu tạo hóa đơn điện tử
 */
public class CreateInvoiceRequestDTO {

    @NotNull(message = "Order ID is required")
    private Long orderId;

    @Email(message = "Invalid email format")
    @Size(max = 255, message = "Email must not exceed 255 characters")
    private String customerEmail;

    @Size(max = 255, message = "Customer name must not exceed 255 characters")
    private String customerName;

    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;

    // Constructor mặc định
    public CreateInvoiceRequestDTO() {}

    // Constructor tiện ích
    public CreateInvoiceRequestDTO(Long orderId) {
        this.orderId = orderId;
    }

    public CreateInvoiceRequestDTO(Long orderId, String customerEmail, String customerName) {
        this.orderId = orderId;
        this.customerEmail = customerEmail;
        this.customerName = customerName;
    }

    // Validation methods

    /**
     * Kiểm tra DTO có hợp lệ không
     */
    public boolean isValid() {
        return orderId != null;
    }

    /**
     * Kiểm tra có thông tin khách hàng không
     */
    public boolean hasCustomerInfo() {
        return (customerEmail != null && !customerEmail.trim().isEmpty()) ||
               (customerName != null && !customerName.trim().isEmpty());
    }

    /**
     * Làm sạch dữ liệu đầu vào
     */
    public void sanitize() {
        if (customerEmail != null) {
            customerEmail = customerEmail.trim().toLowerCase();
        }
        if (customerName != null) {
            customerName = customerName.trim();
        }
        if (notes != null) {
            notes = notes.trim();
        }
    }

    // Getters và Setters

    public Long getOrderId() {
        return orderId;
    }

    public void setOrderId(Long orderId) {
        this.orderId = orderId;
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

    @Override
    public String toString() {
        return "CreateInvoiceRequestDTO{" +
                "orderId=" + orderId +
                ", customerEmail='" + customerEmail + '\'' +
                ", customerName='" + customerName + '\'' +
                ", notes='" + notes + '\'' +
                '}';
    }
}

