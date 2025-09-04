package ktc.spring_project.dtos.order;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class OrderListResponseDTO {
    private String id;                // Mã đơn hàng
    private LocalDateTime createdAt;  // Ngày tạo
    private String shippingAddress;   // Địa chỉ nhận hàng
    private Integer totalItems;       // Số sản phẩm
    private Double deliveryFee;       // Phí vận chuyển
    private StatusInfo status;        // Thông tin trạng thái

    public OrderListResponseDTO(
            String id, 
            LocalDateTime createdAt, 
            String shippingAddress, 
            Integer totalItems,
            Double deliveryFee,
            Long statusId,
            String statusName,
            String statusColor) {
        this.id = id;
        this.createdAt = createdAt;
        this.shippingAddress = shippingAddress;
        this.totalItems = totalItems;
        this.deliveryFee = deliveryFee;
        this.status = new StatusInfo(statusId, statusName, statusColor);
    }

    @Data
    @AllArgsConstructor
    public static class StatusInfo {
        private Long id;
        private String name;
        private String color;
    }
}