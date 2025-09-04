package ktc.spring_project.dtos.order;

import java.math.BigDecimal;
import java.sql.Timestamp;
import lombok.Data;
import lombok.Builder;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OrderSummaryDTO {
    private Long orderId;
    private Long storeId;
    private Timestamp createdAt;
    private String deliveryAddress;
    private Integer totalItems;
    private BigDecimal deliveryFee;
    private String orderStatus;

    // Constructor for JPA projection
    public OrderSummaryDTO(Long orderId, Long storeId, Timestamp createdAt, String deliveryAddress, Long totalItems, BigDecimal deliveryFee, String orderStatus) {
        this.orderId = orderId;
        this.storeId = storeId;
        this.createdAt = createdAt;
        this.deliveryAddress = deliveryAddress;
        this.totalItems = totalItems != null ? totalItems.intValue() : 0;
        this.deliveryFee = deliveryFee;
        this.orderStatus = orderStatus;
    }
}
