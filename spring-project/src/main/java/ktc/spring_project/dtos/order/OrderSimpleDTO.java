package ktc.spring_project.dtos.order;

import java.math.BigDecimal;
import java.sql.Timestamp;
import lombok.Data;

/**
 * Simple DTO for order information in delivery lists
 */
@Data
public class OrderSimpleDTO {
    private Long id;
    private String orderCode;
    private String status;
    private Short statusId;
    private String deliveryAddress;
    private String recipientName;
    private String recipientPhone;
    private BigDecimal totalAmount;
    private Timestamp createdAt;
}
