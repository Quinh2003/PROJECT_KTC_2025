package ktc.spring_project.dtos.order;

import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * DTO for updating an order's status
 */
@Data
public class OrderStatusUpdateDTO {
    @NotNull(message = "Status ID is required")
    private Short statusId;
    
    private String notes; // Optional notes about the status change
}
