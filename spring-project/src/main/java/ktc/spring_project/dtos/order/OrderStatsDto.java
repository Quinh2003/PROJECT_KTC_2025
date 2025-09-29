package ktc.spring_project.dtos.order;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OrderStatsDto {
    private long totalOrders;
    private long processingOrders;
    private long completedOrders;
}