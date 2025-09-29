package ktc.spring_project.dtos.timeline;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OrderTimelineResponse {
    private Long orderId;
    private String orderCode;
    
    // Order status info
    private OrderStatusDto orderStatus;
    
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    
    // Customer info
    private ActorDto customer;
    
    // Driver info (nullable if not assigned)
    private ActorDto driver;
    
    // Timeline steps
    private List<TimelineStepDto> timeline;
}