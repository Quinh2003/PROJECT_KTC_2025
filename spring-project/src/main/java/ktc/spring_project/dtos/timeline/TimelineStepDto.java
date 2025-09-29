package ktc.spring_project.dtos.timeline;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TimelineStepDto {
    private Integer stepOrder;
    private String stepCode;
    private String stepName;
    private String description;
    private Boolean completed;
    private LocalDateTime completedAt;
    private ActorDto actor;
    private String details;
    private String status;
}