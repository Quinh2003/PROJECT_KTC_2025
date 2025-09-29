package ktc.spring_project.dtos.timeline;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OrderStatusDto {
    private Short statusId;
    private String statusName;
    private String statusDescription;
}