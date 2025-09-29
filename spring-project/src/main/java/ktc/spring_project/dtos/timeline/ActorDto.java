package ktc.spring_project.dtos.timeline;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ActorDto {
    private Long userId;
    private String fullName;
    private String role;
    private String phone;
}