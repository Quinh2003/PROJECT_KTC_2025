package ktc.spring_project.dtos.order;

import java.math.BigDecimal;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OrderByStoreResponseDTO {
    private Long id;
    private String createdByUsername;
    private String fullAddress;
    private BigDecimal deliveryFee; 
    private String statusName;
    private Integer totalItems;
}
