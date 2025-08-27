package ktc.spring_project.dtos;

import ktc.spring_project.enums.ServiceType;
import lombok.Builder;
import lombok.Data;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import ktc.spring_project.config.MoneyFormatSerializer;

import java.math.BigDecimal;

@Data
@Builder
public class DeliveryFeeBreakdown {
    
    private Long orderId;
    
    @JsonSerialize(using = MoneyFormatSerializer.class)
    private BigDecimal totalShippingFee;
    
    @JsonSerialize(using = MoneyFormatSerializer.class)
    private BigDecimal distanceFee;
    
    private BigDecimal distance;
    
    private ServiceType serviceType;
    
    private BigDecimal serviceMultiplier;
    
    @JsonSerialize(using = MoneyFormatSerializer.class)
    private BigDecimal baseDeliveryFee;
    
    @JsonSerialize(using = MoneyFormatSerializer.class)
    private BigDecimal totalDeliveryFee;
    
    private String calculationDetails;
}
