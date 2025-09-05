package ktc.spring_project.dtos.tracking;

import java.math.BigDecimal;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * DTO for updating the location of a delivery
 */
@Data
public class LocationUpdateDTO {
    @NotNull(message = "Latitude is required")
    private BigDecimal latitude;
    
    @NotNull(message = "Longitude is required")
    private BigDecimal longitude;
    
    private String location; // Address or location name if available
    
    private String notes; // Any additional notes
    
    private Short statusId; // Optional status update alongside location
}
