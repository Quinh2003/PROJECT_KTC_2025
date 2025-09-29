package ktc.spring_project.dtos.maintenance;

import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * DTO for creating monthly maintenance requests by fleet managers
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateMonthlyMaintenanceRequestDTO {
    
    @NotNull(message = "Vehicle ID is required")
    @Positive(message = "Vehicle ID must be positive")
    private Long vehicleId;
    
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    private LocalDateTime scheduledMaintenanceDate;
    
    private BigDecimal cost;
    
    private String notes;
}