package ktc.spring_project.dtos.vehicle;

import lombok.Data;

/**
 * Simple DTO for vehicle information
 */
@Data
public class VehicleSimpleDTO {
    private Long id;
    private String licensePlate;
    private String vehicleType;
    private String model;
}
