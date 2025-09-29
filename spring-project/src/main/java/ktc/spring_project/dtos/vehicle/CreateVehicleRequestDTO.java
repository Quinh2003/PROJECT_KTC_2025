package ktc.spring_project.dtos.vehicle;

import jakarta.validation.constraints.*;
import java.math.BigDecimal;

/**
 * DTO for creating new vehicles
 */
public class CreateVehicleRequestDTO {
    @NotBlank(message = "License plate is required")
    @Size(max = 20, message = "License plate must not exceed 20 characters")
    private String licensePlate;

    @NotBlank(message = "Vehicle type is required")
    @Size(max = 50, message = "Vehicle type must not exceed 50 characters")
    private String vehicleType;

    @DecimalMin(value = "0.0", message = "Capacity must be positive")
    private BigDecimal capacity;

    // NEW: Thể tích xe (m3)
    @DecimalMin(value = "0.0", message = "Capacity volume must be positive")
    private BigDecimal capacityVolumeM3;

    @Size(max = 500, message = "Notes must not exceed 500 characters")
    private String notes;

    private Long statusId;

    // Constructors
    public CreateVehicleRequestDTO() {}

    public CreateVehicleRequestDTO(String licensePlate, String vehicleType, BigDecimal capacity, BigDecimal capacityVolumeM3) {
        this.licensePlate = licensePlate;
        this.vehicleType = vehicleType;
        this.capacity = capacity;
        this.capacityVolumeM3 = capacityVolumeM3;
    }

    // Getters and Setters
    public String getLicensePlate() { return licensePlate; }
    public void setLicensePlate(String licensePlate) { this.licensePlate = licensePlate; }

    public String getVehicleType() { return vehicleType; }
    public void setVehicleType(String vehicleType) { this.vehicleType = vehicleType; }

    public BigDecimal getCapacity() { return capacity; }
    public void setCapacity(BigDecimal capacity) { this.capacity = capacity; }

    public BigDecimal getCapacityVolumeM3() { return capacityVolumeM3; }
    public void setCapacityVolumeM3(BigDecimal capacityVolumeM3) { this.capacityVolumeM3 = capacityVolumeM3; }

    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }

    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
}
