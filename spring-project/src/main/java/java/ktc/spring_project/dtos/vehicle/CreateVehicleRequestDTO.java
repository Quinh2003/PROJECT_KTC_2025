package java.ktc.spring_project.dtos.vehicle;

import java.ktc.spring_project.enums.VehicleStatus;
import jakarta.validation.constraints.*;
import java.time.LocalDateTime;

/**
 * DTO for creating new vehicles
 */
public class CreateVehicleRequestDTO {
    
    @NotBlank(message = "License plate is required")
    @Size(max = 20, message = "License plate must not exceed 20 characters")
    private String licensePlate;
    
    @Size(max = 50, message = "Vehicle type must not exceed 50 characters")
    private String vehicleType;
    
    @DecimalMin(value = "0.0", message = "Capacity must be positive")
    @DecimalMax(value = "100.0", message = "Capacity must not exceed 100 tons")
    private Double capacity;
    
    @Size(max = 30, message = "Fuel type must not exceed 30 characters")
    private String fuelType;
    
    private LocalDateTime registrationDate;
    
    // Constructors
    public CreateVehicleRequestDTO() {}
    
    public CreateVehicleRequestDTO(String licensePlate, String vehicleType) {
        this.licensePlate = licensePlate;
        this.vehicleType = vehicleType;
    }
    
    // Getters and Setters
    public String getLicensePlate() { return licensePlate; }
    public void setLicensePlate(String licensePlate) { this.licensePlate = licensePlate; }
    
    public String getVehicleType() { return vehicleType; }
    public void setVehicleType(String vehicleType) { this.vehicleType = vehicleType; }
    
    public Double getCapacity() { return capacity; }
    public void setCapacity(Double capacity) { this.capacity = capacity; }
    
    public String getFuelType() { return fuelType; }
    public void setFuelType(String fuelType) { this.fuelType = fuelType; }
    
    public LocalDateTime getRegistrationDate() { return registrationDate; }
    public void setRegistrationDate(LocalDateTime registrationDate) { this.registrationDate = registrationDate; }
}
