package ktc.spring_project.dtos.warehouse;

import jakarta.validation.constraints.*;
import java.math.BigDecimal;

/**
 * DTO for Warehouse creation requests
 * Used when creating a new warehouse facility
 */
public class CreateWarehouseRequestDTO {
    
    @NotBlank(message = "Warehouse name is required")
    @Size(min = 2, max = 255, message = "Warehouse name must be between 2 and 255 characters")
    private String name;
    
    @NotBlank(message = "Address is required")
    @Size(min = 10, max = 500, message = "Address must be between 10 and 500 characters")
    private String address;
    
    @DecimalMin(value = "0.01", message = "Capacity must be greater than 0")
    @Digits(integer = 8, fraction = 2, message = "Capacity must have at most 8 integer digits and 2 decimal places")
    private BigDecimal capacity; // Storage capacity in m³
    
    @DecimalMin(value = "0.01", message = "Max weight must be greater than 0")
    @Digits(integer = 8, fraction = 2, message = "Max weight must have at most 8 integer digits and 2 decimal places")
    private BigDecimal maxWeight; // Maximum weight capacity in tons
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;
    
    // Constructors
    public CreateWarehouseRequestDTO() {}
    
    public CreateWarehouseRequestDTO(String name, String address, BigDecimal capacity, 
                                   BigDecimal maxWeight, String notes) {
        this.name = name;
        this.address = address;
        this.capacity = capacity;
        this.maxWeight = maxWeight;
        this.notes = notes;
    }
    
    // Getters and Setters
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getAddress() {
        return address;
    }
    
    public void setAddress(String address) {
        this.address = address;
    }
    
    public BigDecimal getCapacity() {
        return capacity;
    }
    
    public void setCapacity(BigDecimal capacity) {
        this.capacity = capacity;
    }
    
    public BigDecimal getMaxWeight() {
        return maxWeight;
    }
    
    public void setMaxWeight(BigDecimal maxWeight) {
        this.maxWeight = maxWeight;
    }
    
    public String getNotes() {
        return notes;
    }
    
    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    @Override
    public String toString() {
        return "CreateWarehouseRequestDTO{" +
                "name='" + name + '\'' +
                ", address='" + address + '\'' +
                ", capacity=" + capacity +
                ", maxWeight=" + maxWeight +
                ", notes='" + notes + '\'' +
                '}';
    }
}