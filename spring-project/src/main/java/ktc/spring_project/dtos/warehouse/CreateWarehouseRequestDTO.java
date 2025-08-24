package ktc.spring_project.dtos.warehouse;

import java.math.BigDecimal;

/**
 * DTO for creating a new warehouse
 */
public class CreateWarehouseRequestDTO {
    

    private String name;
    private String address;
    private BigDecimal latitude;
    private BigDecimal longitude;
    private BigDecimal capacityM3;
    private Boolean isActive = true;
    private String notes;
    
    // Constructors
    public CreateWarehouseRequestDTO() {}
    
    public CreateWarehouseRequestDTO(String name, String address) {
        this.name = name;
        this.address = address;
    }
    
    public CreateWarehouseRequestDTO(String name, String address, BigDecimal capacityM3) {
        this.name = name;
        this.address = address;
        this.capacityM3 = capacityM3;
    }
    
    // Getters and Setters
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getAddress() { return address; }
    public void setAddress(String address) { this.address = address; }
    
    public BigDecimal getLatitude() { return latitude; }
    public void setLatitude(BigDecimal latitude) { this.latitude = latitude; }
    
    public BigDecimal getLongitude() { return longitude; }
    public void setLongitude(BigDecimal longitude) { this.longitude = longitude; }
    
    public BigDecimal getCapacityM3() { return capacityM3; }
    public void setCapacityM3(BigDecimal capacityM3) { this.capacityM3 = capacityM3; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    // Validation methods
    public boolean isValid() {
        return name != null && !name.trim().isEmpty() &&
               address != null && !address.trim().isEmpty();
    }
    
    public boolean hasCoordinates() {
        return latitude != null && longitude != null;
    }
    
    public boolean hasCapacity() {
        return capacityM3 != null && capacityM3.compareTo(BigDecimal.ZERO) > 0;
    }
    
    public String getDisplayName() {
        return name;
    }
    
    public boolean isCapacityValid() {
        return capacityM3 == null || capacityM3.compareTo(BigDecimal.ZERO) >= 0;
    }
    
    public String getFormattedCapacity() {
        if (hasCapacity()) {
            return String.format("%.2f m³", capacityM3);
        }
        return "No capacity specified";
    }
}