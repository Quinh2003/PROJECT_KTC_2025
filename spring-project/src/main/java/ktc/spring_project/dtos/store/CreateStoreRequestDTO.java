package ktc.spring_project.dtos.store;

/**
 * DTO for creating a new store
 */
public class CreateStoreRequestDTO {
    

    private String storeName;
    private String email;
    private String phone;
    private String address;
    private Double latitude;
    private Double longitude;
    private Boolean isActive = true;
    private String notes;
    
    // Constructors
    public CreateStoreRequestDTO() {}
    
    public CreateStoreRequestDTO(String storeName, String phone, String address) {
        this.storeName = storeName;
        this.phone = phone;
        this.address = address;
    }
    
    // Getters and Setters

    
    public String getStoreName() { return storeName; }
    public void setStoreName(String storeName) { this.storeName = storeName; }
    
    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }
    
    public String getPhone() { return phone; }
    public void setPhone(String phone) { this.phone = phone; }
    
    public String getAddress() { return address; }
    public void setAddress(String address) { this.address = address; }
    
    public Double getLatitude() { return latitude; }
    public void setLatitude(Double latitude) { this.latitude = latitude; }
    
    public Double getLongitude() { return longitude; }
    public void setLongitude(Double longitude) { this.longitude = longitude; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    // Validation methods
    public boolean isValid() {
        return storeName != null && !storeName.trim().isEmpty() &&
               phone != null && !phone.trim().isEmpty() &&
               address != null && !address.trim().isEmpty();
    }
    
    public boolean hasCoordinates() {
        return latitude != null && longitude != null;
    }
    
    public boolean hasEmail() {
        return email != null && !email.trim().isEmpty();
    }
    
    public String getDisplayName() {
        return storeName != null ? storeName : "New Store";
    }
    
    public boolean isEmailValid() {
        if (email == null || email.trim().isEmpty()) {
            return true; // Email is optional
        }
        return email.contains("@") && email.contains(".");
    }
}