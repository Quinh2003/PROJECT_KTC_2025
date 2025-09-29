package ktc.spring_project.dtos.store;

import jakarta.validation.constraints.*;

/**
 * DTO for updating store information with restricted fields
 * Fields that can be updated: store_name, email, phone, is_active, notes
 * Address is displayed but cannot be updated through this DTO
 */
public class UpdateStoreInfoDTO {
    
    @NotBlank(message = "Store name is required")
    @Size(max = 255, message = "Store name must not exceed 255 characters")
    private String storeName;
    
    @Email(message = "Please provide a valid email address")
    @Size(max = 255, message = "Email must not exceed 255 characters")
    private String email;
    
    @NotBlank(message = "Phone number is required")
    @Size(max = 20, message = "Phone number must not exceed 20 characters")
    @Pattern(regexp = "^[0-9+\\-\\s()]*$", message = "Phone number contains invalid characters")
    private String phone;
    
    @NotNull(message = "Active status is required")
    private Boolean isActive;
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;
    
    // Read-only field for display purposes
    private String address; // This will be shown but not updated
    
    // Constructors
    public UpdateStoreInfoDTO() {}
    
    public UpdateStoreInfoDTO(String storeName, String email, String phone, Boolean isActive, String notes) {
        this.storeName = storeName;
        this.email = email;
        this.phone = phone;
        this.isActive = isActive;
        this.notes = notes;
    }
    
    // Getters and Setters
    public String getStoreName() { 
        return storeName; 
    }
    
    public void setStoreName(String storeName) { 
        this.storeName = storeName; 
    }
    
    public String getEmail() { 
        return email; 
    }
    
    public void setEmail(String email) { 
        this.email = email; 
    }
    
    public String getPhone() { 
        return phone; 
    }
    
    public void setPhone(String phone) { 
        this.phone = phone; 
    }
    
    public Boolean getIsActive() { 
        return isActive; 
    }
    
    public void setIsActive(Boolean isActive) { 
        this.isActive = isActive; 
    }
    
    public String getNotes() { 
        return notes; 
    }
    
    public void setNotes(String notes) { 
        this.notes = notes; 
    }
    
    public String getAddress() { 
        return address; 
    }
    
    public void setAddress(String address) { 
        this.address = address; 
    }
    
    // Utility methods for validation
    public boolean hasValidStoreName() {
        return storeName != null && !storeName.trim().isEmpty();
    }
    
    public boolean hasValidPhone() {
        return phone != null && !phone.trim().isEmpty();
    }
    
    public boolean hasEmail() {
        return email != null && !email.trim().isEmpty();
    }
    
    public boolean isEmailValid() {
        if (email == null || email.trim().isEmpty()) {
            return true; // Email is optional
        }
        return email.contains("@") && email.contains(".");
    }
    
    public boolean isActivating() {
        return Boolean.TRUE.equals(isActive);
    }
    
    public boolean isDeactivating() {
        return Boolean.FALSE.equals(isActive);
    }
    
    @Override
    public String toString() {
        return "UpdateStoreInfoDTO{" +
                "storeName='" + storeName + '\'' +
                ", email='" + email + '\'' +
                ", phone='" + phone + '\'' +
                ", isActive=" + isActive +
                ", notes='" + notes + '\'' +
                ", address='" + address + '\'' +
                '}';
    }
}