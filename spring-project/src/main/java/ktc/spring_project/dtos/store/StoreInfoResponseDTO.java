package ktc.spring_project.dtos.store;

import java.sql.Timestamp;

/**
 * DTO for store information response
 * Shows all store information including address (read-only)
 */
public class StoreInfoResponseDTO {
    
    private Long id;
    private String storeName;
    private String email;
    private String phone;
    private String address; // Read-only, cannot be updated via update endpoint
    private Boolean isActive;
    private String notes;
    private Timestamp createdAt;
    private Timestamp updatedAt;
    private String createdByUserName;
    
    // Constructor
    public StoreInfoResponseDTO() {}
    
    public StoreInfoResponseDTO(Long id, String storeName, String email, String phone, 
                               String address, Boolean isActive, String notes, 
                               Timestamp createdAt, Timestamp updatedAt, String createdByUserName) {
        this.id = id;
        this.storeName = storeName;
        this.email = email;
        this.phone = phone;
        this.address = address;
        this.isActive = isActive;
        this.notes = notes;
        this.createdAt = createdAt;
        this.updatedAt = updatedAt;
        this.createdByUserName = createdByUserName;
    }
    
    // Getters and Setters
    public Long getId() {
        return id;
    }
    
    public void setId(Long id) {
        this.id = id;
    }
    
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
    
    public String getAddress() {
        return address;
    }
    
    public void setAddress(String address) {
        this.address = address;
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
    
    public Timestamp getCreatedAt() {
        return createdAt;
    }
    
    public void setCreatedAt(Timestamp createdAt) {
        this.createdAt = createdAt;
    }
    
    public Timestamp getUpdatedAt() {
        return updatedAt;
    }
    
    public void setUpdatedAt(Timestamp updatedAt) {
        this.updatedAt = updatedAt;
    }
    
    public String getCreatedByUserName() {
        return createdByUserName;
    }
    
    public void setCreatedByUserName(String createdByUserName) {
        this.createdByUserName = createdByUserName;
    }
    
    // Utility methods
    public boolean isActiveStore() {
        return Boolean.TRUE.equals(isActive);
    }
    
    public String getDisplayName() {
        return storeName != null ? storeName : "Store #" + id;
    }
    
    @Override
    public String toString() {
        return "StoreInfoResponseDTO{" +
                "id=" + id +
                ", storeName='" + storeName + '\'' +
                ", email='" + email + '\'' +
                ", phone='" + phone + '\'' +
                ", address='" + address + '\'' +
                ", isActive=" + isActive +
                ", notes='" + notes + '\'' +
                ", createdAt=" + createdAt +
                ", updatedAt=" + updatedAt +
                ", createdByUserName='" + createdByUserName + '\'' +
                '}';
    }
}