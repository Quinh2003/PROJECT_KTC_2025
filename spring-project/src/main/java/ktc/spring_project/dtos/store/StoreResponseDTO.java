package ktc.spring_project.dtos.store;

import java.sql.Timestamp;

/**
 * DTO for store response data
 */
public class StoreResponseDTO {
    
    private Long id;

    private String storeName;
    private String email;
    private String phone;
    private String address;
    private Double latitude;
    private Double longitude;
    private Boolean isActive;
    private String notes;
    
    // Creator information
    private Long createdByUserId;
    private String createdByUserName;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Business metrics
    private Integer totalOrders;
    private Integer activeOrders;
    private Integer totalProducts;
    
    // Constructors
    public StoreResponseDTO() {}
    
    public StoreResponseDTO(Long id, String storeName, String phone, Boolean isActive) {
        this.id = id;
        this.storeName = storeName;
        this.phone = phone;
        this.isActive = isActive;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    

    
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
    
    public Long getCreatedByUserId() { return createdByUserId; }
    public void setCreatedByUserId(Long createdByUserId) { this.createdByUserId = createdByUserId; }
    
    public String getCreatedByUserName() { return createdByUserName; }
    public void setCreatedByUserName(String createdByUserName) { this.createdByUserName = createdByUserName; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    public Integer getTotalOrders() { return totalOrders; }
    public void setTotalOrders(Integer totalOrders) { this.totalOrders = totalOrders; }
    
    public Integer getActiveOrders() { return activeOrders; }
    public void setActiveOrders(Integer activeOrders) { this.activeOrders = activeOrders; }
    
    public Integer getTotalProducts() { return totalProducts; }
    public void setTotalProducts(Integer totalProducts) { this.totalProducts = totalProducts; }
    
    // Utility methods
    public String getDisplayName() {
        return storeName != null ? storeName : "Store #" + id;
    }
    
    public boolean isActiveStore() {
        return Boolean.TRUE.equals(isActive);
    }
    
    public boolean hasCoordinates() {
        return latitude != null && longitude != null;
    }
    
    public String getCoordinatesString() {
        if (hasCoordinates()) {
            return String.format("%.6f, %.6f", latitude, longitude);
        }
        return null;
    }
    
    public boolean hasEmail() {
        return email != null && !email.trim().isEmpty();
    }
    
    public String getStatusDisplay() {
        return isActiveStore() ? "Active" : "Inactive";
    }
    
    public boolean hasOrders() {
        return totalOrders != null && totalOrders > 0;
    }
    
    public boolean hasActiveOrders() {
        return activeOrders != null && activeOrders > 0;
    }
    
    public boolean hasProducts() {
        return totalProducts != null && totalProducts > 0;
    }
    
    public String getOrderSummary() {
        if (totalOrders == null) {
            return "No order data";
        }
        return String.format("%d total (%d active)", totalOrders, activeOrders != null ? activeOrders : 0);
    }
    
    public String getFullContactInfo() {
        StringBuilder contact = new StringBuilder();
        if (phone != null) {
            contact.append("Phone: ").append(phone);
        }
        if (hasEmail()) {
            if (contact.length() > 0) contact.append(" | ");
            contact.append("Email: ").append(email);
        }
        return contact.toString();
    }
}