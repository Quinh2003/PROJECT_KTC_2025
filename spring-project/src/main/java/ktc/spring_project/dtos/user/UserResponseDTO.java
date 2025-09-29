package ktc.spring_project.dtos.user;

import java.sql.Timestamp;

/**
 * DTO for User response data
 * Used when returning user information (excludes sensitive data like password)
 */
public class UserResponseDTO {
    
    private Long id;
    private String fullName;
    private String email;
    private String phone;
    private String address;
    private Boolean isActive;
    private String notes;
    
    // Role information
    private Long roleId;
    private String roleName;
    private String roleDescription;
    
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Constructors
    public UserResponseDTO() {}
    
    public UserResponseDTO(Long id, String fullName, String email, String roleName, Boolean isActive) {
        this.id = id;
        this.fullName = fullName;
        this.email = email;
        this.roleName = roleName;
        this.isActive = isActive;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getFullName() { return fullName; }
    public void setFullName(String fullName) { this.fullName = fullName; }
    
    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }
    
    public String getPhone() { return phone; }
    public void setPhone(String phone) { this.phone = phone; }
    
    public String getAddress() { return address; }
    public void setAddress(String address) { this.address = address; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Long getRoleId() { return roleId; }
    public void setRoleId(Long roleId) { this.roleId = roleId; }
    
    public String getRoleName() { return roleName; }
    public void setRoleName(String roleName) { this.roleName = roleName; }
    
    public String getRoleDescription() { return roleDescription; }
    public void setRoleDescription(String roleDescription) { this.roleDescription = roleDescription; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    // Utility methods
    public String getDisplayName() {
        return fullName != null ? fullName : email;
    }
    
    public boolean isActiveUser() {
        return Boolean.TRUE.equals(isActive);
    }
}
