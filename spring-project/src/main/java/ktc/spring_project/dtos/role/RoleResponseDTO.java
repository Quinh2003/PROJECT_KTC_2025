package ktc.spring_project.dtos.role;

import java.sql.Timestamp;

/**
 * DTO for role response data
 */
public class RoleResponseDTO {
    
    private Long id;
    private String roleName;
    private String permission;
    private String description;
    private Boolean isActive;
    private Timestamp createdAt;
    private Timestamp updatedAt;
    
    // Statistics
    private Integer userCount;
    private Integer activeUserCount;
    
    // Constructors
    public RoleResponseDTO() {}
    
    public RoleResponseDTO(Long id, String roleName, String description, Boolean isActive) {
        this.id = id;
        this.roleName = roleName;
        this.description = description;
        this.isActive = isActive;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getRoleName() { return roleName; }
    public void setRoleName(String roleName) { this.roleName = roleName; }
    
    public String getPermission() { return permission; }
    public void setPermission(String permission) { this.permission = permission; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Timestamp getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Timestamp updatedAt) { this.updatedAt = updatedAt; }
    
    public Integer getUserCount() { return userCount; }
    public void setUserCount(Integer userCount) { this.userCount = userCount; }
    
    public Integer getActiveUserCount() { return activeUserCount; }
    public void setActiveUserCount(Integer activeUserCount) { this.activeUserCount = activeUserCount; }
    
    // Utility methods
    public String getRoleDisplayName() {
        return roleName != null ? roleName.replace("_", " ").toUpperCase() : null;
    }
    
    public boolean isActiveRole() {
        return Boolean.TRUE.equals(isActive);
    }
    
    public boolean hasPermissions() {
        return permission != null && !permission.trim().isEmpty();
    }
    
    public boolean hasUsers() {
        return userCount != null && userCount > 0;
    }
    
    public boolean hasActiveUsers() {
        return activeUserCount != null && activeUserCount > 0;
    }
    
    public String getStatusDisplay() {
        return isActiveRole() ? "Active" : "Inactive";
    }
    
    public boolean isAdminRole() {
        return roleName != null && roleName.toLowerCase().contains("admin");
    }
    
    public boolean isDriverRole() {
        return roleName != null && roleName.toLowerCase().contains("driver");
    }
    
    public boolean isDispatcherRole() {
        return roleName != null && roleName.toLowerCase().contains("dispatcher");
    }
    
    public boolean isManagerRole() {
        return roleName != null && roleName.toLowerCase().contains("manager");
    }
    
    public boolean isViewerRole() {
        return roleName != null && roleName.toLowerCase().contains("viewer");
    }
    
    public String getUserSummary() {
        if (userCount == null) {
            return "No user data";
        }
        return String.format("%d total (%d active)", userCount, activeUserCount != null ? activeUserCount : 0);
    }
    
    public String getRoleType() {
        if (isAdminRole()) return "Administrator";
        if (isDriverRole()) return "Driver";
        if (isDispatcherRole()) return "Dispatcher";
        if (isManagerRole()) return "Manager";
        if (isViewerRole()) return "Viewer";
        return "Custom Role";
    }
    
    public String getPermissionSummary() {
        if (!hasPermissions()) {
            return "No permissions defined";
        }
        // This is a simplified permission summary
        // In practice, you'd parse the JSON and create a meaningful summary
        return "Custom permissions defined";
    }
}