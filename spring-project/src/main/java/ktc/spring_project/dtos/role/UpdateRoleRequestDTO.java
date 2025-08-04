package ktc.spring_project.dtos.role;

/**
 * DTO for updating an existing role
 */
public class UpdateRoleRequestDTO {
    
    private String roleName;
    private String permission;
    private String description;
    private Boolean isActive;
    
    // Constructors
    public UpdateRoleRequestDTO() {}
    
    public UpdateRoleRequestDTO(String description) {
        this.description = description;
    }
    
    // Getters and Setters
    public String getRoleName() { return roleName; }
    public void setRoleName(String roleName) { this.roleName = roleName; }
    
    public String getPermission() { return permission; }
    public void setPermission(String permission) { this.permission = permission; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
    
    // Validation methods
    public boolean hasValidRoleName() {
        return roleName != null && !roleName.trim().isEmpty();
    }
    
    public boolean hasValidDescription() {
        return description != null && !description.trim().isEmpty();
    }
    
    public boolean hasPermissionUpdate() {
        return permission != null;
    }
    
    public boolean hasRoleNameUpdate() {
        return roleName != null;
    }
    
    public boolean hasDescriptionUpdate() {
        return description != null;
    }
    
    public boolean hasStatusUpdate() {
        return isActive != null;
    }
    
    public boolean isActivating() {
        return Boolean.TRUE.equals(isActive);
    }
    
    public boolean isDeactivating() {
        return Boolean.FALSE.equals(isActive);
    }
    
    public boolean isClearingPermissions() {
        return permission != null && permission.trim().isEmpty();
    }
    
    public boolean isUpdatingPermissions() {
        return permission != null && !permission.trim().isEmpty();
    }
    
    public String getRoleDisplayName() {
        return roleName != null ? roleName.replace("_", " ").toUpperCase() : null;
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
    
    public boolean isSystemRole() {
        return isAdminRole() || isDriverRole() || isDispatcherRole() || isManagerRole();
    }
    
    public boolean isCustomRole() {
        return !isSystemRole();
    }
}