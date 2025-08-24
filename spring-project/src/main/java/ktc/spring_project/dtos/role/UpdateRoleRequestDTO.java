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
    
    public boolean isFleetManagerRole() {
        return roleName != null && roleName.toLowerCase().contains("fleet_manager");
    }
    
    public boolean isOperationsManagerRole() {
        return roleName != null && roleName.toLowerCase().contains("operations_manager");
    }
    
    public boolean isCustomerRole() {
        return roleName != null && roleName.toLowerCase().contains("customer");
    }
    
    public boolean isSystemRole() {
        return isAdminRole() || isDriverRole() || isDispatcherRole() || 
               isFleetManagerRole() || isOperationsManagerRole();
    }
    
    public boolean isExternalRole() {
        return isCustomerRole();
    }
    
    public String getDefaultPermissions() {
        if (roleName == null) return null;
        
        if (isAdminRole()) {
            return "{\"users\": [\"create\", \"read\", \"update\", \"delete\"], \"system\": [\"configure\", \"logs\", \"reports\"], \"data\": [\"manage\"]}";
        } else if (isDispatcherRole()) {
            return "{\"orders\": [\"create\", \"read\", \"update\"], \"assignments\": [\"create\", \"update\"], \"tracking\": [\"read\", \"update\"]}";
        } else if (isDriverRole()) {
            return "{\"orders\": [\"read\", \"update_status\"], \"deliveries\": [\"update\", \"proof_upload\"], \"mobile\": [\"access\"]}";
        } else if (isFleetManagerRole()) {
            return "{\"vehicles\": [\"create\", \"read\", \"update\", \"delete\"], \"maintenance\": [\"schedule\", \"track\"], \"inspections\": [\"manage\"]}";
        } else if (isOperationsManagerRole()) {
            return "{\"dashboard\": [\"view_all\"], \"reports\": [\"generate\", \"export\"], \"analytics\": [\"view\", \"analyze\"]}";
        } else if (isCustomerRole()) {
            return "{\"orders\": [\"create\", \"read\"], \"tracking\": [\"read\"], \"profile\": [\"read\", \"update\"], \"invoices\": [\"view\"]}";
        }
        return null;
    }
    
    public String getRoleTypeDescription() {
        if (roleName == null) return null;
        
        if (isAdminRole()) return "Administrator - Người có toàn quyền cấu hình và giám sát hệ thống";
        if (isDriverRole()) return "Driver - Người thực hiện vận chuyển và cập nhật trạng thái đơn hàng";
        if (isDispatcherRole()) return "Dispatcher - Người lên kế hoạch và điều hành các chuyến giao hàng";
        if (isFleetManagerRole()) return "Fleet Manager - Người chịu trách nhiệm quản lý và bảo trì phương tiện";
        if (isOperationsManagerRole()) return "Operations Manager - Người giám sát toàn hệ thống, tối ưu hiệu suất vận hành";
        if (isCustomerRole()) return "Customer (B2B) - Khách hàng doanh nghiệp có thể đặt yêu cầu vận chuyển";
        return "Custom Role - Vai trò tùy chỉnh";
    }
    
    public boolean isCustomRole() {
        return !isSystemRole();
    }
}