package ktc.spring_project.dtos.role;

/**
 * DTO for creating a new role
 */
public class CreateRoleRequestDTO {
    
    private String roleName;
    private String permission;
    private String description;
    private Boolean isActive = true;
    
    // Constructors
    public CreateRoleRequestDTO() {}
    
    public CreateRoleRequestDTO(String roleName, String description) {
        this.roleName = roleName;
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
    public boolean isValid() {
        return roleName != null && !roleName.trim().isEmpty() &&
               description != null && !description.trim().isEmpty();
    }
    
    public boolean hasPermissions() {
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
        return isAdminRole() || isDispatcherRole() || isDriverRole() || 
               isFleetManagerRole() || isOperationsManagerRole();
    }
    
    public boolean isExternalRole() {
        return isCustomerRole();
    }
    
    public String getDefaultPermissions() {
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
        return "{}";
    }
    
}