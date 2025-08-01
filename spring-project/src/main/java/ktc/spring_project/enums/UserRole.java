package ktc.spring_project.enums;

/**
 * User role types based on business requirements
 * Maps to users.role_id through roles table
 */
public enum UserRole {
    CUSTOMER("Customer", "Can create orders and track deliveries"),
    DRIVER("Driver", "Can update delivery status and upload proofs"),
    MANAGER("Manager", "Can manage operations and view reports"),
    ADMIN("Admin", "Full system access and user management");
    
    private final String displayName;
    private final String description;
    
    UserRole(String displayName, String description) {
        this.displayName = displayName;
        this.description = description;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getDescription() {
        return description;
    }
}