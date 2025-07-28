package java.ktc.spring_project.enums;

public enum UserRole {
    ADMIN("Admin"),
    DISPATCHER("Dispatcher"), 
    DRIVER("Driver"),
    FLEET_MANAGER("Fleet Manager"),
    OPERATIONS_MANAGER("Operations Manager");
    
    private final String displayName;
    
    UserRole(String displayName) {
        this.displayName = displayName;
    }
    
    public String getDisplayName() {
        return displayName;
    }
}
