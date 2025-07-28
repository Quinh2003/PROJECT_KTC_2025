package java.ktc.spring_project.enums;

public enum VehicleStatus {
    AVAILABLE("Available"),
    IN_USE("In Use"),
    MAINTENANCE("Under Maintenance"),
    INACTIVE("Inactive"),
    OUT_OF_SERVICE("Out of Service");
    
    private final String displayName;
    
    VehicleStatus(String displayName) {
        this.displayName = displayName;
    }
    
    public String getDisplayName() {
        return displayName;
    }
}
