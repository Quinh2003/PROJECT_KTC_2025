package ktc.spring_project.enums;

/**
 * Vehicle status for fleet management
 * Used with status table where type='VEHICLE'
 */
public enum VehicleStatus {
    AVAILABLE("Available", "Vehicle is available for assignment", true),
    IN_USE("In Use", "Vehicle is currently assigned to delivery", false),
    MAINTENANCE("Maintenance", "Vehicle is under maintenance", false),
    RETIRED("Retired", "Vehicle is retired from service", false);
    
    private final String displayName;
    private final String description;
    private final boolean canBeAssigned; // Whether vehicle can be assigned to orders
    
    VehicleStatus(String displayName, String description, boolean canBeAssigned) {
        this.displayName = displayName;
        this.description = description;
        this.canBeAssigned = canBeAssigned;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getDescription() {
        return description;
    }
    
    public boolean canBeAssigned() {
        return canBeAssigned;
    }
    
    /**
     * Check if vehicle is operational (not retired)
     */
    public boolean isOperational() {
        return this != RETIRED;
    }
}