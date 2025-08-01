package ktc.spring_project.enums;

/**
 * Delivery tracking status for real-time updates
 * Used with status table where type='TRACKING'
 */
public enum TrackingStatus {
    EN_ROUTE("En Route", "Vehicle is traveling to destination"),
    ARRIVED("Arrived", "Vehicle has arrived at location"),
    DEPARTED("Departed", "Vehicle has left the location"),
    COMPLETED("Completed", "Delivery has been completed"),
    DELAYED("Delayed", "Delivery is experiencing delays"),
    FAILED("Failed", "Delivery attempt failed");
    
    private final String displayName;
    private final String description;
    
    TrackingStatus(String displayName, String description) {
        this.displayName = displayName;
        this.description = description;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getDescription() {
        return description;
    }
    
    /**
     * Check if tracking status indicates active delivery
     */
    public boolean isActiveDelivery() {
        return this == EN_ROUTE || this == ARRIVED || this == DEPARTED;
    }
    
    /**
     * Check if tracking status is final
     */
    public boolean isFinalStatus() {
        return this == COMPLETED || this == FAILED;
    }
}