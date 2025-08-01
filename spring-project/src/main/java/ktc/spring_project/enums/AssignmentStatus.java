package ktc.spring_project.enums;

/**
 * Assignment status for dispatch assignments
 * Used with status table where type='ASSIGNMENT'
 */
public enum AssignmentStatus {
    ASSIGNED("Assigned", "Driver has been assigned to delivery", false),
    IN_PROGRESS("In Progress", "Delivery is currently in progress", false),
    COMPLETED("Completed", "Assignment successfully completed", true),
    CANCELLED("Cancelled", "Assignment has been cancelled", true);
    
    private final String displayName;
    private final String description;
    private final boolean isFinalState;
    
    AssignmentStatus(String displayName, String description, boolean isFinalState) {
        this.displayName = displayName;
        this.description = description;
        this.isFinalState = isFinalState;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getDescription() {
        return description;
    }
    
    public boolean isFinalState() {
        return isFinalState;
    }
    
    /**
     * Check if assignment is active (can be worked on)
     */
    public boolean isActive() {
        return this == ASSIGNED || this == IN_PROGRESS;
    }
}