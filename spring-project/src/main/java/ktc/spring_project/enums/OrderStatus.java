package ktc.spring_project.enums;

public enum OrderStatus {
    PENDING("Pending"),
    ASSIGNED("Assigned"),
    IN_PROGRESS("In Progress"),
    PICKED_UP("Picked Up"),
    ON_DELIVERY("On Delivery"),
    DELIVERED("Delivered"),
    FAILED("Failed"),
    CANCELLED("Cancelled");
    
    private final String displayName;
    
    OrderStatus(String displayName) {
        this.displayName = displayName;
    }
    
    public String getDisplayName() {
        return displayName;
    }
}
