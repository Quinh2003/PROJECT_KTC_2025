package java.ktc.spring_project.enums;

public enum TrackingStatus {
    ORDER_CREATED("Order Created"),
    ASSIGNED_TO_DRIVER("Assigned to Driver"),
    DRIVER_ACCEPTED("Driver Accepted"),
    PICKUP_STARTED("Pickup Started"),
    PICKED_UP("Picked Up"),
    ON_THE_WAY("On the Way"),
    ARRIVED_AT_DESTINATION("Arrived at Destination"),
    DELIVERY_ATTEMPTED("Delivery Attempted"),
    DELIVERED("Delivered"),
    FAILED_DELIVERY("Failed Delivery"),
    RETURNED("Returned");
    
    private final String displayName;
    
    TrackingStatus(String displayName) {
        this.displayName = displayName;
    }
    
    public String getDisplayName() {
        return displayName;
    }
}
