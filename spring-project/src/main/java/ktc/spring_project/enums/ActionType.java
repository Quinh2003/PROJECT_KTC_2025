package ktc.spring_project.enums;

public enum ActionType {
    // Basic CRUD operations
    CREATE("Create"),
    UPDATE("Update"),
    DELETE("Delete"),
    
    // Authentication actions
    LOGIN("Login"),
    LOGOUT("Logout"),
    
    // Customer actions for checklist
    CUSTOMER_REGISTER("Customer Register"),
    CUSTOMER_CREATE_ORDER("Customer Create Order"),
    CUSTOMER_PAYMENT_COMPLETE("Customer Payment Complete"),
    CUSTOMER_RECEIVE_ORDER("Customer Receive Order"),
    CUSTOMER_REVIEW_SERVICE("Customer Review Service"),
    
    // Dispatcher actions for checklist
    DISPATCHER_LOGIN("Dispatcher Login"),
    DISPATCHER_PROCESS_ORDER("Dispatcher Process Order"),
    DISPATCHER_ASSIGN_DRIVER("Dispatcher Assign Driver"),
    DISPATCHER_OPTIMIZE_ROUTE("Dispatcher Optimize Route"),
    DISPATCHER_TRACK_DELIVERY("Dispatcher Track Delivery"),
    
    // Driver actions for checklist
    DRIVER_LOGIN("Driver Login"),
    DRIVER_RECEIVE_DELIVERY("Driver Receive Delivery"),
    DRIVER_UPDATE_LOCATION("Driver Update Location"),
    DRIVER_COMPLETE_DELIVERY("Driver Complete Delivery"),
    DRIVER_UPLOAD_PROOF("Driver Upload Proof"),
    
    // Order management actions
    ORDER_CREATED("Order Created"),
    ORDER_PROCESSED("Order Processed"),
    ORDER_ASSIGNED("Order Assigned"),
    ORDER_IN_TRANSIT("Order In Transit"),
    ORDER_DELIVERED("Order Delivered"),
    ORDER_CANCELLED("Order Cancelled"),
    
    // Delivery actions
    DELIVERY_SCHEDULED("Delivery Scheduled"),
    DELIVERY_STARTED("Delivery Started"),
    DELIVERY_LOCATION_UPDATED("Delivery Location Updated"),
    DELIVERY_COMPLETED("Delivery Completed"),
    DELIVERY_FAILED("Delivery Failed"),
    
    // Payment actions
    PAYMENT_INITIATED("Payment Initiated"),
    PAYMENT_COMPLETED("Payment Completed"),
    PAYMENT_FAILED("Payment Failed"),
    
    // Route optimization
    ROUTE_OPTIMIZED("Route Optimized"),
    ROUTE_ASSIGNED("Route Assigned"),
    
    // Vehicle and driver management
    VEHICLE_ASSIGNED("Vehicle Assigned"),
    DRIVER_ASSIGNED("Driver Assigned"),
    
    // System actions
    SYSTEM_NOTIFICATION("System Notification"),
    SYSTEM_ERROR("System Error");

    private final String displayName;

    ActionType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
    
    // Helper methods to check action categories
    public boolean isCustomerAction() {
        return this.name().startsWith("CUSTOMER_");
    }
    
    public boolean isDispatcherAction() {
        return this.name().startsWith("DISPATCHER_");
    }
    
    public boolean isDriverAction() {
        return this.name().startsWith("DRIVER_");
    }
    
    public boolean isChecklistAction() {
        return isCustomerAction() || isDispatcherAction() || isDriverAction();
    }
}
