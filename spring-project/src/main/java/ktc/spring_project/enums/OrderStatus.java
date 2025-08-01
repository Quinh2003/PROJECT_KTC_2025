package ktc.spring_project.enums;

/**
 * Order status lifecycle for order management
 * Used with status table where type='ORDER'
 */
public enum OrderStatus {
    PENDING("Pending", "Order created, waiting for processing", false),
    READY("Ready", "Order processed and ready for pickup", false),
    ON_DELIVERY("On Delivery", "Order is being delivered", true),
    DELIVERED("Delivered", "Order successfully delivered", true),
    CANCELLED("Cancelled", "Order has been cancelled", true);
    
    private final String displayName;
    private final String description;
    private final boolean isFinalState; // Whether this is a terminal state
    
    OrderStatus(String displayName, String description, boolean isFinalState) {
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
     * Check if order can be cancelled from current status
     */
    public boolean canBeCancelled() {
        return this == PENDING || this == READY;
    }
    
    /**
     * Get next possible statuses from current state
     */
    public OrderStatus[] getNextPossibleStatuses() {
        switch (this) {
            case PENDING:
                return new OrderStatus[]{READY, CANCELLED};
            case READY:
                return new OrderStatus[]{ON_DELIVERY, CANCELLED};
            case ON_DELIVERY:
                return new OrderStatus[]{DELIVERED};
            default:
                return new OrderStatus[]{};
        }
    }
}