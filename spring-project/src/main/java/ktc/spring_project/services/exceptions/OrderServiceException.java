package ktc.spring_project.services.exceptions;

/**
 * Exception for Order service operations
 */
public class OrderServiceException extends ServiceException {
    
    public static final String ORDER_NOT_FOUND = "ORDER_NOT_FOUND";
    public static final String ORDER_NOT_CANCELLABLE = "ORDER_NOT_CANCELLABLE";
    public static final String ORDER_ALREADY_ASSIGNED = "ORDER_ALREADY_ASSIGNED";
    public static final String INVALID_ORDER_STATUS = "INVALID_ORDER_STATUS";
    public static final String INSUFFICIENT_STOCK = "INSUFFICIENT_STOCK";
    public static final String INVALID_ORDER_ITEMS = "INVALID_ORDER_ITEMS";
    
    public OrderServiceException(String message, String errorCode) {
        super(message, errorCode);
    }
    
    public OrderServiceException(String message, String errorCode, Throwable cause) {
        super(message, errorCode, cause);
    }
    
    // Factory methods for common exceptions
    public static OrderServiceException orderNotFound(Long orderId) {
        return new OrderServiceException("Order not found with ID: " + orderId, ORDER_NOT_FOUND);
    }
    
    public static OrderServiceException orderNotFound(String orderCode) {
        return new OrderServiceException("Order not found with code: " + orderCode, ORDER_NOT_FOUND);
    }
    
    public static OrderServiceException orderNotCancellable(Long orderId) {
        return new OrderServiceException("Order cannot be cancelled: " + orderId, ORDER_NOT_CANCELLABLE);
    }
    
    public static OrderServiceException orderAlreadyAssigned(Long orderId) {
        return new OrderServiceException("Order is already assigned: " + orderId, ORDER_ALREADY_ASSIGNED);
    }
    
    public static OrderServiceException invalidOrderStatus(String currentStatus, String newStatus) {
        return new OrderServiceException("Cannot change order status from " + currentStatus + " to " + newStatus, INVALID_ORDER_STATUS);
    }
    
    public static OrderServiceException insufficientStock(String productCode, Integer available, Integer required) {
        return new OrderServiceException("Insufficient stock for product " + productCode + ". Available: " + available + ", Required: " + required, INSUFFICIENT_STOCK);
    }
    
    public static OrderServiceException invalidOrderItems() {
        return new OrderServiceException("Order must have at least one item", INVALID_ORDER_ITEMS);
    }
}