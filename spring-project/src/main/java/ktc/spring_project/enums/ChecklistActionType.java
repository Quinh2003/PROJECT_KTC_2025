package ktc.spring_project.enums;

/**
 * Enum để định nghĩa các hành động cụ thể cho checklist system
 * Mỗi role sẽ có các actions riêng để track progress
 */
public enum ChecklistActionType {
    
    // ============ CUSTOMER CHECKLIST ACTIONS ============
    CUSTOMER_ACCOUNT_CREATED("CUSTOMER_ACCOUNT_CREATED", "Tạo tài khoản khách hàng", "CUSTOMER"),
    CUSTOMER_FIRST_ORDER_CREATED("CUSTOMER_FIRST_ORDER_CREATED", "Tạo đơn hàng đầu tiên", "CUSTOMER"),
    CUSTOMER_PAYMENT_COMPLETED("CUSTOMER_PAYMENT_COMPLETED", "Hoàn thành thanh toán", "CUSTOMER"),
    CUSTOMER_ORDER_RECEIVED("CUSTOMER_ORDER_RECEIVED", "Nhận hàng thành công", "CUSTOMER"),
    CUSTOMER_SERVICE_REVIEWED("CUSTOMER_SERVICE_REVIEWED", "Đánh giá dịch vụ", "CUSTOMER"),
    
    // ============ DISPATCHER CHECKLIST ACTIONS ============
    DISPATCHER_SYSTEM_LOGIN("DISPATCHER_SYSTEM_LOGIN", "Đăng nhập hệ thống", "DISPATCHER"),
    DISPATCHER_FIRST_ORDER_PROCESSED("DISPATCHER_FIRST_ORDER_PROCESSED", "Xử lý đơn hàng đầu tiên", "DISPATCHER"),
    DISPATCHER_DRIVER_ASSIGNED("DISPATCHER_DRIVER_ASSIGNED", "Phân công tài xế và gán xe", "DISPATCHER"),
    DISPATCHER_ROUTE_OPTIMIZED("DISPATCHER_ROUTE_OPTIMIZED", "Tối ưu hóa tuyến đường", "DISPATCHER"),
    DISPATCHER_DELIVERY_TRACKED("DISPATCHER_DELIVERY_TRACKED", "Theo dõi giao hàng", "DISPATCHER"),
    
    // ============ DRIVER CHECKLIST ACTIONS ============
    DRIVER_MOBILE_LOGIN("DRIVER_MOBILE_LOGIN", "Đăng nhập ứng dụng mobile", "DRIVER"),
    DRIVER_FIRST_DELIVERY_RECEIVED("DRIVER_FIRST_DELIVERY_RECEIVED", "Nhận đơn giao hàng đầu tiên", "DRIVER"),
    DRIVER_LOCATION_UPDATED("DRIVER_LOCATION_UPDATED", "Cập nhật vị trí GPS", "DRIVER"),
    DRIVER_DELIVERY_COMPLETED("DRIVER_DELIVERY_COMPLETED", "Hoàn thành giao hàng", "DRIVER"),
    DRIVER_PROOF_UPLOADED("DRIVER_PROOF_UPLOADED", "Tải ảnh chứng minh", "DRIVER");
    
    private final String actionCode;
    private final String description;
    private final String roleType;
    
    ChecklistActionType(String actionCode, String description, String roleType) {
        this.actionCode = actionCode;
        this.description = description;
        this.roleType = roleType;
    }
    
    public String getActionCode() {
        return actionCode;
    }
    
    public String getDescription() {
        return description;
    }
    
    public String getRoleType() {
        return roleType;
    }
    
    // Helper methods
    public boolean isCustomerAction() {
        return "CUSTOMER".equals(roleType);
    }
    
    public boolean isDispatcherAction() {
        return "DISPATCHER".equals(roleType);
    }
    
    public boolean isDriverAction() {
        return "DRIVER".equals(roleType);
    }
    
    // Get actions by role
    public static ChecklistActionType[] getCustomerActions() {
        return new ChecklistActionType[]{
            CUSTOMER_ACCOUNT_CREATED,
            CUSTOMER_FIRST_ORDER_CREATED,
            CUSTOMER_PAYMENT_COMPLETED,
            CUSTOMER_ORDER_RECEIVED,
            CUSTOMER_SERVICE_REVIEWED
        };
    }
    
    public static ChecklistActionType[] getDispatcherActions() {
        return new ChecklistActionType[]{
            DISPATCHER_SYSTEM_LOGIN,
            DISPATCHER_FIRST_ORDER_PROCESSED,
            DISPATCHER_DRIVER_ASSIGNED,
            DISPATCHER_ROUTE_OPTIMIZED,
            DISPATCHER_DELIVERY_TRACKED
        };
    }
    
    public static ChecklistActionType[] getDriverActions() {
        return new ChecklistActionType[]{
            DRIVER_MOBILE_LOGIN,
            DRIVER_FIRST_DELIVERY_RECEIVED,
            DRIVER_LOCATION_UPDATED,
            DRIVER_DELIVERY_COMPLETED,
            DRIVER_PROOF_UPLOADED
        };
    }
    
    // Find action by code
    public static ChecklistActionType fromActionCode(String actionCode) {
        for (ChecklistActionType action : values()) {
            if (action.getActionCode().equals(actionCode)) {
                return action;
            }
        }
        return null;
    }
}
