package ktc.spring_project.enums;

public enum StatusType {
    VEHICLE("Vehicle"),
    ORDER("Order"),
    PAYMENT("Payment"),
    USER("User"),
    DELIVERY("Delivery"),
    INVOICE("Invoice"); // Thêm hỗ trợ cho hóa đơn thanh toán

    private final String displayName;

    StatusType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}