package ktc.spring_project.enums;

public enum DeliveryStatus {
    PENDING("Chờ xử lý"),
    ASSIGNED("Đã phân công"),
    IN_TRANSIT("Đang vận chuyển"),
    OUT_FOR_DELIVERY("Đang giao hàng"),
    DELIVERED("Đã giao hàng"),
    FAILED("Giao hàng thất bại"),
    RETURNED("Đã trả hàng"),
    CANCELLED("Đã hủy");

    private final String description;

    DeliveryStatus(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
