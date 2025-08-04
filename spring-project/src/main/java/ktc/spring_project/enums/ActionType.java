package ktc.spring_project.enums;

public enum ActionType {
    CREATE("Tạo mới"),
    UPDATE("Cập nhật"),
    DELETE("Xóa"),
    LOGIN("Đăng nhập"),
    LOGOUT("Đăng xuất"),
    VIEW("Xem"),
    EXPORT("Xuất dữ liệu"),
    IMPORT("Nhập dữ liệu");

    private final String description;

    ActionType(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
