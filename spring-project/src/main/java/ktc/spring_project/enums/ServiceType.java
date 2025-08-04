package ktc.spring_project.enums;

public enum ServiceType {
    STANDARD("Tiêu chuẩn"),
    FAST("Nhanh"),
    PRIORITY("Ưu tiên"),
    EXPRESS("Hỏa tốc");

    private final String description;

    ServiceType(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
