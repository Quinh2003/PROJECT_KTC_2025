package ktc.spring_project.enums;

public enum ProductStatus {
    INACTIVE("Ngừng bán", 0),
    ACTIVE("Đang bán", 1);

    private final String displayName;
    private final int value;

    ProductStatus(String displayName, int value) {
        this.displayName = displayName;
        this.value = value;
    }

    public String getDisplayName() {
        return displayName;
    }

    public int getValue() {
        return value;
    }

    public static ProductStatus fromValue(int value) {
        for (ProductStatus status : values()) {
            if (status.value == value) return status;
        }
        throw new IllegalArgumentException("Unknown value: " + value);
    }
}