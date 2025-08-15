package ktc.spring_project.enums;

public enum StatusType {
    VEHICLE("Vehicle"),
    ORDER("Order"),
    PAYMENT("Payment"),
    USER("User");

    private final String displayName;

    StatusType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
