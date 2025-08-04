package ktc.spring_project.enums;

public enum AddressType {
    DELIVERY("Delivery"),
    PICKUP("Pickup"),
    RETURN("Return");

    private final String displayName;

    AddressType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}