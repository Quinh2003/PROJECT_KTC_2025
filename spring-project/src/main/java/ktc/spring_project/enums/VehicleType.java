package ktc.spring_project.enums;

public enum VehicleType {
    TRUCK("Truck"),
    VAN("Van"),
    MOTORCYCLE("Motorcycle"),
    CAR("Car");

    private final String displayName;

    VehicleType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}