package ktc.spring_project.enums;

public enum TransportMode {
    ROAD("Road"),
    AIR("Air"),
    SEA("Sea"),
    RAIL("Rail");

    private final String displayName;

    TransportMode(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}