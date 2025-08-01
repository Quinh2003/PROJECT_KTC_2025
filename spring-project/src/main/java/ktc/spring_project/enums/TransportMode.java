package ktc.spring_project.enums;

public enum TransportMode {
    STANDARD("Standard"),
    EXPRESS("Express"),
    PRIORITY("Priority");

    private final String displayName;

    TransportMode(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
