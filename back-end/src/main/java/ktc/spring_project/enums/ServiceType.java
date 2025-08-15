package ktc.spring_project.enums;

public enum ServiceType {
    STANDARD("Standard"),
    EXPRESS("Express"),
    PRIORITY("Priority");

    private final String displayName;

    ServiceType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
