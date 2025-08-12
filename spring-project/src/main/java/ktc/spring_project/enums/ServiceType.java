package ktc.spring_project.enums;

public enum ServiceType {
    STANDARD("Standard"),
    FIRST_CLASS("First Class"),
    SECOND_CLASS("Second Class"),
    SAME_DAY("Same Day"),
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