package ktc.spring_project.enums;

public enum ServiceType {
    STANDARD("Standard", 1.0),
    FIRST_CLASS("First Class", 1.3),
    SECOND_CLASS("Second Class", 0.8),
    EXPRESS("Express", 1.8),
    PRIORITY("Priority", 2.0);

    private final String displayName;
    private final double multiplier;

    ServiceType(String displayName, double multiplier) {
        this.displayName = displayName;
        this.multiplier = multiplier;
    }

    public String getDisplayName() {
        return displayName;
    }

    public double getMultiplier() {
        return multiplier;
    }
}