package ktc.spring_project.enums;

public enum TransactionType {
    IN("In"),
    OUT("Out"),
    ADJUSTMENT("Adjustment"),
    TRANSFER("Transfer");

    private final String displayName;

    TransactionType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
