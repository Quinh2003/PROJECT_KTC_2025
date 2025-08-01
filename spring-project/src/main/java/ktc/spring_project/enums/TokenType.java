package ktc.spring_project.enums;

public enum TokenType {
    PASSWORD_RESET("Password Reset"),
    REFRESH("Refresh");

    private final String displayName;

    TokenType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
