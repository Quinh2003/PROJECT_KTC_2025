package ktc.spring_project.enums;

/**
 * Token types for authentication system
 * Used in tokens table.token_type
 */
public enum TokenType {
    PASSWORD_RESET("Password Reset", "Token for password reset functionality"),
    REFRESH("Refresh Token", "Token for refreshing access tokens");
    
    private final String displayName;
    private final String description;
    
    TokenType(String displayName, String description) {
        this.displayName = displayName;
        this.description = description;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getDescription() {
        return description;
    }
}