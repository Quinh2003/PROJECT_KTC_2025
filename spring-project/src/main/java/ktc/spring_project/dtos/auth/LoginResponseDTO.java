package ktc.spring_project.dtos.auth;


import ktc.spring_project.dtos.user.UserResponseDTO;
import java.time.LocalDateTime;

/**
 * DTO for login response (includes JWT token and user info)
 */
public class LoginResponseDTO {
    
    private String accessToken;
    private String tokenType = "Bearer";
    private long expiresIn; // in seconds
    private UserResponseDTO user;
    private LocalDateTime loginTime;
    
    // Constructors
    public LoginResponseDTO() {
        this.loginTime = LocalDateTime.now();
    }
    
    public LoginResponseDTO(String accessToken, long expiresIn, UserResponseDTO user) {
        this();
        this.accessToken = accessToken;
        this.expiresIn = expiresIn;
        this.user = user;
    }
    
    // Getters and Setters
    public String getAccessToken() { return accessToken; }
    public void setAccessToken(String accessToken) { this.accessToken = accessToken; }
    
    public String getTokenType() { return tokenType; }
    public void setTokenType(String tokenType) { this.tokenType = tokenType; }
    
    public long getExpiresIn() { return expiresIn; }
    public void setExpiresIn(long expiresIn) { this.expiresIn = expiresIn; }
    
    public UserResponseDTO getUser() { return user; }
    public void setUser(UserResponseDTO user) { this.user = user; }
    
    public LocalDateTime getLoginTime() { return loginTime; }
    public void setLoginTime(LocalDateTime loginTime) { this.loginTime = loginTime; }
    
    // Utility methods
    public String getFullToken() {
        return tokenType + " " + accessToken;
    }
    
    public boolean isDriver() {
        return user != null && "DRIVER".equals(user.getRoleName());
    }
    
    public boolean isAdmin() {
        return user != null && "ADMIN".equals(user.getRoleName());
    }
    
    public boolean isDispatcher() {
        return user != null && "DISPATCHER".equals(user.getRoleName());
    }
}
