package ktc.spring_project.services.exceptions;

/**
 * Exception for User service operations
 */
public class UserServiceException extends ServiceException {
    
    public static final String USER_NOT_FOUND = "USER_NOT_FOUND";
    public static final String EMAIL_ALREADY_EXISTS = "EMAIL_ALREADY_EXISTS";
    public static final String INVALID_CREDENTIALS = "INVALID_CREDENTIALS";
    public static final String USER_INACTIVE = "USER_INACTIVE";
    public static final String INVALID_ROLE = "INVALID_ROLE";
    
    public UserServiceException(String message, String errorCode) {
        super(message, errorCode);
    }
    
    public UserServiceException(String message, String errorCode, Throwable cause) {
        super(message, errorCode, cause);
    }
    
    // Factory methods for common exceptions
    public static UserServiceException userNotFound(Long userId) {
        return new UserServiceException("User not found with ID: " + userId, USER_NOT_FOUND);
    }
    
    public static UserServiceException userNotFound(String email) {
        return new UserServiceException("User not found with email: " + email, USER_NOT_FOUND);
    }
    
    public static UserServiceException emailAlreadyExists(String email) {
        return new UserServiceException("Email already exists: " + email, EMAIL_ALREADY_EXISTS);
    }
    
    public static UserServiceException invalidCredentials() {
        return new UserServiceException("Invalid email or password", INVALID_CREDENTIALS);
    }
    
    public static UserServiceException userInactive(String email) {
        return new UserServiceException("User account is inactive: " + email, USER_INACTIVE);
    }
    
    public static UserServiceException invalidRole(Long roleId) {
        return new UserServiceException("Invalid role ID: " + roleId, INVALID_ROLE);
    }
}