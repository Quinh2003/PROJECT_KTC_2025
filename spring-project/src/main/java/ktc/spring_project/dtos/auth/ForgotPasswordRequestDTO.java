package ktc.spring_project.dtos.auth;

import jakarta.validation.constraints.*;

/**
 * DTO for password reset requests
 */
public class ForgotPasswordRequestDTO {
    
    @NotBlank(message = "Email is required")
    @Email(message = "Email should be valid")
    private String email;
    
    // Constructors
    public ForgotPasswordRequestDTO() {}
    
    public ForgotPasswordRequestDTO(String email) {
        this.email = email;
    }
    
    // Getters and Setters
    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }
}
