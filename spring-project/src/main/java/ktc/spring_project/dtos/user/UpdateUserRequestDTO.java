package ktc.spring_project.dtos.user;

import ktc.spring_project.enums.UserRole;
import jakarta.validation.constraints.*;

/**
 * DTO for User update requests
 * Used when updating existing user information
 */
public class UpdateUserRequestDTO {
    
    @Size(min = 2, max = 100, message = "Name must be between 2 and 100 characters")
    private String name;
    
    @Email(message = "Email should be valid")
    @Size(max = 150, message = "Email must not exceed 150 characters")
    private String email;
    
    private UserRole role;
    
    @Pattern(regexp = "^[+]?[0-9\\s\\-\\(\\)]{0,20}$", message = "Invalid phone number format")
    private String phone;
    
    @Size(max = 255, message = "Address must not exceed 255 characters")
    private String address;
    
    private Boolean isActive;
    
    // Constructors
    public UpdateUserRequestDTO() {}
    
    // Getters and Setters
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }
    
    public UserRole getRole() { return role; }
    public void setRole(UserRole role) { this.role = role; }
    
    public String getPhone() { return phone; }
    public void setPhone(String phone) { this.phone = phone; }
    
    public String getAddress() { return address; }
    public void setAddress(String address) { this.address = address; }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { this.isActive = isActive; }
}
