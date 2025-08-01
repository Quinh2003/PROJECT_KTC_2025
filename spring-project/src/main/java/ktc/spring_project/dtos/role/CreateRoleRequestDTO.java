package ktc.spring_project.dtos.role;

import jakarta.validation.constraints.*;
import java.util.List;

/**
 * DTO for Role creation requests
 * Used when creating a new role with permissions
 */
public class CreateRoleRequestDTO {
    
    @NotBlank(message = "Role name is required")
    @Size(min = 2, max = 50, message = "Role name must be between 2 and 50 characters")
    @Pattern(regexp = "^[A-Z_]+$", message = "Role name must be uppercase with underscores only (e.g., CUSTOMER, DRIVER)")
    private String roleName;
    
    @NotNull(message = "Permissions are required")
    @Size(min = 1, message = "At least one permission is required")
    private List<String> permissions;
    
    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;
    
    // Constructors
    public CreateRoleRequestDTO() {}
    
    public CreateRoleRequestDTO(String roleName, List<String> permissions, String description) {
        this.roleName = roleName;
        this.permissions = permissions;
        this.description = description;
    }
    
    // Getters and Setters
    public String getRoleName() {
        return roleName;
    }
    
    public void setRoleName(String roleName) {
        this.roleName = roleName;
    }
    
    public List<String> getPermissions() {
        return permissions;
    }
    
    public void setPermissions(List<String> permissions) {
        this.permissions = permissions;
    }
    
    public String getDescription() {
        return description;
    }
    
    public void setDescription(String description) {
        this.description = description;
    }
    
    @Override
    public String toString() {
        return "CreateRoleRequestDTO{" +
                "roleName='" + roleName + '\'' +
                ", permissions=" + permissions +
                ", description='" + description + '\'' +
                '}';
    }
}