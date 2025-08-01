package ktc.spring_project.services.interfaces;

import ktc.spring_project.dtos.user.CreateUserRequestDTO;
import ktc.spring_project.dtos.user.UpdateUserRequestDTO;
import ktc.spring_project.dtos.user.UserResponseDTO;
import ktc.spring_project.dtos.common.PagedResponse;
import ktc.spring_project.entities.User;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;

/**
 * Service interface for User management operations
 * Handles user CRUD, authentication, and role management
 */
public interface UserService {
    
    // ===== CRUD OPERATIONS =====
    /**
     * Create a new user with role assignment
     * @param createUserDTO User creation data
     * @return Created user response
     */
    UserResponseDTO createUser(CreateUserRequestDTO createUserDTO);
    
    /**
     * Update an existing user
     * @param id User ID
     * @param updateUserDTO Update data
     * @return Updated user response
     */
    UserResponseDTO updateUser(Long id, UpdateUserRequestDTO updateUserDTO);
    
    /**
     * Get user by ID
     * @param id User ID
     * @return User response or empty if not found
     */
    Optional<UserResponseDTO> getUserById(Long id);
    
    /**
     * Get user entity by ID (for internal service use)
     * @param id User ID
     * @return User entity or empty if not found
     */
    Optional<User> getUserEntityById(Long id);
    
    /**
     * Get user by email
     * @param email User email
     * @return User response or empty if not found
     */
    Optional<UserResponseDTO> getUserByEmail(String email);
    
    /**
     * Get all users with pagination
     * @param pageable Pagination parameters
     * @return Paged user responses
     */
    PagedResponse<UserResponseDTO> getAllUsers(Pageable pageable);
    
    /**
     * Delete user (soft delete by setting is_active = false)
     * @param id User ID
     */
    void deleteUser(Long id);
    
    // ===== ROLE-BASED QUERIES =====
    /**
     * Get users by role ID
     * @param roleId Role ID
     * @return List of users with the specified role
     */
    List<UserResponseDTO> getUsersByRole(Long roleId);
    
    /**
     * Get all drivers (users with DRIVER role)
     * @return List of driver users
     */
    List<UserResponseDTO> getAllDrivers();
    
    /**
     * Get available drivers (active drivers not currently assigned)
     * @return List of available drivers
     */
    List<UserResponseDTO> getAvailableDrivers();
    
    /**
     * Get all customers (users with CUSTOMER role)
     * @return List of customer users
     */
    List<UserResponseDTO> getAllCustomers();
    
    // ===== AUTHENTICATION SUPPORT =====
    /**
     * Authenticate user by email and password
     * @param email User email
     * @param password Plain text password
     * @return User entity if authentication successful, empty otherwise
     */
    Optional<User> authenticateUser(String email, String password);
    
    /**
     * Check if email is already taken
     * @param email Email to check
     * @return true if email exists, false otherwise
     */
    boolean isEmailTaken(String email);
    
    /**
     * Update user password
     * @param userId User ID
     * @param newPassword New plain text password
     */
    void updatePassword(Long userId, String newPassword);
    
    // ===== SEARCH AND FILTER =====
    /**
     * Search users by name or email
     * @param searchTerm Search term
     * @param pageable Pagination parameters
     * @return Paged search results
     */
    PagedResponse<UserResponseDTO> searchUsers(String searchTerm, Pageable pageable);
    
    /**
     * Get active users only
     * @param pageable Pagination parameters
     * @return Paged active users
     */
    PagedResponse<UserResponseDTO> getActiveUsers(Pageable pageable);
    
    // ===== BUSINESS OPERATIONS =====
    /**
     * Activate user account
     * @param userId User ID
     */
    void activateUser(Long userId);
    
    /**
     * Deactivate user account
     * @param userId User ID
     */
    void deactivateUser(Long userId);
    
    /**
     * Get user statistics
     * @return Map of user statistics (total, active, by role, etc.)
     */
    java.util.Map<String, Object> getUserStatistics();
}