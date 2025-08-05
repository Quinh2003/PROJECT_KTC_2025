package ktc.spring_project.services;

import ktc.spring_project.entities.User;
import ktc.spring_project.repositories.UserJpaRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class UserService {

    @Autowired
    private UserJpaRepository userRepository;

    public User createUser(User user) {
        return userRepository.save(user);
    }

    public User getUserById(Long id) {
        return userRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("User not found with id: " + id));
    }

    public List<User> getAllUsers() {
        return userRepository.findAll();
    }

    public User updateUser(Long id, User userDetails) {
        User user = getUserById(id);
        user.setUsername(userDetails.getUsername());
        user.setEmail(userDetails.getEmail());
        user.setPassword(userDetails.getPassword());
        user.setFullName(userDetails.getFullName());
        user.setPhone(userDetails.getPhone());
        user.setRole(userDetails.getRole());
        user.setStatus(userDetails.getStatus());
        user.setNotes(userDetails.getNotes());
        return userRepository.save(user);
    }

    public void deleteUser(Long id) {
        User user = getUserById(id);
        userRepository.delete(user);
    }

    /**
     * Get filtered users based on role, status, and search terms
     *
     * @param role Optional role filter
     * @param status Optional status filter
     * @param search Optional search term for username or fullName
     * @return List of users matching criteria
     */
    public List<User> getFilteredUsers(String role, String status, String search) {
        // Simple implementation - in a real app, would use more sophisticated filtering
        if (role == null && status == null && search == null) {
            return getAllUsers();
        }

        List<User> filteredUsers = new ArrayList<>();

        // Filter by role if specified
        if (role != null && !role.isEmpty()) {
            return userRepository.findByRoleRoleName(role);
        }

        // Filter by status if specified
        if (status != null && !status.isEmpty()) {
            return userRepository.findByStatusName(status);
        }

        // Search by username or full name
        if (search != null && !search.isEmpty()) {
            filteredUsers.addAll(userRepository.findByUsernameContainingIgnoreCase(search));
            filteredUsers.addAll(userRepository.findByFullNameContainingIgnoreCase(search));
        }

        return filteredUsers;
    }

    /**
     * Get current authenticated user
     *
     * @param authentication Spring Security Authentication object
     * @return User entity for the authenticated user
     */
    public User getCurrentUser(Authentication authentication) {
        if (authentication == null) {
            throw new RuntimeException("No authentication provided");
        }

        UserDetails userDetails = (UserDetails) authentication.getPrincipal();
        return userRepository.findByUsername(userDetails.getUsername())
                .orElseThrow(() -> new EntityNotFoundException("User not found"));
    }
}