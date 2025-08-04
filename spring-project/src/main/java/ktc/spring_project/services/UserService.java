package ktc.spring_project.services;

import ktc.spring_project.entities.User;
import ktc.spring_project.repositories.UserJpaRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

// CRUD tài khoản

@Service
@Transactional
public class UserService {

    @Autowired
    private UserJpaRepository userJpaRepository;

    /**
     * Find user by username with roles fetched
     * @param username the username to search for
     * @return Optional<User> with roles populated
     */
    @Transactional(readOnly = true)
    public Optional<User> findByUserName(String username) {
        return userJpaRepository.findByUsernameWithRoles(username);
    }

    /**
     * Find user by email with roles fetched
     * @param email the email to search for
     * @return Optional<User> with roles populated
     */
    @Transactional(readOnly = true)
    public Optional<User> findByEmailWithRoles(String email) {
        return userJpaRepository.findByEmailWithRoles(email);
    }

    /**
     * Find user by ID with roles fetched
     * @param id the user ID to search for
     * @return Optional<User> with roles populated
     */
    @Transactional(readOnly = true)
    public Optional<User> findByIdWithRoles(Long id) {
        return userJpaRepository.findByIdWithRoles(id);
    }

    /**
     * Get all users with their roles
     * @return List of users with roles populated
     */
    @Transactional(readOnly = true)
    public List<User> findAllWithRoles() {
        return userJpaRepository.findAllWithRoles();
    }

    /**
     * Find users by role name
     * @param roleName the role name to search for
     * @return List of users with the specified role
     */
    @Transactional(readOnly = true)
    public List<User> findByRoleName(String roleName) {
        return userJpaRepository.findByRoleName(roleName);
    }

    /**
     * Find all active drivers
     * @return List of active driver users
     */
    @Transactional(readOnly = true)
    public List<User> findActiveDrivers() {
        return userJpaRepository.findActiveDrivers();
    }

    public List<User> getAllUsers() {
        return userJpaRepository.findAll();
    }

    public User getUserById(Long id) {
        return userJpaRepository.findById(id).orElseThrow();
    }

    public User createUser(User user) {
        return userJpaRepository.save(user);
    }

    public User updateUser(User user) {
        return userJpaRepository.save(user);
    }

    public void deleteUser(Long id) {
        userJpaRepository.deleteById(id);
    }

    /**
     * Check if user exists by username
     * @param username the username to check
     * @return true if user exists, false otherwise
     */
    @Transactional(readOnly = true)
    public boolean existsByUsername(String username) {
        return userJpaRepository.existsByUsername(username);
    }

    /**
     * Check if user exists by email
     * @param email the email to check
     * @return true if user exists, false otherwise
     */
    @Transactional(readOnly = true)
    public boolean existsByEmail(String email) {
        return userJpaRepository.existsByEmail(email);
    }

    /**
     * Count users by role name
     * @param roleName the role name to count
     * @return number of users with the specified role
     */
    @Transactional(readOnly = true)
    public long countByRoleName(String roleName) {
        return userJpaRepository.countByRoleName(roleName);
    }
}