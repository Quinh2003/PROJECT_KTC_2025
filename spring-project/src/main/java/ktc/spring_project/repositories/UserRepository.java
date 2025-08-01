package ktc.spring_project.repositories;

import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Role;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository for User entity
 * Handles user management, authentication, and role-based queries
 */
@Repository
public interface UserRepository extends BaseRepository<User, Long> {
    
    /**
     * Find user by email for authentication
     */
    Optional<User> findByEmail(@Param("email") String email);
    
    /**
     * Find user by email and active status
     */
    Optional<User> findByEmailAndIsActive(@Param("email") String email, @Param("isActive") Boolean isActive);
    
    /**
     * Check if email exists
     */
    boolean existsByEmail(@Param("email") String email);
    
    /**
     * Find all active users
     */
    @Override
    @Query("SELECT u FROM User u WHERE u.isActive = true ORDER BY u.name")
    List<User> findAllActive();
    
    /**
     * Find users by role
     */
    List<User> findByRoleAndIsActive(@Param("role") Role role, @Param("isActive") Boolean isActive);
    
    /**
     * Find users by role name
     */
    @Query("SELECT u FROM User u WHERE u.role.roleName = :roleName AND u.isActive = true ORDER BY u.name")
    List<User> findByRoleName(@Param("roleName") String roleName);
    
    /**
     * Find all drivers (for assignment)
     */
    @Query("SELECT u FROM User u WHERE u.role.roleName = 'DRIVER' AND u.isActive = true ORDER BY u.name")
    List<User> findAllDrivers();
    
    /**
     * Find all customers
     */
    @Query("SELECT u FROM User u WHERE u.role.roleName = 'CUSTOMER' AND u.isActive = true ORDER BY u.name")
    List<User> findAllCustomers();
    
    /**
     * Find available drivers (not currently assigned)
     */
    @Query("SELECT u FROM User u WHERE u.role.roleName = 'DRIVER' AND u.isActive = true " +
           "AND u.id NOT IN (SELECT DISTINCT o.driver.id FROM Order o WHERE o.driver IS NOT NULL " +
           "AND o.status.code IN ('READY', 'ON_DELIVERY')) ORDER BY u.name")
    List<User> findAvailableDrivers();
    
    /**
     * Search users by name or email
     */
    @Query("SELECT u FROM User u WHERE u.isActive = true AND " +
           "(LOWER(u.name) LIKE LOWER(CONCAT('%', :searchTerm, '%')) OR " +
           "LOWER(u.email) LIKE LOWER(CONCAT('%', :searchTerm, '%'))) ORDER BY u.name")
    Page<User> searchByNameOrEmail(@Param("searchTerm") String searchTerm, Pageable pageable);
    
    /**
     * Find users by phone number
     */
    Optional<User> findByPhoneAndIsActive(@Param("phone") String phone, @Param("isActive") Boolean isActive);
    
    /**
     * Count users by role
     */
    @Query("SELECT COUNT(u) FROM User u WHERE u.role.roleName = :roleName AND u.isActive = true")
    Long countByRoleName(@Param("roleName") String roleName);
    
    /**
     * Find users created within date range
     */
    @Query("SELECT u FROM User u WHERE u.isActive = true AND u.createdAt BETWEEN :startDate AND :endDate ORDER BY u.createdAt DESC")
    List<User> findByCreatedAtBetween(@Param("startDate") java.time.LocalDateTime startDate, 
                                     @Param("endDate") java.time.LocalDateTime endDate);
                                     
    /**
     * Find users by role ID  
     */
    @Query("SELECT u FROM User u WHERE u.role.id = :roleId")
    List<User> findByRoleId(@Param("roleId") Long roleId);
    
    /**
     * Find active users with pagination
     */
    @Query("SELECT u FROM User u WHERE u.isActive = true")
    Page<User> findByIsActiveTrue(Pageable pageable);
}