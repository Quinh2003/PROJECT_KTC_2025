package ktc.spring_project.repositories;

import ktc.spring_project.entities.Role;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository for Role entity
 * Handles role-based access control and permissions
 */
@Repository
public interface RoleRepository extends BaseRepository<Role, Long> {
    
    /**
     * Find role by name
     */
    Optional<Role> findByRoleName(@Param("roleName") String roleName);
    
    /**
     * Check if role exists by name
     */
    boolean existsByRoleName(@Param("roleName") String roleName);
    
    /**
     * Find roles containing specific permission
     * Note: Temporarily using LIKE for demo (JSON support disabled)
     */
    @Query("SELECT r FROM Role r WHERE r.permission LIKE CONCAT('%', :permission, '%')")
    List<Role> findByPermission(@Param("permission") String permission);
    
    /**
     * Find all roles ordered by name
     */
    List<Role> findAllByOrderByRoleName();
    
    /**
     * Get role names for user assignment
     */
    @Query("SELECT r.roleName FROM Role r ORDER BY r.roleName")
    List<String> getAllRoleNames();
    
    /**
     * Find customer roles (for customer users)
     */
    @Query("SELECT r FROM Role r WHERE r.roleName IN ('CUSTOMER') ORDER BY r.roleName")
    List<Role> findCustomerRoles();
    
    /**
     * Find operational roles (for staff/drivers)
     */
    @Query("SELECT r FROM Role r WHERE r.roleName IN ('DRIVER', 'MANAGER') ORDER BY r.roleName")
    List<Role> findOperationalRoles();
    
    /**
     * Find admin roles (for system administration)
     */
    @Query("SELECT r FROM Role r WHERE r.roleName IN ('ADMIN', 'MANAGER') ORDER BY r.roleName")
    List<Role> findAdminRoles();
}