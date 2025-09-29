package ktc.spring_project.repositories;

import ktc.spring_project.entities.User;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Legacy UserRepository for backward compatibility
 * Extends UserJpaRepository which contains the updated single-role functionality
 */
@Repository
public interface UserRepository extends UserJpaRepository {
    
    // Legacy methods that use the role relationship
    // These methods are kept for backward compatibility
    
    @Query("SELECT u FROM User u WHERE u.role.id = :roleId")
    List<User> findByRoleId(@Param("roleId") Long roleId);
    
    @Query("SELECT u FROM User u WHERE u.role.roleName = :roleName")
    List<User> findByRole_RoleName(@Param("roleName") String roleName);
    
    // Note: The following methods are inherited from UserJpaRepository:
    // - findByUsername(String username)
    // - findByEmail(String email) 
    // - findByUsernameOrEmail(String username, String email)
    // - findByStatusId(Short statusId)
    // - findByRoleName(String roleName)
    // - findActiveDrivers()
    // - existsByUsername(String username)
    // - existsByEmail(String email)
    // - countByRoleName(String roleName)
}
