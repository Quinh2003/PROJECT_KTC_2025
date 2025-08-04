package ktc.spring_project.repositories;

import ktc.spring_project.entities.User;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Legacy UserRepository for backward compatibility
 * Extends UserJpaRepository which contains the new many-to-many functionality
 */
@Repository
public interface UserRepository extends UserJpaRepository {
    
    // Legacy methods that use the old single-role relationship
    // These methods are now deprecated but kept for backward compatibility
    
    @Deprecated
    @Query("SELECT u FROM User u JOIN u.roles r WHERE r.id = :roleId")
    List<User> findByRoleId(@Param("roleId") Long roleId);
    
    // Note: The following methods are inherited from UserJpaRepository:
    // - findByUsername(String username)
    // - findByEmail(String email) 
    // - findByUsernameOrEmail(String username, String email)
    // - findByStatusId(Short statusId)
    // - findByRoleName(String roleName) - updated for many-to-many
    // - findActiveDrivers() - updated for many-to-many
    // - existsByUsername(String username)
    // - existsByEmail(String email)
    // - countByRoleName(String roleName) - updated for many-to-many
}
