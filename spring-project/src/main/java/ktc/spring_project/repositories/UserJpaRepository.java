package ktc.spring_project.repositories;

import ktc.spring_project.entities.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface UserJpaRepository extends JpaRepository<User, Long> {
    
    @Query("SELECT u FROM User u LEFT JOIN FETCH u.role WHERE u.username = :username")
    Optional<User> findByUsernameWithRole(@Param("username") String username);
    
    @Query("SELECT u FROM User u LEFT JOIN FETCH u.role WHERE u.email = :email")
    Optional<User> findByEmailWithRole(@Param("email") String email);
    
    @Query("SELECT u FROM User u LEFT JOIN FETCH u.role")
    List<User> findAllWithRole();
    
    @Query("SELECT u FROM User u LEFT JOIN FETCH u.role WHERE u.id = :id")
    Optional<User> findByIdWithRole(@Param("id") Long id);
    
    @Query("SELECT u FROM User u WHERE u.role.roleName = :roleName")
    List<User> findByRoleName(@Param("roleName") String roleName);
    
    @Query("SELECT u FROM User u WHERE u.role.roleName = 'DRIVER' AND u.status.name = 'ACTIVE'")
    List<User> findActiveDrivers();
    
    Optional<User> findByUsername(String username);
    
    Optional<User> findByEmail(String email);
    
    Optional<User> findByUsernameOrEmail(String username, String email);
    
    List<User> findByStatusId(Short statusId);
    
    boolean existsByUsername(String username);
    
    boolean existsByEmail(String email);
    
    @Query("SELECT COUNT(u) FROM User u WHERE u.role.roleName = :roleName")
    long countByRoleName(@Param("roleName") String roleName);
    
    @Query("SELECT DISTINCT u FROM User u JOIN FETCH u.role r WHERE r.isActive = true")
    List<User> findUsersWithActiveRole();

    // Methods for filtering support
    @Query("SELECT u FROM User u WHERE u.role.roleName = :roleName")
    List<User> findByRoleRoleName(@Param("roleName") String roleName);

    @Query("SELECT u FROM User u WHERE u.status.name = :statusName")
    List<User> findByStatusName(@Param("statusName") String statusName);

    List<User> findByUsernameContainingIgnoreCase(String username);

    List<User> findByFullNameContainingIgnoreCase(String fullName);

    // Count methods for staff statistics
    @Query("SELECT COUNT(u) FROM User u WHERE u.role.roleName = :roleName")
    long countByRole_RoleName(@Param("roleName") String roleName);

    @Query("SELECT COUNT(u) FROM User u WHERE u.status.name = :statusName AND u.role.roleName != :excludeRole")
    long countByStatusNameAndRoleRoleNameNot(@Param("statusName") String statusName, @Param("excludeRole") String excludeRole);
}
