package ktc.spring_project.repositories;

import ktc.spring_project.entities.Role;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface RoleRepository extends JpaRepository<Role, Long> {
    
    Optional<Role> findByRoleName(String roleName);

    long countByIsActive(Boolean isActive);
    
    List<Role> findByIsActive(Boolean isActive);
    
    @Query("SELECT r FROM Role r WHERE r.isActive = true ORDER BY r.roleName")
    List<Role> findActiveRolesOrderByName();
    
    boolean existsByRoleName(String roleName);
}

