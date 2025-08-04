package ktc.spring_project.repositories;

import ktc.spring_project.entities.Status;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface StatusRepository extends JpaRepository<Status, Short> {
    
    List<Status> findByType(String type);
    
    Optional<Status> findByTypeAndName(String type, String name);
    
    @Query("SELECT s FROM Status s WHERE s.type = :type ORDER BY s.name")
    List<Status> findByTypeOrderByName(@Param("type") String type);
    
    boolean existsByTypeAndName(String type, String name);
}
