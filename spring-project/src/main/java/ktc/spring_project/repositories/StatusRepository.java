package ktc.spring_project.repositories;

import ktc.spring_project.entities.Status;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface StatusRepository extends JpaRepository<Status, Long> {
    
    Optional<Status> findByTypeAndCode(String type, String code);
    
    List<Status> findByType(String type);
    
    List<Status> findByCode(String code);
    
    @Query("SELECT s FROM Status s WHERE s.type = :type")
    List<Status> findAllByType(@Param("type") String type);
    
    boolean existsByTypeAndCode(String type, String code);
}
