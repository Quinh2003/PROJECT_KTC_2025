package ktc.spring_project.repositories;

import ktc.spring_project.entities.Status;
import ktc.spring_project.enums.StatusType;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface StatusRepository extends JpaRepository<Status, Short> {
    List<Status> findByName(String name);
    
    Optional<Status> findFirstByName(String name);

    List<Status> findByStatusType(StatusType statusType);

Optional<Status> findByStatusTypeAndName(StatusType statusType, String name);

@Query("SELECT s FROM Status s WHERE s.statusType = :statusType ORDER BY s.name")
List<Status> findByStatusTypeOrderByName(@Param("statusType") StatusType statusType);

boolean existsByStatusTypeAndName(StatusType statusType, String name);

}

