package ktc.spring_project.repositories;

import ktc.spring_project.entities.ActivityLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface ActivityLogRepository extends JpaRepository<ActivityLog, Long> {
    
    List<ActivityLog> findByActorId(Long actorId);
    
    List<ActivityLog> findByRoleId(Long roleId);
    
    List<ActivityLog> findByStatusId(Long statusId);
    
    @Query("SELECT al FROM ActivityLog al WHERE al.actionTimestamp BETWEEN :startTime AND :endTime")
    List<ActivityLog> findByTimeRange(@Param("startTime") LocalDateTime startTime, 
                                    @Param("endTime") LocalDateTime endTime);
    
    @Query("SELECT al FROM ActivityLog al WHERE al.actor.id = :actorId ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findByActorIdOrderByTimestampDesc(@Param("actorId") Long actorId);
    
    @Query("SELECT al FROM ActivityLog al ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findAllOrderByTimestampDesc();
}
