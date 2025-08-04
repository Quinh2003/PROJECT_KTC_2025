package ktc.spring_project.repositories;

import ktc.spring_project.entities.ActivityLog;
import ktc.spring_project.enums.ActionType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface ActivityLogRepository extends JpaRepository<ActivityLog, Long> {
    
    List<ActivityLog> findByActorId(Long actorId);
    
    List<ActivityLog> findByActionType(ActionType actionType);
    
    List<ActivityLog> findByTableName(String tableName);
    
    List<ActivityLog> findByRoleId(Long roleId);
    
    List<ActivityLog> findByStatusId(Short statusId);
    
    @Query("SELECT al FROM ActivityLog al WHERE al.actionTimestamp BETWEEN :startTime AND :endTime ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findLogsBetweenDates(@Param("startTime") LocalDateTime startTime, 
                                         @Param("endTime") LocalDateTime endTime);
    
    @Query("SELECT al FROM ActivityLog al WHERE al.tableName = :tableName AND al.recordId = :recordId ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findByTableNameAndRecordId(@Param("tableName") String tableName, 
                                                @Param("recordId") Long recordId);
    
    @Query("SELECT al FROM ActivityLog al WHERE al.actor.id = :actorId ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findByActorIdOrderByTimestampDesc(@Param("actorId") Long actorId);
    
    @Query("SELECT al FROM ActivityLog al WHERE al.actor.username = :username ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findByUsernameOrderByTimestampDesc(@Param("username") String username);
    
    @Query("SELECT al FROM ActivityLog al WHERE al.role.roleName = :roleName ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findByRoleNameOrderByTimestampDesc(@Param("roleName") String roleName);
    
    @Query("SELECT COUNT(al) FROM ActivityLog al WHERE al.actionType = :actionType")
    long countByActionType(@Param("actionType") ActionType actionType);
    
    @Query("SELECT COUNT(al) FROM ActivityLog al WHERE al.actor.id = :actorId")
    long countByActorId(@Param("actorId") Long actorId);
    
    @Query("SELECT COUNT(al) FROM ActivityLog al WHERE DATE(al.actionTimestamp) = CURRENT_DATE")
    long countTodayLogs();
    
    @Query("SELECT DISTINCT al.tableName FROM ActivityLog al ORDER BY al.tableName")
    List<String> findDistinctTableNames();
}  