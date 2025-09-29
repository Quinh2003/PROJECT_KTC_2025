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

    // Tìm theo actor ID
    List<ActivityLog> findByActorId(Long actorId);

    // Tìm system activities (không có actor)
    List<ActivityLog> findByActorIdIsNull();

    // Tìm theo loại hành động
    List<ActivityLog> findByActionType(ActionType actionType);

    // Tìm theo tên bảng
    List<ActivityLog> findByTableName(String tableName);

    // Tìm theo role ID
    @Query("SELECT al FROM ActivityLog al WHERE al.role.id = :roleId")
    List<ActivityLog> findByRoleId(@Param("roleId") Long roleId);

    // Tìm theo status ID
    @Query("SELECT al FROM ActivityLog al WHERE al.status.id = :statusId")
    List<ActivityLog> findByStatusId(@Param("statusId") Short statusId);

    // Tìm log trong khoảng thời gian
    @Query("SELECT al FROM ActivityLog al WHERE al.actionTimestamp BETWEEN :startTime AND :endTime ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findLogsBetweenDates(@Param("startTime") LocalDateTime startTime,
                                           @Param("endTime") LocalDateTime endTime);

    // Tìm theo table name và record id
    @Query("SELECT al FROM ActivityLog al WHERE al.tableName = :tableName AND al.recordId = :recordId ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findByTableNameAndRecordId(@Param("tableName") String tableName,
                                                 @Param("recordId") Long recordId);

    // Tìm log theo actor ID (sắp xếp mới nhất)
    List<ActivityLog> findByActorIdOrderByActionTimestampDesc(Long actorId);

    // Tìm theo role name
    @Query("SELECT al FROM ActivityLog al WHERE al.role.roleName = :roleName ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findByRoleNameOrderByActionTimestampDesc(@Param("roleName") String roleName);

    // Đếm theo action type
    long countByActionType(ActionType actionType);

    // Đếm theo actor ID
    long countByActorId(Long actorId);

    // Đếm log hôm nay
    @Query("SELECT COUNT(al) FROM ActivityLog al WHERE DATE(al.actionTimestamp) = CURRENT_DATE")
    long countTodayLogs();

    // Danh sách table name duy nhất
    @Query("SELECT DISTINCT al.tableName FROM ActivityLog al ORDER BY al.tableName")
    List<String> findDistinctTableNames();
    
    // Kiểm tra user có thực hiện action nào đó không - sử dụng metadata field
    @Query("SELECT COUNT(al) > 0 FROM ActivityLog al WHERE al.actor.id = :actorId AND al.metadata LIKE %:action%")
    boolean existsByActorIdAndActionContaining(@Param("actorId") Long actorId, @Param("action") String action);

    // Thay đổi method này cho khớp với database schema  
    @Query("SELECT COUNT(al) > 0 FROM ActivityLog al WHERE al.actor.id = :userId AND al.metadata LIKE %:action%")
    boolean existsByUserIdAndActionContaining(@Param("userId") Long userId, @Param("action") String action);
}
