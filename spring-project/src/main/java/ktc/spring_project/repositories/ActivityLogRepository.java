package ktc.spring_project.repositories;

import ktc.spring_project.entities.ActivityLog;
import ktc.spring_project.entities.Role;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Repository for ActivityLog entity
 * Handles system activity logging and audit trails
 */
@Repository
public interface ActivityLogRepository extends BaseRepository<ActivityLog, Long> {
    
    /**
     * Find activity logs by actor (user)
     */
    List<ActivityLog> findByActorIdOrderByActionTimestampDesc(@Param("actorId") Long actorId);
    
    /**
     * Find activity logs by role
     */
    List<ActivityLog> findByRoleOrderByActionTimestampDesc(@Param("role") Role role);
    
    /**
     * Find activity logs by status code
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.status.code = :statusCode " +
           "ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findByStatusCode(@Param("statusCode") String statusCode);
    
    /**
     * Find activity logs by session
     */
    List<ActivityLog> findBySessionIdOrderByActionTimestampDesc(@Param("sessionId") String sessionId);
    
    /**
     * Find activity logs within date range
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.actionTimestamp BETWEEN :startDate AND :endDate " +
           "ORDER BY al.actionTimestamp DESC")
    Page<ActivityLog> findByDateRange(@Param("startDate") LocalDateTime startDate, 
                                     @Param("endDate") LocalDateTime endDate, 
                                     Pageable pageable);
    
    /**
     * Find failed activities
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.status.code IN ('FAILED', 'ERROR') " +
           "ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findFailedActivities();
    
    /**
     * Find successful activities
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.status.code IN ('SUCCESS', 'COMPLETED') " +
           "ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findSuccessfulActivities();
    
    /**
     * Get activity statistics by role
     */
    @Query("SELECT al.role.roleName, COUNT(al) as activityCount, " +
           "SUM(CASE WHEN al.status.code IN ('SUCCESS', 'COMPLETED') THEN 1 ELSE 0 END) as successCount " +
           "FROM ActivityLog al WHERE al.actionTimestamp BETWEEN :startDate AND :endDate " +
           "GROUP BY al.role.roleName ORDER BY activityCount DESC")
    List<Object[]> getActivityStatisticsByRole(@Param("startDate") LocalDateTime startDate, 
                                              @Param("endDate") LocalDateTime endDate);
    
    /**
     * Get activity statistics by status
     */
    @Query("SELECT al.status.code, COUNT(al) as count FROM ActivityLog al " +
           "WHERE al.actionTimestamp BETWEEN :startDate AND :endDate " +
           "GROUP BY al.status.code ORDER BY count DESC")
    List<Object[]> getActivityStatisticsByStatus(@Param("startDate") LocalDateTime startDate, 
                                                 @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find user activities (non-system activities)
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.actorId IS NOT NULL " +
           "ORDER BY al.actionTimestamp DESC")
    Page<ActivityLog> findUserActivities(Pageable pageable);
    
    /**
     * Find system activities (automated activities)
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.actorId IS NULL " +
           "ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findSystemActivities();
    
    /**
     * Search activities by metadata content
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.metadata IS NOT NULL " +
           "AND LOWER(al.metadata) LIKE LOWER(CONCAT('%', :searchTerm, '%')) " +
           "ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> searchByMetadata(@Param("searchTerm") String searchTerm);
    
    /**
     * Find recent activities for monitoring
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.actionTimestamp >= :sinceTime " +
           "ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findRecentActivities(@Param("sinceTime") LocalDateTime sinceTime);
    
    /**
     * Count activities by actor and date range
     */
    @Query("SELECT COUNT(al) FROM ActivityLog al WHERE al.actorId = :actorId " +
           "AND al.actionTimestamp BETWEEN :startDate AND :endDate")
    Long countByActorAndDateRange(@Param("actorId") Long actorId,
                                 @Param("startDate") LocalDateTime startDate,
                                 @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find activities with device info (login tracking)
     */
    @Query("SELECT al FROM ActivityLog al WHERE al.deviceInfo IS NOT NULL " +
           "ORDER BY al.actionTimestamp DESC")
    List<ActivityLog> findActivitiesWithDeviceInfo();
    
    /**
     * Get hourly activity distribution
     */
    @Query("SELECT HOUR(al.actionTimestamp) as hour, COUNT(al) as count " +
           "FROM ActivityLog al WHERE al.actionTimestamp BETWEEN :startDate AND :endDate " +
           "GROUP BY HOUR(al.actionTimestamp) ORDER BY hour")
    List<Object[]> getHourlyActivityDistribution(@Param("startDate") LocalDateTime startDate, 
                                                 @Param("endDate") LocalDateTime endDate);
    
    /**
     * Clean up old activity logs (for maintenance)
     */
    @Query("DELETE FROM ActivityLog al WHERE al.actionTimestamp < :cutoffDate")
    void deleteOldActivityLogs(@Param("cutoffDate") LocalDateTime cutoffDate);
}