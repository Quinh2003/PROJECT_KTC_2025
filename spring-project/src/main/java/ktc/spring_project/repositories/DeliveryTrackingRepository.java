package ktc.spring_project.repositories;

import ktc.spring_project.entities.DeliveryTracking;
import ktc.spring_project.entities.Vehicle;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repository for DeliveryTracking entity
 * Handles real-time delivery tracking and location updates
 */
@Repository
public interface DeliveryTrackingRepository extends BaseRepository<DeliveryTracking, Long> {
    
    /**
     * Find tracking records by vehicle
     */
    List<DeliveryTracking> findByVehicleOrderByTimestampDesc(@Param("vehicle") Vehicle vehicle);
    
    /**
     * Find latest tracking for vehicle
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.vehicle = :vehicle " +
           "ORDER BY dt.timestamp DESC LIMIT 1")
    Optional<DeliveryTracking> findLatestByVehicle(@Param("vehicle") Vehicle vehicle);
    
    /**
     * Find tracking by status code
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.status.code = :statusCode " +
           "ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findByStatusCode(@Param("statusCode") String statusCode);
    
    /**
     * Find active deliveries (in progress tracking)
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.status.code IN ('EN_ROUTE', 'ARRIVED', 'DEPARTED') " +
           "ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findActiveDeliveries();
    
    /**
     * Find tracking within date range
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.timestamp BETWEEN :startDate AND :endDate " +
           "ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findByTimestampRange(@Param("startDate") LocalDateTime startDate, 
                                               @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find tracking by location
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE LOWER(dt.location) LIKE LOWER(CONCAT('%', :location, '%')) " +
           "ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findByLocation(@Param("location") String location);
    
    /**
     * Find tracking with GPS coordinates
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.latitude IS NOT NULL AND dt.longitude IS NOT NULL " +
           "ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findTrackingWithCoordinates();
    
    /**
     * Find recent vehicle locations (last position for each vehicle)
     */
    @Query("SELECT dt1 FROM DeliveryTracking dt1 WHERE dt1.timestamp = " +
           "(SELECT MAX(dt2.timestamp) FROM DeliveryTracking dt2 WHERE dt2.vehicle = dt1.vehicle) " +
           "ORDER BY dt1.timestamp DESC")
    List<DeliveryTracking> findLatestVehiclePositions();
    
    /**
     * Find tracking by GPS area (within radius)
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.latitude IS NOT NULL AND dt.longitude IS NOT NULL " +
           "AND (6371 * acos(cos(radians(:centerLat)) * cos(radians(dt.latitude)) * " +
           "cos(radians(dt.longitude) - radians(:centerLng)) + sin(radians(:centerLat)) * " +
           "sin(radians(dt.latitude)))) <= :radiusKm ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findWithinRadius(@Param("centerLat") Double centerLat, 
                                           @Param("centerLng") Double centerLng, 
                                           @Param("radiusKm") Double radiusKm);
    
    /**
     * Find overdue deliveries (no updates for specified time)
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.timestamp < :cutoffTime " +
           "AND dt.status.code NOT IN ('COMPLETED', 'FAILED') ORDER BY dt.timestamp ASC")
    List<DeliveryTracking> findOverdueDeliveries(@Param("cutoffTime") LocalDateTime cutoffTime);
    
    /**
     * Get delivery performance metrics
     */
    @Query("SELECT dt.status.code, COUNT(dt) as count, " +
           "AVG(TIMESTAMPDIFF(MINUTE, dt.createdAt, dt.arrivedAt)) as avgDuration " +
           "FROM DeliveryTracking dt WHERE dt.arrivedAt IS NOT NULL " +
           "AND dt.timestamp BETWEEN :startDate AND :endDate " +
           "GROUP BY dt.status.code ORDER BY dt.status.code")
    List<Object[]> getDeliveryPerformanceMetrics(@Param("startDate") LocalDateTime startDate, 
                                                 @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find tracking requiring attention (delays, issues)
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE " +
           "(dt.notes IS NOT NULL AND LOWER(dt.notes) LIKE '%issue%') OR " +
           "(dt.timestamp < :thresholdTime AND dt.status.code NOT IN ('COMPLETED', 'FAILED')) " +
           "ORDER BY dt.timestamp ASC")
    List<DeliveryTracking> findTrackingRequiringAttention(@Param("thresholdTime") LocalDateTime thresholdTime);
    
    /**
     * Count active trackings by status
     */
    @Query("SELECT COUNT(dt) FROM DeliveryTracking dt WHERE dt.status.code = :statusCode " +
           "AND dt.timestamp >= :sinceDatetime")
    Long countActiveByStatus(@Param("statusCode") String statusCode, 
                           @Param("sinceDatetime") LocalDateTime sinceDatetime);
    
    /**
     * Find vehicle journey history
     */
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.vehicle = :vehicle " +
           "AND dt.timestamp BETWEEN :startDate AND :endDate " +
           "ORDER BY dt.timestamp ASC")
    List<DeliveryTracking> findVehicleJourney(@Param("vehicle") Vehicle vehicle,
                                             @Param("startDate") LocalDateTime startDate,
                                             @Param("endDate") LocalDateTime endDate);
}