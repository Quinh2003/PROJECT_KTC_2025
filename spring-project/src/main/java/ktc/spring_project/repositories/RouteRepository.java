package ktc.spring_project.repositories;

import ktc.spring_project.entities.Route;
import ktc.spring_project.entities.User;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Repository for Route entity
 * Handles route optimization and AI-powered logistics
 */
@Repository
public interface RouteRepository extends BaseRepository<Route, Long> {
    
    /**
     * Find routes by creator
     */
    List<Route> findByCreatedByOrderByCreatedAtDesc(@Param("createdBy") User createdBy);
    
    /**
     * Find AI-optimized routes
     */
    @Query("SELECT r FROM Route r WHERE r.aiOptimized = true ORDER BY r.estimatedCost ASC")
    List<Route> findAiOptimizedRoutes();
    
    /**
     * Find routes by distance range
     */
    @Query("SELECT r FROM Route r WHERE r.estimatedDistance BETWEEN :minDistance AND :maxDistance " +
           "ORDER BY r.estimatedDistance ASC")
    List<Route> findByDistanceRange(@Param("minDistance") BigDecimal minDistance, 
                                   @Param("maxDistance") BigDecimal maxDistance);
    
    /**
     * Find routes by duration range
     */
    @Query("SELECT r FROM Route r WHERE r.estimatedDuration BETWEEN :minDuration AND :maxDuration " +
           "ORDER BY r.estimatedDuration ASC")
    List<Route> findByDurationRange(@Param("minDuration") Integer minDuration, 
                                   @Param("maxDuration") Integer maxDuration);
    
    /**
     * Find most efficient routes (best cost per km)
     */
    @Query("SELECT r FROM Route r WHERE r.estimatedDistance > 0 AND r.estimatedCost > 0 " +
           "ORDER BY (r.estimatedCost / r.estimatedDistance) ASC")
    List<Route> findMostEfficientRoutes();
    
    /**
     * Find routes by cost range
     */
    @Query("SELECT r FROM Route r WHERE r.estimatedCost BETWEEN :minCost AND :maxCost " +
           "ORDER BY r.estimatedCost ASC")
    List<Route> findByCostRange(@Param("minCost") BigDecimal minCost, 
                               @Param("maxCost") BigDecimal maxCost);
    
    /**
     * Search routes by name
     */
    @Query("SELECT r FROM Route r WHERE LOWER(r.name) LIKE LOWER(CONCAT('%', :searchTerm, '%')) " +
           "ORDER BY r.name")
    List<Route> searchByName(@Param("searchTerm") String searchTerm);
    
    /**
     * Find routes with orders assigned
     */
    @Query("SELECT DISTINCT r FROM Route r JOIN r.orders o ORDER BY r.createdAt DESC")
    List<Route> findRoutesWithOrders();
    
    /**
     * Find unused routes (no orders assigned)
     */
    @Query("SELECT r FROM Route r WHERE r.orders IS EMPTY ORDER BY r.createdAt DESC")
    List<Route> findUnusedRoutes();
    
    /**
     * Get route efficiency statistics
     */
    @Query("SELECT AVG(r.estimatedDistance), AVG(r.estimatedDuration), " +
           "AVG(r.estimatedCost), COUNT(r) FROM Route r WHERE r.aiOptimized = :aiOptimized")
    List<Object[]> getRouteStatistics(@Param("aiOptimized") Boolean aiOptimized);
    
    /**
     * Find routes created within date range
     */
    @Query("SELECT r FROM Route r WHERE r.createdAt BETWEEN :startDate AND :endDate " +
           "ORDER BY r.createdAt DESC")
    List<Route> findByCreatedAtRange(@Param("startDate") LocalDateTime startDate, 
                                    @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find routes needing optimization (not AI optimized)
     */
    @Query("SELECT r FROM Route r WHERE r.aiOptimized = false " +
           "AND r.orders IS NOT EMPTY ORDER BY r.estimatedCost DESC")
    List<Route> findRoutesNeedingOptimization();
    
    /**
     * Calculate average route metrics
     */
    @Query("SELECT AVG(r.estimatedDistance), AVG(r.estimatedDuration), AVG(r.estimatedCost) " +
           "FROM Route r WHERE r.createdAt BETWEEN :startDate AND :endDate")
    List<Object[]> calculateAverageMetrics(@Param("startDate") LocalDateTime startDate, 
                                          @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find routes by waypoint count (using JSON_LENGTH)
     */
    @Query("SELECT r FROM Route r WHERE JSON_LENGTH(r.waypoints) = :waypointCount " +
           "ORDER BY r.estimatedDistance ASC")
    List<Route> findByWaypointCount(@Param("waypointCount") Integer waypointCount);
    
    /**
     * Get cost savings from AI optimization
     */
    @Query("SELECT (SELECT AVG(r1.estimatedCost) FROM Route r1 WHERE r1.aiOptimized = false) - " +
           "(SELECT AVG(r2.estimatedCost) FROM Route r2 WHERE r2.aiOptimized = true) as costSavings")
    BigDecimal calculateAiOptimizationSavings();
}