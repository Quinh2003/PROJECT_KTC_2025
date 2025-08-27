package ktc.spring_project.repositories;

import ktc.spring_project.entities.Route;
import ktc.spring_project.entities.User;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface RouteRepository extends JpaRepository<Route, Long> {
    List<Route> findByCreatedBy(User createdBy);

    @Query("SELECT r FROM Route r WHERE r.completedAt IS NOT NULL ORDER BY r.completedAt DESC")
    List<Route> findCompletedRoutesOrderByCompletedAt();
    
    @Query("SELECT r FROM Route r WHERE r.completedAt IS NULL ORDER BY r.createdAt ASC")
    List<Route> findPendingRoutesOrderByCreatedAt();
    
    @Query("SELECT r FROM Route r WHERE r.estimatedCost BETWEEN :minCost AND :maxCost")
    List<Route> findByEstimatedCostRange(@Param("minCost") BigDecimal minCost, 
                                       @Param("maxCost") BigDecimal maxCost);
    
    @Query("SELECT r FROM Route r WHERE r.estimatedDistanceKm BETWEEN :minDistance AND :maxDistance")
    List<Route> findByEstimatedDistanceRange(@Param("minDistance") BigDecimal minDistance, 
                                           @Param("maxDistance") BigDecimal maxDistance);
    
    @Query("SELECT r FROM Route r WHERE r.createdAt BETWEEN :startDate AND :endDate ORDER BY r.createdAt DESC")
    List<Route> findRoutesCreatedBetween(@Param("startDate") LocalDateTime startDate, 
                                       @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT COUNT(r) FROM Route r WHERE r.completedAt IS NOT NULL")
    long countCompletedRoutes();
    
    @Query("SELECT AVG(r.estimatedCost) FROM Route r WHERE r.completedAt IS NOT NULL")
    BigDecimal getAverageCompletedRouteCost();
}

