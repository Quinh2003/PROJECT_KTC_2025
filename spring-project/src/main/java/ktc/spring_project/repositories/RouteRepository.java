package ktc.spring_project.repositories;

import ktc.spring_project.entities.Route;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface RouteRepository extends JpaRepository<Route, Long> {
    
    List<Route> findByNameContainingIgnoreCase(String name);
    
    List<Route> findByAiOptimizedTrue();
    
    List<Route> findByCreatedById(Long createdById);
    
    @Query("SELECT r FROM Route r WHERE r.estimatedDistance >= :minDistance")
    List<Route> findByMinDistance(@Param("minDistance") Double minDistance);
    
    @Query("SELECT r FROM Route r WHERE r.estimatedCost <= :maxCost")
    List<Route> findByMaxCost(@Param("maxCost") Double maxCost);
}
