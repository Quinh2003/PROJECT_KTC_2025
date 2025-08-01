package ktc.spring_project.repositories;

import ktc.spring_project.entities.Status;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository for Status entity
 * Handles centralized status management across all entities
 */
@Repository
public interface StatusRepository extends BaseRepository<Status, Long> {
    
    /**
     * Find status by type and code
     */
    Optional<Status> findByTypeAndCode(@Param("type") String type, @Param("code") String code);
    
    /**
     * Find all statuses by type
     */
    List<Status> findByTypeOrderByCode(@Param("type") String type);
    
    /**
     * Check if status exists for type and code
     */
    boolean existsByTypeAndCode(@Param("type") String type, @Param("code") String code);
    
    /**
     * Find all distinct status types
     */
    @Query("SELECT DISTINCT s.type FROM Status s ORDER BY s.type")
    List<String> findAllDistinctTypes();
    
    /**
     * Find all statuses for order management
     */
    @Query("SELECT s FROM Status s WHERE s.type = 'ORDER' ORDER BY s.code")
    List<Status> findOrderStatuses();
    
    /**
     * Find all statuses for vehicle management
     */
    @Query("SELECT s FROM Status s WHERE s.type = 'VEHICLE' ORDER BY s.code")
    List<Status> findVehicleStatuses();
    
    /**
     * Find all statuses for payment management
     */
    @Query("SELECT s FROM Status s WHERE s.type = 'PAYMENT' ORDER BY s.code")
    List<Status> findPaymentStatuses();
}