package ktc.spring_project.repositories;

import ktc.spring_project.entities.Vehicle;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * Repository for Vehicle entity
 * Handles fleet management and vehicle assignments
 */
@Repository
public interface VehicleRepository extends BaseRepository<Vehicle, Long> {
    
    /**
     * Find vehicle by license plate
     */
    Optional<Vehicle> findByLicensePlate(@Param("licensePlate") String licensePlate);
    
    /**
     * Check if license plate exists
     */
    boolean existsByLicensePlate(@Param("licensePlate") String licensePlate);
    
    /**
     * Find vehicles by status code
     */
    @Query("SELECT v FROM Vehicle v WHERE v.status.code = :statusCode ORDER BY v.licensePlate")
    List<Vehicle> findByStatusCode(@Param("statusCode") String statusCode);
    
    /**
     * Find available vehicles (can be assigned)
     */
    @Query("SELECT v FROM Vehicle v WHERE v.status.code = 'AVAILABLE' ORDER BY v.capacity DESC")
    List<Vehicle> findAvailableVehicles();
    
    /**
     * Find vehicles in use
     */
    @Query("SELECT v FROM Vehicle v WHERE v.status.code = 'IN_USE' ORDER BY v.licensePlate")
    List<Vehicle> findVehiclesInUse();
    
    /**
     * Find vehicles by type
     */
    List<Vehicle> findByVehicleTypeOrderByCapacityDesc(@Param("vehicleType") String vehicleType);
    
    /**
     * Find vehicles with capacity greater than or equal to specified value
     */
    @Query("SELECT v FROM Vehicle v WHERE v.capacity >= :minCapacity " +
           "AND v.status.code = 'AVAILABLE' ORDER BY v.capacity ASC")
    List<Vehicle> findByMinCapacity(@Param("minCapacity") BigDecimal minCapacity);
    
    /**
     * Find vehicles suitable for delivery (available and operational)
     */
    @Query("SELECT v FROM Vehicle v WHERE v.status.code IN ('AVAILABLE') " +
           "ORDER BY v.capacity DESC")
    List<Vehicle> findSuitableForDelivery();
    
    /**
     * Search vehicles by license plate or type
     */
    @Query("SELECT v FROM Vehicle v WHERE " +
           "LOWER(v.licensePlate) LIKE LOWER(CONCAT('%', :searchTerm, '%')) OR " +
           "LOWER(v.vehicleType) LIKE LOWER(CONCAT('%', :searchTerm, '%')) " +
           "ORDER BY v.licensePlate")
    List<Vehicle> searchVehicles(@Param("searchTerm") String searchTerm);
    
    /**
     * Get vehicle utilization statistics
     */
    @Query("SELECT v.status.code, COUNT(v) FROM Vehicle v GROUP BY v.status.code ORDER BY v.status.code")
    List<Object[]> getVehicleUtilizationStats();
    
    /**
     * Find vehicles requiring maintenance
     */
    @Query("SELECT v FROM Vehicle v WHERE v.status.code = 'MAINTENANCE' ORDER BY v.updatedAt ASC")
    List<Vehicle> findVehiclesInMaintenance();
    
    /**
     * Count vehicles by status
     */
    @Query("SELECT COUNT(v) FROM Vehicle v WHERE v.status.code = :statusCode")
    Long countByStatus(@Param("statusCode") String statusCode);
    
    /**
     * Find vehicles by capacity range
     */
    @Query("SELECT v FROM Vehicle v WHERE v.capacity BETWEEN :minCapacity AND :maxCapacity " +
           "ORDER BY v.capacity")
    List<Vehicle> findByCapacityRange(@Param("minCapacity") BigDecimal minCapacity, 
                                     @Param("maxCapacity") BigDecimal maxCapacity);
    
    /**
     * Get fleet capacity summary
     */
    @Query("SELECT v.vehicleType, COUNT(v) as vehicleCount, SUM(v.capacity) as totalCapacity, " +
           "AVG(v.capacity) as avgCapacity FROM Vehicle v " +
           "WHERE v.status.code != 'RETIRED' GROUP BY v.vehicleType ORDER BY totalCapacity DESC")
    List<Object[]> getFleetCapacitySummary();
    
    /**
     * Find idle vehicles (available but not assigned recently)
     */
    @Query("SELECT v FROM Vehicle v WHERE v.status.code = 'AVAILABLE' " +
           "AND v.id NOT IN (SELECT DISTINCT o.vehicle.id FROM Order o WHERE o.vehicle IS NOT NULL " +
           "AND o.updatedAt >= :cutoffDate) ORDER BY v.updatedAt ASC")
    List<Vehicle> findIdleVehicles(@Param("cutoffDate") java.time.LocalDateTime cutoffDate);
}