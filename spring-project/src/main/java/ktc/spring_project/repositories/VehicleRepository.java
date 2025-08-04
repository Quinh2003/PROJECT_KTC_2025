package ktc.spring_project.repositories;

import ktc.spring_project.entities.Vehicle;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehicleRepository extends JpaRepository<Vehicle, Long> {
    
    Optional<Vehicle> findByLicensePlate(String licensePlate);
    
    List<Vehicle> findByVehicleType(String vehicleType);
    
    List<Vehicle> findByStatusId(Long statusId);
    
    @Query("SELECT v FROM Vehicle v WHERE v.status.code = :statusCode")
    List<Vehicle> findByStatusCode(@Param("statusCode") String statusCode);
    
    @Query("SELECT v FROM Vehicle v WHERE v.capacity >= :minCapacity")
    List<Vehicle> findByMinCapacity(@Param("minCapacity") Double minCapacity);
    
    @Query("SELECT v FROM Vehicle v WHERE v.status.code = 'AVAILABLE'")
    List<Vehicle> findAvailableVehicles();
    
    boolean existsByLicensePlate(String licensePlate);
}
