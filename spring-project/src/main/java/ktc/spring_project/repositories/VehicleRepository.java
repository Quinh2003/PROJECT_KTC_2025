package ktc.spring_project.repositories;

import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.enums.VehicleType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehicleRepository extends JpaRepository<Vehicle, Long> {
    
    Optional<Vehicle> findByLicensePlate(String licensePlate);
    
    List<Vehicle> findByVehicleType(VehicleType vehicleType);
    
    List<Vehicle> findByStatusId(Short statusId);
    
    List<Vehicle> findByCurrentDriverId(Long driverId);
    
    @Query("SELECT v FROM Vehicle v WHERE v.currentDriver IS NULL AND v.status.name = 'AVAILABLE'")
    List<Vehicle> findAvailableVehicles();
    
    @Query("SELECT v FROM Vehicle v WHERE v.status.name = 'ACTIVE' ORDER BY v.licensePlate")
    List<Vehicle> findActiveVehiclesOrderByLicense();
    
    @Query("SELECT v FROM Vehicle v WHERE v.vehicleType = :type AND v.status.name = 'AVAILABLE'")
    List<Vehicle> findAvailableVehiclesByType(@Param("type") VehicleType type);
    
    boolean existsByLicensePlate(String licensePlate);
    
    @Query("SELECT COUNT(v) FROM Vehicle v WHERE v.status.name = 'ACTIVE'")
    long countActiveVehicles();
}
