package ktc.spring_project.repositories;

import ktc.spring_project.entities.DeliveryTracking;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface DeliveryTrackingRepository extends JpaRepository<DeliveryTracking, Long> {
    
    List<DeliveryTracking> findByVehicleId(Long vehicleId);
    
    List<DeliveryTracking> findByStatusId(Long statusId);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.timestamp BETWEEN :startTime AND :endTime")
    List<DeliveryTracking> findByTimeRange(@Param("startTime") LocalDateTime startTime, 
                                         @Param("endTime") LocalDateTime endTime);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.vehicle.id = :vehicleId ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findLatestByVehicleId(@Param("vehicleId") Long vehicleId);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.status.code = :statusCode")
    List<DeliveryTracking> findByStatusCode(@Param("statusCode") String statusCode);
}
