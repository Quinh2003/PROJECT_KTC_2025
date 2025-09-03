package ktc.spring_project.repositories;

import ktc.spring_project.entities.Delivery;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.enums.TransportMode;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface DeliveryRepository extends JpaRepository<Delivery, Long> {
    
    List<Delivery> findByOrderId(Long orderId);
    
    boolean existsByOrderId(Long orderId);
    
    List<Delivery> findByVehicleId(Long vehicleId);
    
    List<Delivery> findByDriverId(Long driverId);
    
    
    List<Delivery> findByTransportMode(TransportMode transportMode);
    
    List<Delivery> findByServiceType(ServiceType serviceType);
    
    @Query("SELECT d FROM Delivery d WHERE d.scheduleDeliveryTime BETWEEN :startDate AND :endDate ORDER BY d.scheduleDeliveryTime")
    List<Delivery> findDeliveriesScheduledBetween(@Param("startDate") LocalDateTime startDate, 
                                                @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT d FROM Delivery d WHERE d.actualDeliveryTime IS NULL AND d.scheduleDeliveryTime < :currentTime")
    List<Delivery> findOverdueDeliveries(@Param("currentTime") LocalDateTime currentTime);
    
    @Query("SELECT d FROM Delivery d WHERE d.lateDeliveryRisk = 1 AND d.actualDeliveryTime IS NULL")
    List<Delivery> findAtRiskDeliveries();
    
    @Query("SELECT d FROM Delivery d WHERE d.driver.id = :driverId AND d.actualDeliveryTime IS NULL")
    List<Delivery> findActiveDeliveriesByDriver(@Param("driverId") Long driverId);
    
    @Query("SELECT d FROM Delivery d WHERE d.vehicle.id = :vehicleId AND d.actualDeliveryTime IS NULL")
    List<Delivery> findActiveDeliveriesByVehicle(@Param("vehicleId") Long vehicleId);
      
    @Query("SELECT COUNT(d) FROM Delivery d WHERE DATE(d.actualDeliveryTime) = CURRENT_DATE")
    long countTodayDeliveries();
    
    @Query("SELECT d FROM Delivery d WHERE d.deliveryAttempts > :attempts")
    List<Delivery> findDeliveriesWithMultipleAttempts(@Param("attempts") Integer attempts);

}
