package ktc.spring_project.repositories;

import ktc.spring_project.entities.DeliveryTracking;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface DeliveryTrackingRepository extends JpaRepository<DeliveryTracking, Long> {
    
    List<DeliveryTracking> findByVehicleId(Long vehicleId);
    
    List<DeliveryTracking> findByDeliveryId(Long deliveryId);
    
List<DeliveryTracking> findByStatus_Id(Short statusId);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.vehicle.id = :vehicleId ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findByVehicleIdOrderByTimestampDesc(@Param("vehicleId") Long vehicleId);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.vehicle.id = :vehicleId AND dt.timestamp = " +
           "(SELECT MAX(dt2.timestamp) FROM DeliveryTracking dt2 WHERE dt2.vehicle.id = :vehicleId)")
    Optional<DeliveryTracking> findLatestByVehicleId(@Param("vehicleId") Long vehicleId);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.timestamp BETWEEN :startTime AND :endTime ORDER BY dt.timestamp")
    List<DeliveryTracking> findTrackingInTimeRange(@Param("startTime") LocalDateTime startTime, 
                                                 @Param("endTime") LocalDateTime endTime);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.latitude BETWEEN :minLat AND :maxLat " +
           "AND dt.longitude BETWEEN :minLng AND :maxLng")
    List<DeliveryTracking> findTrackingInArea(@Param("minLat") BigDecimal minLat,
                                            @Param("maxLat") BigDecimal maxLat,
                                            @Param("minLng") BigDecimal minLng,
                                            @Param("maxLng") BigDecimal maxLng);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.vehicle.id = :vehicleId " +
           "AND dt.timestamp BETWEEN :startTime AND :endTime ORDER BY dt.timestamp")
    List<DeliveryTracking> findVehicleTrackingInTimeRange(@Param("vehicleId") Long vehicleId,
                                                         @Param("startTime") LocalDateTime startTime,
                                                         @Param("endTime") LocalDateTime endTime);
    
    @Query("SELECT COUNT(dt) FROM DeliveryTracking dt WHERE dt.vehicle.id = :vehicleId")
    long countTrackingPointsByVehicle(@Param("vehicleId") Long vehicleId);
    
    @Query("SELECT DISTINCT dt.vehicle.id FROM DeliveryTracking dt WHERE dt.timestamp >= :since")
    List<Long> findActiveVehicleIdsSince(@Param("since") LocalDateTime since);
    
    // Delivery-specific methods
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.delivery.id = :deliveryId ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findByDeliveryIdOrderByTimestampDesc(@Param("deliveryId") Long deliveryId);
    
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.delivery.id = :deliveryId AND dt.timestamp = " +
           "(SELECT MAX(dt2.timestamp) FROM DeliveryTracking dt2 WHERE dt2.delivery.id = :deliveryId)")
    Optional<DeliveryTracking> findLatestByDeliveryId(@Param("deliveryId") Long deliveryId);
    
    @Query("SELECT COUNT(dt) FROM DeliveryTracking dt WHERE dt.delivery.id = :deliveryId")
    long countTrackingPointsByDelivery(@Param("deliveryId") Long deliveryId);

    // Find tracking by vehicle and delivery
    @Query("SELECT dt FROM DeliveryTracking dt WHERE dt.vehicle.id = :vehicleId AND dt.delivery.id = :deliveryId ORDER BY dt.timestamp DESC")
    List<DeliveryTracking> findByVehicleIdAndDeliveryId(@Param("vehicleId") Long vehicleId, @Param("deliveryId") Long deliveryId);

    
}

