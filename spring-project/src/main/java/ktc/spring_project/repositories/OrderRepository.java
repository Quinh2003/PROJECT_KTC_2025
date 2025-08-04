package ktc.spring_project.repositories;

import ktc.spring_project.entities.Order;
import ktc.spring_project.enums.TransportMode;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface OrderRepository extends JpaRepository<Order, Long> {
    
    Optional<Order> findByOrderCode(String orderCode);
    
    List<Order> findByCustomerId(Long customerId);
    
    List<Order> findByDriverId(Long driverId);
    
    List<Order> findByVehicleId(Long vehicleId);
    
    List<Order> findByStatusId(Long statusId);
    
    List<Order> findByTransportMode(TransportMode transportMode);
    
    @Query("SELECT o FROM Order o WHERE o.createdAt BETWEEN :startDate AND :endDate")
    List<Order> findByDateRange(@Param("startDate") LocalDateTime startDate, 
                               @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT o FROM Order o WHERE o.status.code = :statusCode")
    List<Order> findByStatusCode(@Param("statusCode") String statusCode);
    
    @Query("SELECT o FROM Order o WHERE o.recipientPhone = :phone")
    List<Order> findByRecipientPhone(@Param("phone") String phone);
    
    @Query("SELECT o FROM Order o WHERE o.deliveryAddress LIKE %:address%")
    List<Order> findByDeliveryAddress(@Param("address") String address);
    
    boolean existsByOrderCode(String orderCode);
}
