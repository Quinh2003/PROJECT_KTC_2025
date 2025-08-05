package ktc.spring_project.repositories;

import ktc.spring_project.entities.Order;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface OrderRepository extends JpaRepository<Order, Long> {
    
    Optional<Order> findByOrderId(String orderId);
    
    List<Order> findByStatusId(Short statusId);
    
    List<Order> findByStoreId(Long storeId);
    
    List<Order> findByCreatedBy(Long createdBy);

    List<Order> findActiveOrdersByDriverId(Long driverId);

    List<Order> findAvailableOrdersForVehicle(BigDecimal weightCapacity, BigDecimal volumeCapacity, Long vehicleId);
    
    @Query("SELECT o FROM Order o WHERE o.status.name = :statusName ORDER BY o.createdAt DESC")
    List<Order> findByStatusNameOrderByCreatedAtDesc(@Param("statusName") String statusName);
    
    @Query("SELECT o FROM Order o WHERE o.createdAt BETWEEN :startDate AND :endDate ORDER BY o.createdAt DESC")
    List<Order> findOrdersBetweenDates(@Param("startDate") LocalDateTime startDate, 
                                     @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT o FROM Order o WHERE o.totalAmount BETWEEN :minAmount AND :maxAmount")
    List<Order> findByTotalAmountRange(@Param("minAmount") BigDecimal minAmount, 
                                     @Param("maxAmount") BigDecimal maxAmount);
    
    @Query("SELECT o FROM Order o WHERE o.store.storeCode = :storeCode ORDER BY o.createdAt DESC")
    List<Order> findByStoreCodeOrderByCreatedAtDesc(@Param("storeCode") String storeCode);
    
    @Query("SELECT COUNT(o) FROM Order o WHERE o.status.name = :statusName")
    long countByStatusName(@Param("statusName") String statusName);
    
    @Query("SELECT SUM(o.totalAmount) FROM Order o WHERE o.status.name = 'COMPLETED' " +
           "AND o.createdAt BETWEEN :startDate AND :endDate")
    BigDecimal getTotalRevenueInPeriod(@Param("startDate") LocalDateTime startDate, 
                                     @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT COUNT(o) FROM Order o WHERE DATE(o.createdAt) = CURRENT_DATE")
    long countTodayOrders();

    @Query("SELECT COUNT(o) FROM Order o WHERE o.driver.id = :driverId AND o.status = 'DELIVERED' AND DATE(o.deliveryDate) = CURRENT_DATE")
    int countDeliveredOrdersByDriverIdToday(Long driverId);
}

