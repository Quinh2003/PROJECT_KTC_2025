package ktc.spring_project.repositories;

import ktc.spring_project.entities.MaintenanceRequest;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Status;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

@Repository
public interface MaintenanceRequestRepository extends JpaRepository<MaintenanceRequest, Long> {

    // Find all maintenance requests by creator (driver)
    Page<MaintenanceRequest> findByCreatedBy_IdOrderByCreatedAtDesc(Long createdById, Pageable pageable);

    // Find all maintenance requests by vehicle
    Page<MaintenanceRequest> findByVehicleOrderByCreatedAtDesc(Vehicle vehicle, Pageable pageable);

    // Find by status
    Page<MaintenanceRequest> findByStatusOrderByCreatedAtDesc(Status status, Pageable pageable);

    // Find by maintenance type
    Page<MaintenanceRequest> findByMaintenanceTypeOrderByCreatedAtDesc(String maintenanceType, Pageable pageable);

    // Find recent maintenance requests for dashboard
    List<MaintenanceRequest> findTop10ByOrderByCreatedAtDesc();

    // Find by vehicle and date range
    @Query("SELECT m FROM MaintenanceRequest m WHERE m.vehicle = :vehicle AND m.createdAt BETWEEN :startDate AND :endDate ORDER BY m.createdAt DESC")
    List<MaintenanceRequest> findByVehicleAndDateRange(@Param("vehicle") Vehicle vehicle, 
                                                       @Param("startDate") Timestamp startDate, 
                                                       @Param("endDate") Timestamp endDate);

    // Find pending maintenance requests
    @Query("SELECT m FROM MaintenanceRequest m JOIN m.status s WHERE s.name = 'PENDING' ORDER BY m.createdAt DESC")
    List<MaintenanceRequest> findPendingMaintenanceRequests();

    // Find maintenance requests by creator ID
    List<MaintenanceRequest> findByCreatedBy_Id(Long createdById);

    // Count maintenance requests by status
    @Query("SELECT COUNT(m) FROM MaintenanceRequest m JOIN m.status s WHERE s.name = :statusName")
    Long countByStatusName(@Param("statusName") String statusName);

    // Find maintenance requests with cost greater than amount
    @Query("SELECT m FROM MaintenanceRequest m WHERE m.cost > :amount ORDER BY m.cost DESC")
    List<MaintenanceRequest> findMaintenanceRequestsWithCostGreaterThan(@Param("amount") Double amount);

    // Find overdue maintenance requests (next due date has passed)
    @Query("SELECT m FROM MaintenanceRequest m WHERE m.nextDueDate < CURRENT_TIMESTAMP ORDER BY m.nextDueDate ASC")
    List<MaintenanceRequest> findOverdueMaintenanceRequests();

    // Search maintenance requests by description or maintenance type
    @Query("SELECT m FROM MaintenanceRequest m WHERE LOWER(m.description) LIKE LOWER(CONCAT('%', :keyword, '%')) OR LOWER(m.maintenanceType) LIKE LOWER(CONCAT('%', :keyword, '%')) ORDER BY m.createdAt DESC")
    Page<MaintenanceRequest> searchMaintenanceRequests(@Param("keyword") String keyword, Pageable pageable);
}