package ktc.spring_project.repositories;

import ktc.spring_project.entities.VehicleMaintenance;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface VehicleMaintenanceRepository extends JpaRepository<VehicleMaintenance, Long> {
    
    /**
     * Count maintenance requests that do not have the specified statuses
     * (i.e., count only pending/active requests, not completed/cancelled ones)
     */
    long countByStatusNotIn(List<String> statuses);
}
