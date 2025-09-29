
package ktc.spring_project.services;

import ktc.spring_project.entities.VehicleMaintenance;
import ktc.spring_project.repositories.VehicleMaintenanceRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class VehicleMaintenanceService {
    @Autowired
    private VehicleMaintenanceRepository repository;

    public List<VehicleMaintenance> findAll() {
        return repository.findAll();
    }

    public Optional<VehicleMaintenance> findById(Long id) {
        return repository.findById(id);
    }

    public VehicleMaintenance save(VehicleMaintenance entity) {
        return repository.save(entity);
    }

    public void deleteById(Long id) {
        repository.deleteById(id);
    }

    public VehicleMaintenance updateFields(VehicleMaintenance oldEntity, VehicleMaintenance newEntity) {
        if (newEntity.getVehicle() != null) oldEntity.setVehicle(newEntity.getVehicle());
        if (newEntity.getMaintenanceDate() != null) oldEntity.setMaintenanceDate(newEntity.getMaintenanceDate());
        if (newEntity.getNextDueDate() != null) oldEntity.setNextDueDate(newEntity.getNextDueDate());
        if (newEntity.getMaintenanceType() != null) oldEntity.setMaintenanceType(newEntity.getMaintenanceType());
        if (newEntity.getDescription() != null) oldEntity.setDescription(newEntity.getDescription());
        if (newEntity.getCost() != null) oldEntity.setCost(newEntity.getCost());
        if (newEntity.getStatus() != null) oldEntity.setStatus(newEntity.getStatus());
        if (newEntity.getCreatedBy() != null) oldEntity.setCreatedBy(newEntity.getCreatedBy());
        if (newEntity.getNotes() != null) oldEntity.setNotes(newEntity.getNotes());
        // ... các trường khác nếu có ...
        return repository.save(oldEntity);
    }

    /**
     * Count pending/active maintenance requests only (not completed ones)
     */
    public long countMaintenanceRequests() {
        // Chỉ đếm những yêu cầu bảo trì chưa hoàn thành
        // Giả sử status có thể là: "Scheduled", "In Progress", "Pending", etc.
        // Không đếm "Completed", "Cancelled"
        return repository.countByStatusNotIn(List.of("Completed", "Cancelled"));
    }
}
