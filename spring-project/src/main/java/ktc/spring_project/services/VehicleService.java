package ktc.spring_project.services;

import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.repositories.VehicleRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import ktc.spring_project.exceptions.VehicleInvalidCapacityException;

@Service
public class VehicleService {

    @Autowired
    private VehicleRepository vehicleRepository;

    public Vehicle createVehicle(Vehicle vehicle) {
        // Kiểm tra capacity weight/volume không được âm
        if (vehicle.getCapacityWeightKg() != null && vehicle.getCapacityWeightKg().compareTo(BigDecimal.ZERO) < 0) {
            throw new VehicleInvalidCapacityException("Capacity weight cannot be negative");
        }
        if (vehicle.getCapacityVolumeM3() != null && vehicle.getCapacityVolumeM3().compareTo(BigDecimal.ZERO) < 0) {
            throw new VehicleInvalidCapacityException("Capacity volume cannot be negative");
        }
        // Kiểm tra trùng lặp biển số xe
        if (vehicleRepository.findByLicensePlate(vehicle.getLicensePlate()).isPresent()) {
            throw new ktc.spring_project.exceptions.EntityDuplicateException("License plate");
        }
        
        Vehicle savedVehicle = vehicleRepository.save(vehicle);
        
        // Broadcast vehicle metrics update
        broadcastVehicleMetricsUpdate();
        
        return savedVehicle;
    }

    public Vehicle getVehicleById(Long id) {
        return vehicleRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Vehicle not found with id: " + id));
    }

    public List<Vehicle> getAllVehicles() {
        return vehicleRepository.findAll();
    }

    public List<Vehicle> getAllVehiclesSorted() {
        return vehicleRepository.findAll(Sort.by("updatedAt").descending());
    }

    public Page<Vehicle> getVehiclesPaginated(int page, int size) {
        Pageable pageable = PageRequest.of(page - 1, size, Sort.by("updatedAt").descending());
        return vehicleRepository.findAll(pageable);
    }

    public Vehicle updateVehicle(Long id, Vehicle vehicleDetails) {
    Vehicle vehicle = getVehicleById(id);
    vehicle.setLicensePlate(vehicleDetails.getLicensePlate());
    vehicle.setVehicleType(vehicleDetails.getVehicleType());
    vehicle.setCapacityWeightKg(vehicleDetails.getCapacityWeightKg());
    vehicle.setCapacityVolumeM3(vehicleDetails.getCapacityVolumeM3());
    vehicle.setStatus(vehicleDetails.getStatus());
    vehicle.setCurrentDriver(vehicleDetails.getCurrentDriver());
    vehicle.setNotes(vehicleDetails.getNotes());
    
    Vehicle updatedVehicle = vehicleRepository.save(vehicle);
    
    // Broadcast vehicle metrics update
    broadcastVehicleMetricsUpdate();
    
    return updatedVehicle;
    }

    public void deleteVehicle(Long id) {
        Vehicle vehicle = getVehicleById(id);
        vehicleRepository.delete(vehicle);
        
        // Broadcast vehicle metrics update
        broadcastVehicleMetricsUpdate();
    }

    /**
     * Đếm số xe theo status_id
     */
    public long countVehiclesByStatusId(Byte statusId) {
        return vehicleRepository.countByStatusId(statusId);
    }

    /**
     * Đếm tổng số xe
     */
    public long countTotalVehicles() {
        return vehicleRepository.count();
    }

    /**
     * Lấy thống kê xe đang sử dụng (status_id = 18)
     */
    public VehicleStatsDto getActiveVehicleStats() {
        long activeCount = countVehiclesByStatusId((byte) 18); // IN_USE status
        long totalCount = countTotalVehicles();
        double percentage = totalCount > 0 ? (double) activeCount / totalCount * 100 : 0;
        
        return new VehicleStatsDto(activeCount, totalCount, percentage);
    }

    // Inner class cho vehicle stats
    public static class VehicleStatsDto {
        private long active;
        private long total;
        private double percentage;
        
        public VehicleStatsDto(long active, long total, double percentage) {
            this.active = active;
            this.total = total;
            this.percentage = percentage;
        }
        
        // Getters
        public long getActive() { return active; }
        public long getTotal() { return total; }
        public double getPercentage() { return percentage; }
        public String getRatio() { return active + "/" + total; }
    }

    /**
     * Broadcast vehicle metrics update via SSE
     */
    private void broadcastVehicleMetricsUpdate() {
        try {
            VehicleStatsDto stats = getActiveVehicleStats();
            
            // SSE removed - vehicle metrics now updated manually via refresh
            System.out.println("Vehicle metrics calculated: " + stats.getRatio());
        } catch (Exception e) {
            System.err.println("Error calculating vehicle metrics: " + e.getMessage());
        }
    }
}