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
        return vehicleRepository.save(vehicle);
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
    return vehicleRepository.save(vehicle);
    }

    public void deleteVehicle(Long id) {
        Vehicle vehicle = getVehicleById(id);
        vehicleRepository.delete(vehicle);
    }
}