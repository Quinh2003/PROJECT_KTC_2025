package ktc.spring_project.services;

import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.repositories.VehicleRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class VehicleService {

    @Autowired
    private VehicleRepository vehicleRepository;

    public Vehicle createVehicle(Vehicle vehicle) {
        if (vehicle.getCapacityWeightKg() != null && vehicle.getCapacityWeightKg().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Capacity weight cannot be negative");
        }
        if (vehicle.getCapacityVolumeM3() != null && vehicle.getCapacityVolumeM3().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Capacity volume cannot be negative");
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

    public Vehicle updateVehicle(Long id, Vehicle vehicleDetails) {
        Vehicle vehicle = getVehicleById(id);
        if (vehicle.getCapacityWeightKg() != null && vehicle.getCapacityWeightKg().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Capacity weight cannot be negative");
        }
        if (vehicle.getCapacityVolumeM3() != null && vehicle.getCapacityVolumeM3().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Capacity volume cannot be negative");
        }
        vehicle.setLicensePlate(vehicleDetails.getLicensePlate());
        vehicle.setVehicleType(vehicleDetails.getVehicleType());
        vehicle.setCapacityWeightKg(vehicleDetails.getCapacityWeightKg());
        vehicle.setCapacityVolumeM3(vehicleDetails.getCapacityVolumeM3());
        vehicle.setStatusId(vehicleDetails.getStatusId());
        vehicle.setCurrentDriverId(vehicleDetails.getCurrentDriverId());
        vehicle.setNotes(vehicleDetails.getNotes());
        return vehicleRepository.save(vehicle);
    }

    public void deleteVehicle(Long id) {
        Vehicle vehicle = getVehicleById(id);
        vehicleRepository.delete(vehicle);
    }
}