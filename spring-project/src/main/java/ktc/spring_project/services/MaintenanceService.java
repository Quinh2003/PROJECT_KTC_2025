package ktc.spring_project.services;

import ktc.spring_project.entities.VehicleMaintenance;
import ktc.spring_project.repositories.VehicleMaintenanceRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

// 	Quản lý bảo trì xe

@Service
public class MaintenanceService {

    @Autowired
    private VehicleMaintenanceRepository maintenanceRepository;

    public VehicleMaintenance scheduleMaintenance(VehicleMaintenance maintenance) {
        return maintenanceRepository.save(maintenance);
    }

    public List<VehicleMaintenance> getMaintenanceByVehicleId(Long vehicleId) {
        return maintenanceRepository.findByVehicleId(vehicleId);
    }

    public List<VehicleMaintenance> getAllMaintenanceRecords() {
        return maintenanceRepository.findAll();
    }
}