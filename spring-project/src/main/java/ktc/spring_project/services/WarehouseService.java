package ktc.spring_project.services;

import ktc.spring_project.entities.Warehouse;
import ktc.spring_project.repositories.WarehouseRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class WarehouseService {

    @Autowired
    private WarehouseRepository warehouseRepository;

    public Warehouse createWarehouse(Warehouse warehouse) {
        return warehouseRepository.save(warehouse);
    }

    public Warehouse getWarehouseById(Long id) {
        return warehouseRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Warehouse not found with id: " + id));
    }

    public List<Warehouse> getAllWarehouses() {
        return warehouseRepository.findAll();
    }

    public Warehouse updateWarehouse(Long id, Warehouse warehouseDetails) {
        Warehouse warehouse = getWarehouseById(id);
        warehouse.setWarehouseCode(warehouseDetails.getWarehouseCode());
        warehouse.setName(warehouseDetails.getName());
        warehouse.setAddress(warehouseDetails.getAddress());
        warehouse.setLatitude(warehouseDetails.getLatitude());
        warehouse.setLongitude(warehouseDetails.getLongitude());
        warehouse.setCapacityM3(warehouseDetails.getCapacityM3());
        warehouse.setIsActive(warehouseDetails.getIsActive());
        warehouse.setCreatedBy(warehouseDetails.getCreatedBy());
        warehouse.setNotes(warehouseDetails.getNotes());
        return warehouseRepository.save(warehouse);
    }

    public void deleteWarehouse(Long id) {
        Warehouse warehouse = getWarehouseById(id);
        warehouseRepository.delete(warehouse);
    }
}