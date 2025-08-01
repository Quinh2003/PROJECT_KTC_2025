package ktc.spring_project.services;

import ktc.spring_project.entities.Warehouse;
import ktc.spring_project.repository.WarehouseRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class WarehouseService {
    private final WarehouseRepository warehouseRepository;

    public List<Warehouse> findAll() {
        return warehouseRepository.findAll();
    }

    public Optional<Warehouse> findById(Long id) {
        return warehouseRepository.findById(id);
    }

    public Warehouse save(Warehouse entities) {
        return warehouseRepository.save(entities);
    }

    public void delete(Long id) {
        warehouseRepository.deleteById(id);
    }
}
