package ktc.spring_project.services;

import ktc.spring_project.entities.WarehouseTransaction;
import ktc.spring_project.repositories.WarehouseTransactionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class WarehouseTransactionService {

    private WarehouseTransactionRepository warehouseTransactionRepository;

    public List<WarehouseTransaction> findAll() {
        return warehouseTransactionRepository.findAll();
    }

    public Optional<WarehouseTransaction> findById(Long id) {
        return warehouseTransactionRepository.findById(id);
    }

    public WarehouseTransaction save(WarehouseTransaction entities) {
        return warehouseTransactionRepository.save(entities);
    }

    public void delete(Long id) {
        warehouseTransactionRepository.deleteById(id);
    }
}
