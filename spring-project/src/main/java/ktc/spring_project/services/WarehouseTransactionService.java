package ktc.spring_project.serviceư;

import ktc.spring_project.entities.InventoryTransaction;
import ktc.spring_project.repository.InventoryTransactionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class InventoryTransactionService {
    private final InventoryTransactionRepository inventoryTransactionRepository;

    public List<InventoryTransaction> findAll() {
        return inventoryTransactionRepository.findAll();
    }

    public Optional<InventoryTransaction> findById(Long id) {
        return inventoryTransactionRepository.findById(id);
    }

    public InventoryTransaction save(InventoryTransaction entities) {
        return inventoryTransactionRepository.save(entities);
    }

    public void delete(Long id) {
        inventoryTransactionRepository.deleteById(id);
    }
}
