package com.ktc.logistics.service;

import com.ktc.logistics.entity.InventoryTransaction;
import com.ktc.logistics.repository.InventoryTransactionRepository;
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

    public InventoryTransaction save(InventoryTransaction entity) {
        return inventoryTransactionRepository.save(entity);
    }

    public void delete(Long id) {
        inventoryTransactionRepository.deleteById(id);
    }
}
