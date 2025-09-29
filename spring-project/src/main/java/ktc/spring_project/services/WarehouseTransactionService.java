package ktc.spring_project.services;

import ktc.spring_project.entities.WarehouseTransaction;
import ktc.spring_project.repositories.WarehouseTransactionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class WarehouseTransactionService {

    @Autowired
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

    /**
     * Get warehouse transactions for a specific product
     *
     * @param productId The product ID to filter transactions
     * @param dateFrom Optional start date filter
     * @param dateTo Optional end date filter
     * @param transactionType Optional transaction type filter (IN, OUT, TRANSFER)
     * @return List of warehouse transactions matching the criteria
     */
    public List<WarehouseTransaction> getProductTransactions(
            Long productId,
            String dateFrom,
            String dateTo,
            String transactionType) {
        // This is a placeholder implementation
        // In a real implementation, you would use repository methods with appropriate filters
        // For example: warehouseTransactionRepository.findByProductIdAndDateRange(productId, dateFrom, dateTo)

        // TODO: Implement proper repository query when repository is updated
        List<WarehouseTransaction> allTransactions = warehouseTransactionRepository.findAll();
        List<WarehouseTransaction> filteredTransactions = new ArrayList<>();

        // Filter logic would go here in a real implementation
        // For now, we'll just return all transactions related to the productId
        for (WarehouseTransaction transaction : allTransactions) {
            if (transaction.getProduct().equals(productId)) {
                // Apply additional filters here (date range, transaction type)
                filteredTransactions.add(transaction);
            }
        }

        return filteredTransactions;
    }
}
