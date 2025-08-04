package ktc.spring_project.controllers;

import ktc.spring_project.entities.Warehouse;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.WarehouseService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * Controller responsible for managing warehouses and warehouse transactions
 * Based on database schema for warehouses and warehouse_transactions tables
 */
@RestController
@RequestMapping("/api/warehouses")
public class WarehouseController {

    @Autowired
    private WarehouseService warehouseService;

    @Autowired
    private UserService userService;

    /**
     * Get all warehouses with optional filters
     */
    @GetMapping
    public ResponseEntity<List<Warehouse>> getAllWarehouses(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String search) {

        List<Warehouse> warehouses = warehouseService.getFilteredWarehouses(status, search);
        return ResponseEntity.ok(warehouses);
    }

    /**
     * Get warehouse by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Warehouse> getWarehouseById(@PathVariable Long id) {
        Warehouse warehouse = warehouseService.getWarehouseById(id);
        return ResponseEntity.ok(warehouse);
    }

    /**
     * Create new warehouse
     */
    @PostMapping
    public ResponseEntity<Warehouse> createWarehouse(
            @Valid @RequestBody Warehouse warehouse,
            Authentication authentication) {

        Warehouse createdWarehouse = warehouseService.createWarehouse(warehouse, authentication);
        return new ResponseEntity<>(createdWarehouse, HttpStatus.CREATED);
    }

    /**
     * Update warehouse information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Warehouse> updateWarehouse(
            @PathVariable Long id,
            @Valid @RequestBody Warehouse warehouse,
            Authentication authentication) {

        Warehouse updatedWarehouse = warehouseService.updateWarehouse(id, warehouse, authentication);
        return ResponseEntity.ok(updatedWarehouse);
    }

    /**
     * Delete warehouse (soft delete)
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteWarehouse(
            @PathVariable Long id,
            Authentication authentication) {

        warehouseService.deleteWarehouse(id, authentication);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get warehouse inventory
     */
    @GetMapping("/{id}/inventory")
    public ResponseEntity<List<Map<String, Object>>> getWarehouseInventory(
            @PathVariable Long id,
            @RequestParam(required = false) String productCategory,
            @RequestParam(required = false) String productStatus) {

        List<Map<String, Object>> inventory = warehouseService.getWarehouseInventory(
                id, productCategory, productStatus);
        return ResponseEntity.ok(inventory);
    }

    /**
     * Get warehouse capacity utilization
     */
    @GetMapping("/{id}/capacity")
    public ResponseEntity<Map<String, Object>> getWarehouseCapacity(@PathVariable Long id) {
        Map<String, Object> capacity = warehouseService.getWarehouseCapacity(id);
        return ResponseEntity.ok(capacity);
    }

    /**
     * Get transactions for warehouse
     */
    @GetMapping("/{id}/transactions")
    public ResponseEntity<List<Map<String, Object>>> getWarehouseTransactions(
            @PathVariable Long id,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String transactionType) {

        List<Map<String, Object>> transactions = warehouseService.getWarehouseTransactions(
                id, dateFrom, dateTo, transactionType);
        return ResponseEntity.ok(transactions);
    }

    /**
     * Get all warehouse transactions with optional filters
     */
    @GetMapping("/transactions")
    public ResponseEntity<List<Map<String, Object>>> getAllWarehouseTransactions(
            @RequestParam(required = false) String transactionType,
            @RequestParam(required = false) Long productId,
            @RequestParam(required = false) Long warehouseId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        List<Map<String, Object>> transactions = warehouseService.getFilteredTransactions(
                transactionType, productId, warehouseId, dateFrom, dateTo);

        return ResponseEntity.ok(transactions);
    }

    /**
     * Get transaction by ID
     */
    @GetMapping("/transactions/{id}")
    public ResponseEntity<Map<String, Object>> getTransactionById(@PathVariable Long id) {
        Map<String, Object> transaction = warehouseService.getTransactionById(id);
        return ResponseEntity.ok(transaction);
    }

    /**
     * Create a new warehouse transaction
     */
    @PostMapping("/transactions")
    public ResponseEntity<Map<String, Object>> createTransaction(
            @Valid @RequestBody Map<String, Object> transaction,
            Authentication authentication) {

        Map<String, Object> createdTransaction = warehouseService.createTransaction(transaction, authentication);
        return new ResponseEntity<>(createdTransaction, HttpStatus.CREATED);
    }

    /**
     * Get low stock products
     */
    @GetMapping("/low-stock")
    public ResponseEntity<List<Map<String, Object>>> getLowStockProducts(
            @RequestParam(defaultValue = "10") int threshold,
            @RequestParam(required = false) Long warehouseId) {

        List<Map<String, Object>> lowStockProducts = warehouseService.getLowStockProducts(threshold, warehouseId);
        return ResponseEntity.ok(lowStockProducts);
    }

    /**
     * Create a stock transfer between warehouses
     */
    @PostMapping("/transfer")
    public ResponseEntity<Map<String, Object>> createStockTransfer(
            @Valid @RequestBody Map<String, Object> transferData,
            Authentication authentication) {

        Map<String, Object> result = warehouseService.createStockTransfer(transferData, authentication);
        return new ResponseEntity<>(result, HttpStatus.CREATED);
    }

    /**
     * Get inventory movement report
     */
    @GetMapping("/movement")
    public ResponseEntity<List<Map<String, Object>>> getInventoryMovementReport(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) Long productId,
            @RequestParam(required = false) Long warehouseId) {

        List<Map<String, Object>> movementReport = warehouseService.getInventoryMovementReport(
                dateFrom, dateTo, productId, warehouseId);
        return ResponseEntity.ok(movementReport);
    }

    /**
     * Adjust inventory quantity
     */
    @PostMapping("/adjust")
    public ResponseEntity<Map<String, Object>> adjustInventory(
            @Valid @RequestBody Map<String, Object> adjustmentData,
            Authentication authentication) {

        Map<String, Object> adjustment = warehouseService.adjustInventory(adjustmentData, authentication);
        return new ResponseEntity<>(adjustment, HttpStatus.CREATED);
    }

    /**
     * Get inventory valuation report
     */
    @GetMapping("/valuation")
    public ResponseEntity<Map<String, Object>> getInventoryValuation(
            @RequestParam(required = false) Long warehouseId) {

        Map<String, Object> valuation = warehouseService.getInventoryValuation(warehouseId);
        return ResponseEntity.ok(valuation);
    }

    /**
     * Get inventory turnover report
     */
    @GetMapping("/turnover")
    public ResponseEntity<List<Map<String, Object>>> getInventoryTurnover(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) Long categoryId) {

        List<Map<String, Object>> turnoverReport = warehouseService.getInventoryTurnover(dateFrom, dateTo, categoryId);
        return ResponseEntity.ok(turnoverReport);
    }
}
