package ktc.spring_project.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * Controller responsible for managing stores
 * Based on database schema for stores table
 */
@RestController
@RequestMapping("/api/stores")
public class StoreController {

    // Assume these services are implemented later
    @Autowired
    private StoreService storeService;

    @Autowired
    private UserService userService;

    /**
     * Get all stores with optional filters
     */
    @GetMapping
    public ResponseEntity<List<Map<String, Object>>> getAllStores(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) String region) {

        List<Map<String, Object>> stores = storeService.getFilteredStores(status, search, region);
        return ResponseEntity.ok(stores);
    }

    /**
     * Get store by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Map<String, Object>> getStoreById(@PathVariable Long id) {
        Map<String, Object> store = storeService.getStoreById(id);
        return ResponseEntity.ok(store);
    }

    /**
     * Create a new store
     */
    @PostMapping
    public ResponseEntity<Map<String, Object>> createStore(
            @Valid @RequestBody Map<String, Object> storeData,
            Authentication authentication) {

        Map<String, Object> createdStore = storeService.createStore(storeData, authentication);
        return new ResponseEntity<>(createdStore, HttpStatus.CREATED);
    }

    /**
     * Update store information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Map<String, Object>> updateStore(
            @PathVariable Long id,
            @Valid @RequestBody Map<String, Object> storeData,
            Authentication authentication) {

        Map<String, Object> updatedStore = storeService.updateStore(id, storeData, authentication);
        return ResponseEntity.ok(updatedStore);
    }

    /**
     * Delete store (soft delete)
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteStore(
            @PathVariable Long id,
            Authentication authentication) {

        storeService.deleteStore(id, authentication);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get orders for a store
     */
    @GetMapping("/{id}/orders")
    public ResponseEntity<List<Map<String, Object>>> getStoreOrders(
            @PathVariable Long id,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        List<Map<String, Object>> orders = storeService.getStoreOrders(id, status, dateFrom, dateTo, page, size);
        return ResponseEntity.ok(orders);
    }

    /**
     * Get store performance statistics
     */
    @GetMapping("/{id}/statistics")
    public ResponseEntity<Map<String, Object>> getStoreStatistics(
            @PathVariable Long id,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        Map<String, Object> statistics = storeService.getStoreStatistics(id, dateFrom, dateTo);
        return ResponseEntity.ok(statistics);
    }

    /**
     * Get nearby stores
     */
    @GetMapping("/nearby")
    public ResponseEntity<List<Map<String, Object>>> getNearbyStores(
            @RequestParam Double latitude,
            @RequestParam Double longitude,
            @RequestParam(defaultValue = "10") Double radiusKm) {

        List<Map<String, Object>> nearbyStores = storeService.getNearbyStores(latitude, longitude, radiusKm);
        return ResponseEntity.ok(nearbyStores);
    }

    /**
     * Update store status (active/inactive)
     */
    @PatchMapping("/{id}/status")
    public ResponseEntity<Map<String, Object>> updateStoreStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Boolean> statusData,
            Authentication authentication) {

        Boolean isActive = statusData.get("isActive");

        Map<String, Object> updatedStore = storeService.updateStoreStatus(id, isActive, authentication);
        return ResponseEntity.ok(updatedStore);
    }
}
