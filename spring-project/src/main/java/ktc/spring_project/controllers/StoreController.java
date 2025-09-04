package ktc.spring_project.controllers;

import ktc.spring_project.entities.Store;
import ktc.spring_project.services.StoreService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;

import jakarta.validation.Valid;
import java.util.ArrayList;
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
    public ResponseEntity<List<Store>> getAllStores(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) String region) {

        // Get all stores from service
        List<Store> allStores = storeService.getAllStores();

        // Manual filtering since getFilteredStores is not implemented in service
        // TO-DO: Implement proper filtering in StoreService
        if (status != null || search != null || region != null) {
            List<Store> filteredStores = new ArrayList<>();

            for (Store store : allStores) {
                boolean statusMatch = true;
                boolean searchMatch = true;
                boolean regionMatch = true;

                // Filter by status if provided
                if (status != null) {
                    boolean isActiveStatus = "1".equals(status) || "true".equalsIgnoreCase(status);
                    statusMatch = (store.getIsActive() == isActiveStatus);
                }

                // Filter by search term if provided
                if (search != null && !search.isEmpty()) {
                    String searchLower = search.toLowerCase();
                    searchMatch = store.getStoreName().toLowerCase().contains(searchLower) ||
                            (store.getAddress() != null && store.getAddress().toLowerCase().contains(searchLower));
                }

                // Region filtering would be implemented here
                // Currently just passing through since region field might not exist in entity

                // Add to filtered list if all conditions match
                if (statusMatch && searchMatch && regionMatch) {
                    filteredStores.add(store);
                }
            }

            return ResponseEntity.ok(filteredStores);
        }

        // Return all stores if no filters applied
        return ResponseEntity.ok(allStores);
    }

    /**
     * Get store by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Store> getStoreById(@PathVariable Long id) {
        Store store = storeService.getStoreById(id);
        return ResponseEntity.ok(store);
    }

    /**
     * Create a new store
     */
    @PostMapping
    public ResponseEntity<Store> createStore(
            @Valid @RequestBody ktc.spring_project.dtos.store.CreateStoreRequestDTO dto,
            Authentication authentication) {

        Store store = new Store();
        store.setStoreName(dto.getStoreName());
        store.setEmail(dto.getEmail());
        store.setPhone(dto.getPhone());
        store.setAddress(dto.getAddress());
        if (dto.getLatitude() != null) {
            store.setLatitude(java.math.BigDecimal.valueOf(dto.getLatitude()));
        }
        if (dto.getLongitude() != null) {
            store.setLongitude(java.math.BigDecimal.valueOf(dto.getLongitude()));
        }
        store.setIsActive(dto.getIsActive());
        store.setNotes(dto.getNotes());

        Store createdStore = storeService.createStore(store);
        return new ResponseEntity<>(createdStore, HttpStatus.CREATED);
    }

    /**
     * Update store information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Store> updateStore(
            @PathVariable Long id,
            @Valid @RequestBody Store storeDetails,
            Authentication authentication) {

        // TO-DO: Add authentication handling once implemented in service
        Store updatedStore = storeService.updateStore(id, storeDetails);
        return ResponseEntity.ok(updatedStore);
    }

    /**
     * Delete store (soft delete)
     */

    /**
 * Delete store (soft delete)
 */

    /**
     * Get orders for a store
     * TO-DO: Implement getStoreOrders method in StoreService
     */
    @GetMapping("/{id}/orders")
    public ResponseEntity<List<Map<String, Object>>> getStoreOrders(
            @PathVariable Long id,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        // TO-DO: Replace with actual implementation when getStoreOrders is implemented in StoreService
        // List<Map<String, Object>> orders = storeService.getStoreOrders(id, status, dateFrom, dateTo, page, size);

        return ResponseEntity.ok(List.of(
                Map.of(
                        "message", "Store orders listing will be implemented in a future update",
                        "storeId", id,
                        "parameters", Map.of(
                                "status", status != null ? status : "",
                                "dateFrom", dateFrom != null ? dateFrom : "",
                                "dateTo", dateTo != null ? dateTo : "",
                                "page", page,
                                "size", size
                        )
                )
        ));
    }

    /**
     * Get store performance statistics
     * TO-DO: Implement getStoreStatistics method in StoreService
     */
    @GetMapping("/{id}/statistics")
    public ResponseEntity<Map<String, Object>> getStoreStatistics(
            @PathVariable Long id,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        // TO-DO: Replace with actual implementation when getStoreStatistics is implemented in StoreService
        // Map<String, Object> statistics = storeService.getStoreStatistics(id, dateFrom, dateTo);

        return ResponseEntity.ok(Map.of(
                "message", "Store statistics will be implemented in a future update",
                "storeId", id,
                "dateRange", Map.of(
                        "from", dateFrom != null ? dateFrom : "",
                        "to", dateTo != null ? dateTo : ""
                )
        ));
    }

    /**
     * Get nearby stores
     * TO-DO: Implement getNearbyStores method in StoreService
     */
    @GetMapping("/nearby")
    public ResponseEntity<List<Map<String, Object>>> getNearbyStores(
            @RequestParam Double latitude,
            @RequestParam Double longitude,
            @RequestParam(defaultValue = "10") Double radiusKm) {

        // TO-DO: Replace with actual implementation when getNearbyStores is implemented in StoreService
        // List<Map<String, Object>> nearbyStores = storeService.getNearbyStores(latitude, longitude, radiusKm);

        return ResponseEntity.ok(List.of(
                Map.of(
                        "message", "Nearby stores search will be implemented in a future update",
                        "parameters", Map.of(
                                "latitude", latitude,
                                "longitude", longitude,
                                "radiusKm", radiusKm
                        )
                )
        ));
    }
@DeleteMapping("/{id}")
public ResponseEntity<Void> deleteStore(
        @PathVariable Long id,
        Authentication authentication) {
    storeService.deleteStore(id);
    return ResponseEntity.noContent().build();
}

/**
 * Update store status (active/inactive)
 * TO-DO: Implement updateStoreStatus method in StoreService
 */
@PatchMapping("/{id}/status")
    public ResponseEntity<Map<String, Object>> updateStoreStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Boolean> statusData,
            Authentication authentication) {

        Boolean isActive = statusData.get("isActive");

        // TO-DO: Replace with actual implementation when updateStoreStatus is implemented in StoreService
        // Map<String, Object> updatedStore = storeService.updateStoreStatus(id, isActive, authentication);

        // Temporary implementation using existing service method
        Store store = storeService.getStoreById(id);
        store.setIsActive(isActive);
        Store updatedStore = storeService.updateStore(id, store);

        return ResponseEntity.ok(Map.of(
                "id", updatedStore.getId(),
                "storeName", updatedStore.getStoreName(),
                "isActive", updatedStore.getIsActive(),
                "message", "Store status updated successfully"
        ));
    }

    @PatchMapping("/{id}")
public ResponseEntity<Store> patchStore(
        @PathVariable Long id,
        @RequestBody Map<String, Object> updates,
        Authentication authentication) {

    Store store = storeService.getStoreById(id);
    if (updates.containsKey("storeName")) {
        store.setStoreName((String) updates.get("storeName"));
    }
    // Thêm các trường khác nếu cần PATCH

    Store updatedStore = storeService.updateStore(id, store);
    return ResponseEntity.ok(updatedStore);
}

}
