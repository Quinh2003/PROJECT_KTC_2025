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
 * Controller responsible for managing deliveries
 * Based on database schema for deliveries table and user stories for delivery management
 */
@RestController
@RequestMapping("/api/deliveries")
public class DeliveryController {

    // Assume these services are implemented later
    @Autowired
    private DeliveryService deliveryService;

    @Autowired
    private UserService userService;

    @Autowired
    private VehicleService vehicleService;

    @Autowired
    private TrackingService trackingService;

    /**
     * Get all deliveries with optional filters
     */
    @GetMapping
    public ResponseEntity<List<Map<String, Object>>> getAllDeliveries(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Long driverId,
            @RequestParam(required = false) Long vehicleId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        List<Map<String, Object>> deliveries = deliveryService.getFilteredDeliveries(
                status, driverId, vehicleId, dateFrom, dateTo);

        return ResponseEntity.ok(deliveries);
    }

    /**
     * Get delivery by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Map<String, Object>> getDeliveryById(@PathVariable Long id) {
        Map<String, Object> delivery = deliveryService.getDeliveryById(id);
        return ResponseEntity.ok(delivery);
    }

    /**
     * Create a new delivery
     */
    @PostMapping
    public ResponseEntity<Map<String, Object>> createDelivery(
            @Valid @RequestBody Map<String, Object> deliveryData,
            Authentication authentication) {

        Map<String, Object> createdDelivery = deliveryService.createDelivery(deliveryData, authentication);
        return new ResponseEntity<>(createdDelivery, HttpStatus.CREATED);
    }

    /**
     * Update delivery information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Map<String, Object>> updateDelivery(
            @PathVariable Long id,
            @Valid @RequestBody Map<String, Object> deliveryData,
            Authentication authentication) {

        Map<String, Object> updatedDelivery = deliveryService.updateDelivery(id, deliveryData, authentication);
        return ResponseEntity.ok(updatedDelivery);
    }

    /**
     * Assign driver to delivery
     */
    @PatchMapping("/{id}/assign-driver")
    public ResponseEntity<Map<String, Object>> assignDriverToDelivery(
            @PathVariable Long id,
            @RequestBody Map<String, Long> driverData,
            Authentication authentication) {

        Long driverId = driverData.get("driverId");

        Map<String, Object> updatedDelivery = deliveryService.assignDriverToDelivery(id, driverId, authentication);
        return ResponseEntity.ok(updatedDelivery);
    }

    /**
     * Assign vehicle to delivery
     */
    @PatchMapping("/{id}/assign-vehicle")
    public ResponseEntity<Map<String, Object>> assignVehicleToDelivery(
            @PathVariable Long id,
            @RequestBody Map<String, Long> vehicleData,
            Authentication authentication) {

        Long vehicleId = vehicleData.get("vehicleId");

        Map<String, Object> updatedDelivery = deliveryService.assignVehicleToDelivery(id, vehicleId, authentication);
        return ResponseEntity.ok(updatedDelivery);
    }

    /**
     * Update delivery status
     */
    @PatchMapping("/{id}/status")
    public ResponseEntity<Map<String, Object>> updateDeliveryStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Object> statusData,
            Authentication authentication) {

        String status = (String) statusData.get("status");
        String notes = (String) statusData.get("notes");

        Map<String, Object> updatedDelivery = deliveryService.updateDeliveryStatus(id, status, notes, authentication);
        return ResponseEntity.ok(updatedDelivery);
    }

    /**
     * Get delivery tracking information
     */
    @GetMapping("/{id}/tracking")
    public ResponseEntity<List<Map<String, Object>>> getDeliveryTracking(@PathVariable Long id) {
        List<Map<String, Object>> trackingData = trackingService.getDeliveryTrackingHistory(id);
        return ResponseEntity.ok(trackingData);
    }

    /**
     * Calculate optimal route for delivery
     */
    @PostMapping("/{id}/calculate-route")
    public ResponseEntity<Map<String, Object>> calculateOptimalRoute(
            @PathVariable Long id,
            @RequestBody(required = false) Map<String, Object> routeOptions,
            Authentication authentication) {

        Map<String, Object> routeData = deliveryService.calculateOptimalRoute(id, routeOptions, authentication);
        return ResponseEntity.ok(routeData);
    }

    /**
     * Get delivery stats by vehicle type
     */
    @GetMapping("/stats/by-vehicle-type")
    public ResponseEntity<List<Map<String, Object>>> getStatsByVehicleType(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        List<Map<String, Object>> stats = deliveryService.getStatsByVehicleType(dateFrom, dateTo);
        return ResponseEntity.ok(stats);
    }

    /**
     * Get delivery stats by service type
     */
    @GetMapping("/stats/by-service-type")
    public ResponseEntity<List<Map<String, Object>>> getStatsByServiceType(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        List<Map<String, Object>> stats = deliveryService.getStatsByServiceType(dateFrom, dateTo);
        return ResponseEntity.ok(stats);
    }

    /**
     * Get deliveries at risk of late delivery
     */
    @GetMapping("/at-risk")
    public ResponseEntity<List<Map<String, Object>>> getDeliveriesAtRisk() {
        List<Map<String, Object>> atRiskDeliveries = deliveryService.getDeliveriesAtRisk();
        return ResponseEntity.ok(atRiskDeliveries);
    }
}
