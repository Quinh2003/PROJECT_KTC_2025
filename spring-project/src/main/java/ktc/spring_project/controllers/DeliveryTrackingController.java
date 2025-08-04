package ktc.spring_project.controllers;

import ktc.spring_project.entities.DeliveryTracking;
import ktc.spring_project.services.TrackingService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * Controller responsible for managing delivery tracking
 * Based on user stories:
 * - US-ORDER-TRACK-01: Real-Time Order Tracking
 * - US-DRIVER-STATUS-UPDATE-01: Update Delivery Status
 * - US-MAP-REALTIME-01: 3D Real-Time Vehicle Visualization
 */
@RestController
@RequestMapping("/api/tracking")
public class DeliveryTrackingController {

    @Autowired
    private TrackingService trackingService;

    @Autowired
    private UserService userService;

    /**
     * Update vehicle location and status
     * US-DRIVER-STATUS-UPDATE-01
     */
    @PostMapping("/location")
    public ResponseEntity<DeliveryTracking> updateLocation(
            @Valid @RequestBody Map<String, Object> locationData,
            Authentication authentication) {

        Long vehicleId = Long.valueOf(locationData.get("vehicleId").toString());
        Double latitude = Double.valueOf(locationData.get("latitude").toString());
        Double longitude = Double.valueOf(locationData.get("longitude").toString());
        Long statusId = Long.valueOf(locationData.get("statusId").toString());
        String location = (String) locationData.get("location");
        String notes = (String) locationData.get("notes");

        DeliveryTracking tracking = trackingService.updateVehicleLocation(
                vehicleId, latitude, longitude, statusId, location, notes);

        return new ResponseEntity<>(tracking, HttpStatus.CREATED);
    }

    /**
     * Get tracking history for a vehicle
     * US-ORDER-TRACK-01
     */
    @GetMapping("/vehicle/{vehicleId}/history")
    public ResponseEntity<List<DeliveryTracking>> getVehicleTrackingHistory(
            @PathVariable Long vehicleId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(defaultValue = "50") int limit) {

        List<DeliveryTracking> trackingHistory = trackingService.getVehicleTrackingHistory(
                vehicleId, dateFrom, dateTo, limit);

        return ResponseEntity.ok(trackingHistory);
    }

    /**
     * Get current location of a specific vehicle
     * US-MAP-REALTIME-01
     */
    @GetMapping("/vehicle/{vehicleId}/current")
    public ResponseEntity<DeliveryTracking> getCurrentVehicleLocation(@PathVariable Long vehicleId) {
        DeliveryTracking currentLocation = trackingService.getCurrentVehicleLocation(vehicleId);
        return ResponseEntity.ok(currentLocation);
    }

    /**
     * Get all active vehicle locations for real-time map
     * US-MAP-REALTIME-01
     */
    @GetMapping("/active-vehicles")
    public ResponseEntity<List<Map<String, Object>>> getActiveVehicleLocations(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String vehicleType) {

        List<Map<String, Object>> activeLocations = trackingService.getActiveVehicleLocations(status, vehicleType);
        return ResponseEntity.ok(activeLocations);
    }

    /**
     * Get delivery route tracking for an order
     * US-MAP-DETAIL-02
     */
    @GetMapping("/order/{orderId}/route")
    public ResponseEntity<Map<String, Object>> getOrderRouteTracking(@PathVariable Long orderId) {
        Map<String, Object> routeData = trackingService.getOrderRouteTracking(orderId);
        return ResponseEntity.ok(routeData);
    }

    /**
     * Get tracking statistics
     */
    @GetMapping("/statistics")
    public ResponseEntity<Map<String, Object>> getTrackingStatistics(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) Long vehicleId) {

        Map<String, Object> statistics = trackingService.getTrackingStatistics(dateFrom, dateTo, vehicleId);
        return ResponseEntity.ok(statistics);
    }

    /**
     * Bulk update multiple vehicle locations
     */
    @PostMapping("/bulk-location")
    public ResponseEntity<List<DeliveryTracking>> bulkUpdateLocations(
            @Valid @RequestBody List<Map<String, Object>> locationDataList,
            Authentication authentication) {

        List<DeliveryTracking> updatedTrackings = trackingService.bulkUpdateLocations(locationDataList);
        return ResponseEntity.ok(updatedTrackings);
    }
}
