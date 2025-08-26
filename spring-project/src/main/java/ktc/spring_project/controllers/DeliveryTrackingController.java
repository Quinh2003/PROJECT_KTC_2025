package ktc.spring_project.controllers;
import ktc.spring_project.services.DeliveryService;

import ktc.spring_project.entities.DeliveryTracking;

import ktc.spring_project.services.DeliveryTrackingService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.VehicleService;
import ktc.spring_project.services.StatusService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
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

    // DTO for returning simple tracking info
    public static class TrackingPointDTO {
        public Long id;
        public Double latitude;
        public Double longitude;
        public Long vehicleId;
        public String timestamp;
        public TrackingPointDTO() {}
        public TrackingPointDTO(DeliveryTracking tracking) {
            this.id = tracking.getId();
            this.latitude = tracking.getLatitude() != null ? tracking.getLatitude().doubleValue() : null;
            this.longitude = tracking.getLongitude() != null ? tracking.getLongitude().doubleValue() : null;
            this.vehicleId = tracking.getVehicle() != null ? tracking.getVehicle().getId() : null;
            this.timestamp = tracking.getTimestamp() != null ? tracking.getTimestamp().toString() : null;
        }
    }

    @Autowired
    private DeliveryTrackingService deliveryTrackingService;

    @Autowired
    private UserService userService;

    @Autowired
    private VehicleService vehicleService;

    @Autowired
    private StatusService statusService;

    @Autowired
    private DeliveryService deliveryService;

    /**
     * Update vehicle location and status
     * US-DRIVER-STATUS-UPDATE-01
     * TO-DO: Implement updateVehicleLocation method in DeliveryTrackingService
     */
    @PostMapping("/location")
    public ResponseEntity<DeliveryTracking> updateLocation(
            @Valid @RequestBody Map<String, Object> locationData,
            Authentication authentication) {
        DeliveryTracking tracking = new DeliveryTracking();
        tracking.setLatitude(new BigDecimal(locationData.get("latitude").toString()));
        tracking.setLongitude(new BigDecimal(locationData.get("longitude").toString()));
        tracking.setLocation((String) locationData.get("location"));
        tracking.setNotes((String) locationData.get("notes"));
        tracking.setTimestamp(Timestamp.from(Instant.now()));

        // Gán vehicle nếu có vehicleId
        if (locationData.containsKey("vehicleId")) {
            Long vehicleId = Long.valueOf(locationData.get("vehicleId").toString());
            tracking.setVehicle(vehicleService.getVehicleById(vehicleId));
        }
        // Gán status nếu có statusId
        if (locationData.containsKey("statusId")) {
            Short statusId = Short.valueOf(locationData.get("statusId").toString());
            statusService.getStatusById(statusId).ifPresent(tracking::setStatus);
        }
        // Gán delivery nếu có deliveryId
        if (locationData.containsKey("deliveryId")) {
            Long deliveryId = Long.valueOf(locationData.get("deliveryId").toString());
            try {
                tracking.setDelivery(deliveryService.getDeliveryById(deliveryId));
            } catch (Exception e) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                        .body(null); // hoặc trả về message lỗi deliveryId không tồn tại
            }
        }
        DeliveryTracking saved = deliveryTrackingService.save(tracking);
        return new ResponseEntity<>(saved, HttpStatus.CREATED);
    }

    /**
     * Get tracking history for a vehicle
     * US-ORDER-TRACK-01
     * TO-DO: Implement getVehicleTrackingHistory method in DeliveryTrackingService
     */
    @GetMapping("/vehicle/{vehicleId}/history")
    public ResponseEntity<List<DeliveryTracking>> getVehicleTrackingHistory(
            @PathVariable Long vehicleId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(defaultValue = "50") int limit) {

        // TO-DO: This endpoint needs implementation in the service layer
        // Will be integrated with tracking history database and possible AI analytics
        return ResponseEntity.ok(new ArrayList<>());
    }

    /**
     * Get current location of a specific vehicle
     * US-MAP-REALTIME-01
     * TO-DO: Implement getCurrentVehicleLocation method in DeliveryTrackingService
     */
    @GetMapping("/vehicle/{vehicleId}/current")
    public ResponseEntity<TrackingPointDTO> getCurrentVehicleLocation(@PathVariable Long vehicleId) {
        DeliveryTracking tracking = deliveryTrackingService.findLatestByVehicleId(vehicleId).orElse(null);
        if (tracking == null) {
            return ResponseEntity.notFound().build();
        }
        TrackingPointDTO dto = new TrackingPointDTO();
        dto.id = tracking.getId();
        dto.latitude = tracking.getLatitude() != null ? tracking.getLatitude().doubleValue() : null;
        dto.longitude = tracking.getLongitude() != null ? tracking.getLongitude().doubleValue() : null;
        dto.vehicleId = (tracking.getVehicle() != null) ? tracking.getVehicle().getId() : null;
        dto.timestamp = (tracking.getTimestamp() != null) ? tracking.getTimestamp().toString() : null;
        return ResponseEntity.ok(dto);
    }

    /**
     * Get all active vehicle locations for real-time map
     * US-MAP-REALTIME-01
     * TO-DO: Integrate with real-time tracking and AI prediction system
     * Future implementation will connect to an external AI service for predictive tracking
     */
    @GetMapping("/active-vehicles")
    public ResponseEntity<List<Map<String, Object>>> getActiveVehicleLocations(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String vehicleType) {

        // TO-DO: This is a temporary implementation.
        // In the future, this will be integrated with a real-time tracking system
        // and AI-based prediction for more accurate vehicle positioning
        // External AI model will be deployed to a separate server and called from here

        // Simple hardcoded response for development purposes
        return ResponseEntity.ok(List.of(
            Map.of(
                "vehicleId", 1L,
                "licensePlate", "59A-12345",
                "vehicleType", "Motorcycle",
                "driverId", 101L,
                "latitude", 10.772903,
                "longitude", 106.698824,
                "message", "This endpoint will be integrated with a real-time tracking system"
            )
        ));
    }

    /**
     * Get order route tracking for an order
     * US-MAP-DETAIL-02
     * TO-DO: Implement getOrderRouteTracking method in DeliveryTrackingService
     * Future implementation will use AI for route optimization and ETA prediction
     */
    @GetMapping("/order/{orderId}/route")
    public ResponseEntity<Map<String, Object>> getOrderRouteTracking(@PathVariable Long orderId) {
        // TO-DO: This is a temporary implementation
        // Future implementation will connect to an external AI service for route optimization
        Map<String, Object> routeData = new HashMap<>();
        routeData.put("orderId", orderId);
        routeData.put("startPoint", Map.of(
            "name", "Warehouse Quận 1",
            "address", "123 Nguyễn Huệ, Quận 1, TP.HCM",
            "coordinates", Map.of("lat", 10.772903, "lng", 106.698824)
        ));
        routeData.put("endPoint", Map.of(
            "name", "Customer Location",
            "address", "456 Điện Biên Phủ, Quận 3, TP.HCM",
            "coordinates", Map.of("lat", 10.779693, "lng", 106.684228)
        ));
        routeData.put("currentLocation", Map.of(
            "coordinates", Map.of("lat", 10.775987, "lng", 106.689987),
            "updatedAt", System.currentTimeMillis(),
            "status", "In Transit"
        ));
        routeData.put("waypoints", List.of(
            Map.of("lat", 10.772903, "lng", 106.698824, "name", "Warehouse"),
            Map.of("lat", 10.774562, "lng", 106.694532, "name", "Checkpoint 1"),
            Map.of("lat", 10.776890, "lng", 106.692341, "name", "Checkpoint 2"),
            Map.of("lat", 10.779693, "lng", 106.684228, "name", "Destination")
        ));
        routeData.put("estimatedDistance", 3.8); // km
        routeData.put("estimatedDuration", 18); // minutes
        routeData.put("actualDistance", 2.2); // km traversed so far

        return ResponseEntity.ok(routeData);
    }

    /**
     * Get tracking statistics
     * TO-DO: Implement getTrackingStatistics method in DeliveryTrackingService
     * Future implementation will use AI for predictive analytics
     */
    @GetMapping("/statistics")
    public ResponseEntity<Map<String, Object>> getTrackingStatistics(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) Long vehicleId) {

        // TO-DO: This is a temporary implementation
        // Future implementation will connect to an external AI service for analytics
        Map<String, Object> statistics = new HashMap<>();

        // Thông tin tổng hợp
        statistics.put("totalDistanceCovered", 5678.9); // km
        statistics.put("averageDeliveryTime", 42.3); // minutes
        statistics.put("totalDeliveries", 358);
        statistics.put("completedDeliveries", 312);
        statistics.put("inProgressDeliveries", 46);

        // Thống kê theo loại phương tiện
        statistics.put("vehicleTypeStats", List.of(
            Map.of(
                "vehicleType", "Motorcycle",
                "totalVehicles", 25,
                "activeVehicles", 18,
                "averageSpeed", 35.2, // km/h
                "fuelEfficiency", 2.8 // lít/100km
            ),
            Map.of(
                "vehicleType", "Car",
                "totalVehicles", 15,
                "activeVehicles", 10,
                "averageSpeed", 40.5,
                "fuelEfficiency", 7.2
            ),
            Map.of(
                "vehicleType", "Truck",
                "totalVehicles", 8,
                "activeVehicles", 5,
                "averageSpeed", 32.1,
                "fuelEfficiency", 12.5
            )
        ));

        // Thống kê theo khu vực
        statistics.put("regionStats", List.of(
            Map.of(
                "region", "Quận 1",
                "deliveries", 78,
                "averageDeliveryTime", 38.2
            ),
            Map.of(
                "region", "Quận 2",
                "deliveries", 56,
                "averageDeliveryTime", 45.7
            ),
            Map.of(
                "region", "Quận 3",
                "deliveries", 67,
                "averageDeliveryTime", 41.3
            )
        ));

        // Nếu có vehicleId, thêm thông tin chi tiết của phương tiện đó
        if (vehicleId != null) {
            statistics.put("vehicleDetails", Map.of(
                "id", vehicleId,
                "licensePlate", "59A-12345",
                "totalDistance", 1234.5,
                "deliveriesCompleted", 45,
                "averageDeliveryTime", 39.2,
                "fuelUsed", 120.8
            ));
        }

        return ResponseEntity.ok(statistics);
    }

    /**
     * Bulk update multiple vehicle locations
     * TO-DO: Implement bulkUpdateLocations method in DeliveryTrackingService
     */
    @PostMapping("/bulk-location")
    public ResponseEntity<List<DeliveryTracking>> bulkUpdateLocations(
            @Valid @RequestBody List<Map<String, Object>> locationDataList,
            Authentication authentication) {

        // TO-DO: This is a temporary implementation
        // Future implementation will process bulk updates more efficiently
        List<DeliveryTracking> updatedTrackings = new ArrayList<>();

        // Process each location data
        for (Map<String, Object> locationData : locationDataList) {
            DeliveryTracking tracking = new DeliveryTracking();
            tracking.setLatitude(new BigDecimal(locationData.get("latitude").toString()));
            tracking.setLongitude(new BigDecimal(locationData.get("longitude").toString()));
            tracking.setLocation((String) locationData.get("location"));
            tracking.setNotes((String) locationData.get("notes"));
            tracking.setTimestamp(Timestamp.from(Instant.now()));

            // TO-DO: Need to implement proper vehicle and status lookup
            // Future implementation will look up the vehicle and status properly

            // Save and add to result list
            updatedTrackings.add(deliveryTrackingService.save(tracking));
        }

        return ResponseEntity.ok(updatedTrackings);
    }
}