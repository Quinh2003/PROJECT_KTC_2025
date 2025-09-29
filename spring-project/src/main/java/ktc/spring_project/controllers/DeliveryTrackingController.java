package ktc.spring_project.controllers;
import ktc.spring_project.services.DeliveryService;
import ktc.spring_project.services.ChecklistService;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.Order;
import java.util.Optional;
import ktc.spring_project.dtos.ChecklistProgressResponse;

import ktc.spring_project.entities.DeliveryTracking;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.repositories.DeliveryRepository;

import ktc.spring_project.services.DeliveryTrackingService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.VehicleService;
import ktc.spring_project.services.StatusService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import org.springframework.transaction.annotation.Transactional;

import jakarta.validation.Valid;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import lombok.extern.slf4j.Slf4j;
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
@Slf4j
public class DeliveryTrackingController {

    // DTO for returning simple tracking info
    public static class TrackingPointDTO {
        public Long id;
        public Double latitude;
        public Double longitude;
        public Long vehicleId;
        public String timestamp;
        public String location;
        public String notes;
        public TrackingPointDTO() {}
        public TrackingPointDTO(DeliveryTracking tracking) {
            this.id = tracking.getId();
            this.latitude = tracking.getLatitude() != null ? tracking.getLatitude().doubleValue() : null;
            this.longitude = tracking.getLongitude() != null ? tracking.getLongitude().doubleValue() : null;
            this.vehicleId = tracking.getVehicle() != null ? tracking.getVehicle().getId() : null;
            this.timestamp = tracking.getTimestamp() != null ? tracking.getTimestamp().toString() : null;
            this.location = tracking.getLocation();
            this.notes = tracking.getNotes();
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

    @Autowired
    private DeliveryRepository deliveryRepository;

    @Autowired
    private ChecklistService checklistService;

    /**
     * Simple vehicle location update - chỉ cần vehicleId, latitude, longitude
     * Endpoint đơn giản để lưu vị trí xe mà không cần delivery_id
     */
    @PostMapping("/vehicle-location")
    @Transactional
    public ResponseEntity<?> updateVehicleLocation(
            @Valid @RequestBody Map<String, Object> locationData,
            Authentication authentication) {
        try {
            // Kiểm tra các trường bắt buộc
            if (!locationData.containsKey("vehicleId") || 
                !locationData.containsKey("latitude") || 
                !locationData.containsKey("longitude") ||
                !locationData.containsKey("deliveryId")) {
                return ResponseEntity.badRequest()
                    .body(Map.of("error", "vehicleId, latitude, longitude, deliveryId are required"));
            }

            DeliveryTracking tracking = new DeliveryTracking();
            tracking.setLatitude(new BigDecimal(locationData.get("latitude").toString()));
            tracking.setLongitude(new BigDecimal(locationData.get("longitude").toString()));
            tracking.setTimestamp(Timestamp.from(Instant.now()));

            // Set vehicle
            Long vehicleId = Long.valueOf(locationData.get("vehicleId").toString());
            tracking.setVehicle(vehicleService.getVehicleById(vehicleId));

            // Set delivery
            Long deliveryId = Long.valueOf(locationData.get("deliveryId").toString());
            tracking.setDelivery(deliveryService.getDeliveryById(deliveryId));

            // Set optional fields
            if (locationData.containsKey("location")) {
                tracking.setLocation((String) locationData.get("location"));
            }
            if (locationData.containsKey("notes")) {
                tracking.setNotes((String) locationData.get("notes"));
            }

            // Set default status nếu cần
            statusService.getStatusById((short) 1).ifPresent(tracking::setStatus);

            log.info("🔍 About to save tracking: vehicleId={}, deliveryId={}, lat={}, lng={}", 
                vehicleId, deliveryId, tracking.getLatitude(), tracking.getLongitude());
            
            DeliveryTracking saved = deliveryTrackingService.save(tracking);
            
            log.info("✅ Tracking saved successfully: id={}, vehicleId={}, deliveryId={}", 
                saved.getId(), saved.getVehicle().getId(), saved.getDelivery().getId());
            
            return ResponseEntity.ok(Map.of(
                "success", true,
                "message", "Vehicle location updated successfully",
                "trackingId", saved.getId(),
                "vehicleId", vehicleId,
                "latitude", saved.getLatitude(),
                "longitude", saved.getLongitude(),
                "timestamp", saved.getTimestamp()
            ));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(Map.of("error", "Failed to save vehicle location: " + e.getMessage()));
        }
    }

    @GetMapping("/{trackingId}")
    public ResponseEntity<?> getTrackingById(@PathVariable Long trackingId) {
        try {
            DeliveryTracking tracking = deliveryTrackingService.findById(trackingId)
                .orElse(null);
            if (tracking == null) {
                return ResponseEntity.notFound().build();
            }
            // Trả về DTO chi tiết, bao gồm các trường lồng nhau cần thiết
            TrackingDetailDTO dto = new TrackingDetailDTO(tracking);
            return ResponseEntity.ok(dto);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(Map.of("error", "Failed to get tracking: " + e.getMessage()));
        }
    }

    // DTO chi tiết cho tracking, bao gồm các trường lồng nhau
    public static class TrackingDetailDTO {
        public Long id;
        public Double latitude;
        public Double longitude;
        public String location;
        public String notes;
        public Long vehicleId;
        public VehicleInfo vehicle;
        public Long deliveryId;
        public DeliveryInfo delivery;
        public StatusInfo status;
        public String timestamp;
        public String createdAt;
        public String updatedAt;
        public Integer statusId;

        public TrackingDetailDTO(DeliveryTracking t) {
            this.id = t.getId();
            this.latitude = t.getLatitude() != null ? t.getLatitude().doubleValue() : null;
            this.longitude = t.getLongitude() != null ? t.getLongitude().doubleValue() : null;
            this.location = t.getLocation();
            this.notes = t.getNotes();
            this.vehicleId = t.getVehicle() != null ? t.getVehicle().getId() : null;
            this.vehicle = t.getVehicle() != null ? new VehicleInfo(t.getVehicle()) : null;
            this.deliveryId = t.getDelivery() != null ? t.getDelivery().getId() : null;
            this.delivery = t.getDelivery() != null ? new DeliveryInfo(t.getDelivery()) : null;
            this.status = t.getStatus() != null ? new StatusInfo(t.getStatus()) : null;
            this.timestamp = t.getTimestamp() != null ? t.getTimestamp().toString() : null;
            this.createdAt = t.getCreatedAt() != null ? t.getCreatedAt().toString() : null;
            this.updatedAt = t.getUpdatedAt() != null ? t.getUpdatedAt().toString() : null;
            this.statusId = t.getStatusId();
        }
        // Nested DTOs
        public static class VehicleInfo {
            public Long id;
            public String licensePlate;
            public String vehicleType;
            public String model;
            public Double capacityWeightKg;
            public Double capacityVolumeM3;
            public VehicleInfo(ktc.spring_project.entities.Vehicle v) {
                this.id = v.getId();
                this.licensePlate = v.getLicensePlate();
                this.vehicleType = v.getVehicleType() != null ? v.getVehicleType().toString() : null;
                this.model = v.getModel();
                this.capacityWeightKg = v.getCapacityWeightKg() != null ? v.getCapacityWeightKg().doubleValue() : null;
                this.capacityVolumeM3 = v.getCapacityVolumeM3() != null ? v.getCapacityVolumeM3().doubleValue() : null;
            }
        }
        public static class DeliveryInfo {
            public Long id;
            public String code;
            public String notes;
            public Long orderId;
            public DeliveryInfo(ktc.spring_project.entities.Delivery d) {
                this.id = d.getId();
                // Sửa lại getter cho code nếu không phải getCode()
                this.notes = d.getDeliveryNotes();
                // Lấy orderId từ entity Delivery
                this.orderId = d.getOrder() != null ? d.getOrder().getId() : null;
            }
        }
        public static class StatusInfo {
            public Short id;
            public String name;
            public String description;
            public StatusInfo(ktc.spring_project.entities.Status s) {
                this.id = s.getId();
                this.name = s.getName();
                this.description = s.getDescription();
            }
        }
    }

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
     * Get current tracking for a delivery
     * Fetch tracking data by delivery_id from delivery_tracking table
     */
    @GetMapping("/delivery/{deliveryId}/current")
    public ResponseEntity<TrackingPointDTO> getCurrentDeliveryLocation(@PathVariable Long deliveryId) {
        try {
            // Kiểm tra delivery có tồn tại không
            if (!deliveryRepository.existsById(deliveryId)) {
                return ResponseEntity.notFound().build();
            }

            // Tìm tracking record mới nhất cho delivery này
            List<DeliveryTracking> trackingList = deliveryTrackingService.findByDeliveryId(deliveryId);
            
            if (trackingList == null || trackingList.isEmpty()) {
                return ResponseEntity.notFound().build();
            }

            // Lấy tracking record mới nhất (theo timestamp)
            DeliveryTracking latestTracking = trackingList.stream()
                .filter(t -> t.getTimestamp() != null)
                .max((t1, t2) -> t1.getTimestamp().compareTo(t2.getTimestamp()))
                .orElse(trackingList.get(trackingList.size() - 1));

            TrackingPointDTO dto = new TrackingPointDTO(latestTracking);
            return ResponseEntity.ok(dto);
        } catch (Exception e) {
            log.error("Error getting current delivery location for deliveryId {}: {}", deliveryId, e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Get current tracking for an order
     * Fetch tracking data by order_id via delivery relationship
     */
    @GetMapping("/order/{orderId}/current")
    public ResponseEntity<TrackingPointDTO> getCurrentOrderLocation(@PathVariable Long orderId) {
        try {
            // Tìm delivery cho order này
            List<Delivery> deliveries = deliveryService.findByOrderId(orderId);
            if (deliveries == null || deliveries.isEmpty()) {
                return ResponseEntity.notFound().build();
            }

            // Lấy delivery đầu tiên (mỗi order thường có 1 delivery)
            Delivery delivery = deliveries.get(0);

            // Delegate to delivery tracking
            return getCurrentDeliveryLocation(delivery.getId());
        } catch (Exception e) {
            log.error("Error getting current order location for orderId {}: {}", orderId, e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
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

    /**
     * Check if tracking exists for vehicle and delivery
     */
    @GetMapping("/vehicle/{vehicleId}/delivery/{deliveryId}")
    public ResponseEntity<TrackingPointDTO> getTrackingByVehicleAndDelivery(
            @PathVariable Long vehicleId, 
            @PathVariable Long deliveryId) {
        try {
            // Tìm tracking record theo vehicleId và deliveryId
            List<DeliveryTracking> trackings = deliveryTrackingService.findByVehicleIdAndDeliveryId(vehicleId, deliveryId);
            
            if (trackings != null && !trackings.isEmpty()) {
                // Lấy tracking mới nhất
                DeliveryTracking latest = trackings.get(trackings.size() - 1);
                TrackingPointDTO dto = new TrackingPointDTO(latest);
                return ResponseEntity.ok(dto);
            } else {
                return ResponseEntity.notFound().build();
            }
        } catch (Exception e) {
            log.error("Error finding tracking for vehicle {} and delivery {}: {}", vehicleId, deliveryId, e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Update existing tracking record
     */
    @PutMapping("/vehicle-location/{trackingId}")
    @Transactional
    public ResponseEntity<Map<String, Object>> updateExistingVehicleLocation(
            @PathVariable Long trackingId,
            @RequestBody Map<String, Object> locationData) {
        try {
            // Tìm tracking record hiện tại
            DeliveryTracking existingTracking = deliveryTrackingService.findById(trackingId)
                .orElseThrow(() -> new RuntimeException("Tracking record not found with ID: " + trackingId));

            // Cập nhật thông tin mới
            existingTracking.setLatitude(new BigDecimal(locationData.get("latitude").toString()));
            existingTracking.setLongitude(new BigDecimal(locationData.get("longitude").toString()));
            existingTracking.setTimestamp(Timestamp.from(Instant.now()));

            if (locationData.containsKey("location")) {
                existingTracking.setLocation((String) locationData.get("location"));
            }
            if (locationData.containsKey("notes")) {
                existingTracking.setNotes((String) locationData.get("notes"));
            }
            if (locationData.containsKey("statusId")) {
                existingTracking.setStatusId(Integer.valueOf(locationData.get("statusId").toString()));
            }

            log.info("🔄 Updating existing tracking: id={}, vehicleId={}, deliveryId={}, lat={}, lng={}", 
                trackingId, existingTracking.getVehicle().getId(), existingTracking.getDelivery().getId(),
                existingTracking.getLatitude(), existingTracking.getLongitude());

            DeliveryTracking updated = deliveryTrackingService.save(existingTracking);
            
            log.info("✅ Tracking updated successfully: id={}", updated.getId());

                    // Ghi log checklist step DRIVER_START_DELIVERY
                    Delivery delivery = updated.getDelivery();
                    if (delivery != null && delivery.getOrder() != null && updated.getVehicle() != null && updated.getVehicle().getCurrentDriver() != null) {
                        Long driverId = updated.getVehicle().getCurrentDriver().getId();
                        Long orderId = delivery.getOrder().getId();
                        String stepCode = null;
                        if (locationData.containsKey("stepCode")) {
                            stepCode = locationData.get("stepCode").toString();
                        }

                        // Xử lý logic trạng thái theo stepCode
                        if ("DRIVER_RECEIVE_ORDER".equals(stepCode)) {
                            // Chuyển trạng thái sang Shipped
                            Optional<ktc.spring_project.entities.Status> shippedStatus = statusService.getStatusByTypeAndName("ORDER", "Shipped");
                            if (shippedStatus.isPresent()) {
                                delivery.getOrder().setStatus(shippedStatus.get());
                                deliveryService.save(delivery);
                            }
                            checklistService.markStepCompleted(driverId, orderId, "DRIVER_RECEIVE_ORDER", "Driver received order (order shipped)");
                        } else if ("DRIVER_DELIVERED".equals(stepCode)) {
                            // Chuyển trạng thái sang Delivered
                            Optional<ktc.spring_project.entities.Status> deliveredStatus = statusService.getStatusByTypeAndName("ORDER", "Delivered");
                            if (deliveredStatus.isPresent()) {
                                delivery.getOrder().setStatus(deliveredStatus.get());
                                deliveryService.save(delivery);
                            }
                            checklistService.markStepCompleted(driverId, orderId, "DRIVER_DELIVERED", "Order delivered (customer chưa thanh toán)");
                        }

                        // Trả về log checklist progress cho order
                        ChecklistProgressResponse checklistLog = checklistService.getProgressByOrder(orderId);
                        Map<String, Object> result = new java.util.HashMap<>();
                        result.put("success", true);
                        result.put("message", "Vehicle location updated and checklist log updated");
                        result.put("trackingId", updated.getId());
                        result.put("vehicleId", updated.getVehicle().getId());
                        result.put("deliveryId", updated.getDelivery().getId());
                        result.put("latitude", updated.getLatitude());
                        result.put("longitude", updated.getLongitude());
                        result.put("timestamp", updated.getTimestamp());
                        result.put("location", updated.getLocation());
                        result.put("notes", updated.getNotes());
                        result.put("statusId", updated.getStatusId());
                        result.put("checklistLog", checklistLog);
                        // Removed stepName reference (only use stepCode)
                        result.put("orderStatus", delivery.getOrder().getStatus() != null ? delivery.getOrder().getStatus().getName() : null);
                        return ResponseEntity.ok(result);
                } else {
                    Map<String, Object> result = new java.util.HashMap<>();
                    result.put("success", true);
                    result.put("message", "Vehicle location updated successfully, but checklist log not updated (missing driver/order info)");
                    result.put("trackingId", updated.getId());
                    result.put("vehicleId", updated.getVehicle() != null ? updated.getVehicle().getId() : null);
                    result.put("deliveryId", updated.getDelivery() != null ? updated.getDelivery().getId() : null);
                    result.put("latitude", updated.getLatitude());
                    result.put("longitude", updated.getLongitude());
                    result.put("timestamp", updated.getTimestamp());
                    result.put("location", updated.getLocation());
                    result.put("notes", updated.getNotes());
                    result.put("statusId", updated.getStatusId());
                    return ResponseEntity.ok(result);
                }
        } catch (Exception e) {
            log.error("Error updating tracking record {}: {}", trackingId, e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(Map.of("error", "Failed to update vehicle location: " + e.getMessage()));
        }
    }
}