package ktc.spring_project.controllers;

import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.entities.Status;
import ktc.spring_project.services.DeliveryService;
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
import java.util.ArrayList;
import java.util.HashMap;
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
    private DeliveryTrackingService deliveryTrackingService;

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

        // Lấy tất cả giao hàng
        List<Delivery> allDeliveries = deliveryService.getAllDeliveries();

        // Chuyển đổi từ Delivery sang Map để dễ dàng thêm thông tin bổ sung nếu cần
        List<Map<String, Object>> deliveriesMap = new ArrayList<>();
        for (Delivery delivery : allDeliveries) {
            Map<String, Object> deliveryMap = new HashMap<>();
            deliveryMap.put("id", delivery.getId());
            deliveryMap.put("orderId", delivery.getOrder() != null ? delivery.getOrder().getId() : null);
            deliveryMap.put("deliveryFee", delivery.getDeliveryFee());
            deliveryMap.put("transportMode", delivery.getTransportMode());
            deliveryMap.put("serviceType", delivery.getServiceType());
            deliveryMap.put("orderDate", delivery.getOrderDate());
            deliveryMap.put("pickupDate", delivery.getPickupDate());
            deliveryMap.put("scheduleDeliveryTime", delivery.getScheduleDeliveryTime());
            deliveryMap.put("actualDeliveryTime", delivery.getActualDeliveryTime());
            deliveryMap.put("deliveryStatus", delivery.getDeliveryStatus());
            deliveryMap.put("driverId", delivery.getDriver() != null ? delivery.getDriver().getId() : null);
            deliveryMap.put("vehicleId", delivery.getVehicle() != null ? delivery.getVehicle().getId() : null);

            // Lọc theo tiêu chí nếu có
            boolean matchesFilter = true;

            if (status != null && delivery.getDeliveryStatus() != null) {
                matchesFilter = matchesFilter && delivery.getDeliveryStatus().getName().equalsIgnoreCase(status);
            }

            if (driverId != null && delivery.getDriver() != null) {
                matchesFilter = matchesFilter && delivery.getDriver().getId().equals(driverId);
            }

            if (vehicleId != null && delivery.getVehicle() != null) {
                matchesFilter = matchesFilter && delivery.getVehicle().getId().equals(vehicleId);
            }

            // Thêm logic lọc theo ngày nếu cần

            if (matchesFilter) {
                deliveriesMap.add(deliveryMap);
            }
        }

        return ResponseEntity.ok(deliveriesMap);
    }

    /**
     * Get delivery by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Map<String, Object>> getDeliveryById(@PathVariable Long id) {
        // Lấy thông tin giao hàng
        Delivery delivery = deliveryService.getDeliveryById(id);

        // Chuyển đổi thành Map
        Map<String, Object> deliveryMap = new HashMap<>();
        deliveryMap.put("id", delivery.getId());
        deliveryMap.put("orderId", delivery.getOrder() != null ? delivery.getOrder().getId() : null);
        deliveryMap.put("deliveryFee", delivery.getDeliveryFee());
        deliveryMap.put("transportMode", delivery.getTransportMode());
        deliveryMap.put("serviceType", delivery.getServiceType());
        deliveryMap.put("orderDate", delivery.getOrderDate());
        deliveryMap.put("pickupDate", delivery.getPickupDate());
        deliveryMap.put("scheduleDeliveryTime", delivery.getScheduleDeliveryTime());
        deliveryMap.put("actualDeliveryTime", delivery.getActualDeliveryTime());
        deliveryMap.put("deliveryStatus", delivery.getDeliveryStatus() != null ? delivery.getDeliveryStatus().getName() : null);
        deliveryMap.put("driverId", delivery.getDriver() != null ? delivery.getDriver().getId() : null);
        deliveryMap.put("vehicleId", delivery.getVehicle() != null ? delivery.getVehicle().getId() : null);
        deliveryMap.put("deliveryAttempts", delivery.getDeliveryAttempts());
        deliveryMap.put("deliveryNotes", delivery.getDeliveryNotes());

        return ResponseEntity.ok(deliveryMap);
    }

    /**
     * Create a new delivery
     */
    @PostMapping
    public ResponseEntity<Map<String, Object>> createDelivery(
            @Valid @RequestBody Map<String, Object> deliveryData,
            Authentication authentication) {

        // Tạo đối tượng Delivery từ dữ liệu đầu vào
        Delivery delivery = new Delivery();
        // Thiết lập các thuộc tính từ deliveryData
        // Đây là giải pháp tạm thời, trong thực tế cần mapping chi tiết từ Map sang Entity

        // Lưu giao hàng
        Delivery createdDelivery = deliveryService.createDelivery(delivery);

        // Chuyển đổi kết quả thành Map
        Map<String, Object> result = new HashMap<>();
        result.put("id", createdDelivery.getId());
        result.put("message", "Delivery created successfully");

        return new ResponseEntity<>(result, HttpStatus.CREATED);
    }

    /**
     * Update delivery information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Map<String, Object>> updateDelivery(
            @PathVariable Long id,
            @Valid @RequestBody Map<String, Object> deliveryData,
            Authentication authentication) {

        // Lấy thông tin giao hàng hiện tại
        Delivery existingDelivery = deliveryService.getDeliveryById(id);

        // Cập nhật các thuộc tính từ deliveryData
        // Đây là giải pháp tạm thời, trong thực tế cần mapping chi tiết từ Map sang Entity

        // Lưu giao hàng đã cập nhật
        Delivery updatedDelivery = deliveryService.updateDelivery(id, existingDelivery);

        // Chuyển đổi kết quả thành Map
        Map<String, Object> result = new HashMap<>();
        result.put("id", updatedDelivery.getId());
        result.put("message", "Delivery updated successfully");

        return ResponseEntity.ok(result);
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

        // Lấy thông tin giao hàng
        Delivery delivery = deliveryService.getDeliveryById(id);

        // Lấy thông tin tài xế
        User driver = userService.getUserById(driverId);

        // Cập nhật tài xế cho giao hàng
        delivery.setDriver(driver);

        // Lưu giao hàng đã cập nhật
        Delivery updatedDelivery = deliveryService.updateDelivery(id, delivery);

        // Chuyển đổi kết quả thành Map
        Map<String, Object> result = new HashMap<>();
        result.put("id", updatedDelivery.getId());
        result.put("driverId", driverId);
        result.put("message", "Driver assigned successfully");

        return ResponseEntity.ok(result);
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

        // Lấy thông tin giao hàng
        Delivery delivery = deliveryService.getDeliveryById(id);

        // Lấy thông tin phương tiện
        Vehicle vehicle = vehicleService.getVehicleById(vehicleId);

        // Cập nhật phương tiện cho giao hàng
        delivery.setVehicle(vehicle);

        // Lưu giao hàng đã cập nhật
        Delivery updatedDelivery = deliveryService.updateDelivery(id, delivery);

        // Chuyển đổi kết quả thành Map
        Map<String, Object> result = new HashMap<>();
        result.put("id", updatedDelivery.getId());
        result.put("vehicleId", vehicleId);
        result.put("message", "Vehicle assigned successfully");

        return ResponseEntity.ok(result);
    }

    /**
     * Update delivery status
     */
    @PatchMapping("/{id}/status")
    public ResponseEntity<Map<String, Object>> updateDeliveryStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Object> statusData,
            Authentication authentication) {

        String statusName = (String) statusData.get("status");
        String notes = (String) statusData.get("notes");

        // Lấy thông tin giao hàng
        Delivery delivery = deliveryService.getDeliveryById(id);

        // Lấy hoặc tạo mới đối tượng Status dựa trên tên status
        Status status = new Status();
        status.setName(statusName);

        // Cập nhật trạng thái và ghi chú cho giao hàng
        delivery.setDeliveryStatus(status);
        delivery.setDeliveryNotes(notes);

        // Lưu giao hàng đã cập nhật
        Delivery updatedDelivery = deliveryService.updateDelivery(id, delivery);

        // Chuyển đổi kết quả thành Map
        Map<String, Object> result = new HashMap<>();
        result.put("id", updatedDelivery.getId());
        result.put("status", statusName);
        result.put("notes", notes);
        result.put("message", "Delivery status updated successfully");

        return ResponseEntity.ok(result);
    }

    /**
     * Get delivery tracking information
     */
    @GetMapping("/{id}/tracking")
    public ResponseEntity<List<Map<String, Object>>> getDeliveryTracking(@PathVariable Long id) {
        List<Map<String, Object>> trackingData = deliveryTrackingService.getDeliveryTrackingHistory(id);
        return ResponseEntity.ok(trackingData);
    }

    /**
     * Calculate optimal route for delivery
     * TO-DO: Future integration with AI service for real-time route optimization
     */
    @PostMapping("/{id}/calculate-route")
    public ResponseEntity<Map<String, Object>> calculateOptimalRoute(
            @PathVariable Long id,
            @RequestBody(required = false) Map<String, Object> routeOptions,
            Authentication authentication) {

        // TO-DO: This is a temporary implementation.
        // When AI model is deployed to separate server,
        // this endpoint will call the AI service to calculate truly optimal routes
        // based on traffic, distance, weather conditions, and other factors

        // Lấy thông tin giao hàng cơ bản
        Delivery delivery = deliveryService.getDeliveryById(id);

        // Return minimal information instead of sample data
        return ResponseEntity.ok(Map.of(
            "deliveryId", delivery.getId(),
            "message", "Route optimization will be implemented in a future AI integration",
            "requestedOptions", routeOptions != null ? routeOptions : Map.of()
        ));
    }

    /**
     * Get delivery stats by vehicle type
     * TO-DO: Implement analytics service to calculate actual statistics from delivery data
     */
    @GetMapping("/stats/by-vehicle-type")
    public ResponseEntity<List<Map<String, Object>>> getStatsByVehicleType(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        // TO-DO: This is a temporary implementation.
        // In the future, this will analyze actual delivery data and calculate real statistics
        // based on vehicle type, time period, and other relevant factors

        // Return minimal placeholder message instead of sample data
        return ResponseEntity.ok(List.of(
            Map.of(
                "message", "Vehicle type statistics will be implemented in a future update",
                "requestedDateRange", Map.of(
                    "from", dateFrom != null ? dateFrom : "",
                    "to", dateTo != null ? dateTo : ""
                )
            )
        ));
    }

    /**
     * Get delivery stats by service type
     * TO-DO: Implement analytics service to calculate actual statistics from delivery data
     */
    @GetMapping("/stats/by-service-type")
    public ResponseEntity<List<Map<String, Object>>> getStatsByServiceType(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        // TO-DO: This is a temporary implementation.
        // In the future, this will analyze actual delivery data and calculate real statistics
        // based on service type, revenue, customer satisfaction, and other metrics

        // Return minimal placeholder message instead of sample data
        return ResponseEntity.ok(List.of(
            Map.of(
                "message", "Service type statistics will be implemented in a future update",
                "requestedDateRange", Map.of(
                    "from", dateFrom != null ? dateFrom : "",
                    "to", dateTo != null ? dateTo : ""
                )
            )
        ));
    }

    /**
     * Get deliveries at risk of late delivery
     * TO-DO: Future integration with AI service for real-time delivery risk assessment
     */
    @GetMapping("/at-risk")
    public ResponseEntity<List<Map<String, Object>>> getDeliveriesAtRisk() {
        // TO-DO: This is a temporary implementation.
        // When AI model is deployed to separate server,
        // this endpoint will call the AI service to predict deliveries at risk of being late
        // The AI model will analyze factors such as:
        // - Current traffic conditions
        // - Weather patterns
        // - Driver performance history
        // - Route complexity and distance
        // - Package volume and delivery window

        // Simple placeholder response
        return ResponseEntity.ok(List.of(
            Map.of(
                "message", "Delivery risk prediction will be implemented in a future AI integration",
                "description", "This endpoint will identify deliveries that may be delayed based on predictive analysis"
            )
        ));
    }
}
