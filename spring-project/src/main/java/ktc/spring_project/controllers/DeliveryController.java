package ktc.spring_project.controllers;

import ktc.spring_project.entities.Delivery;
import ktc.spring_project.dtos.delivery.CreateDeliveryRequestDTO;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.entities.Status;
import ktc.spring_project.services.DeliveryService;
import ktc.spring_project.services.DeliveryTrackingService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.VehicleService;
import ktc.spring_project.services.StatusService;
import ktc.spring_project.services.RouteService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;

import ktc.spring_project.services.OrderService; // Add this import if missing

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

    @Autowired
    private DeliveryService deliveryService;

    @Autowired
    private UserService userService;

    @Autowired
    private VehicleService vehicleService;

    @Autowired
    private DeliveryTrackingService deliveryTrackingService;

    @Autowired
    private StatusService statusService;

    @Autowired
    private RouteService routeService;

    @Autowired
    private OrderService orderService;

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
            // Order status used instead of non-existent deliveryStatus
            deliveryMap.put("orderStatus", delivery.getOrder() != null && delivery.getOrder().getStatus() != null ?
                delivery.getOrder().getStatus().getName() : null);
            deliveryMap.put("driverId", delivery.getDriver() != null ? delivery.getDriver().getId() : null);
            deliveryMap.put("vehicleId", delivery.getVehicle() != null ? delivery.getVehicle().getId() : null);

            // Lọc theo tiêu chí nếu có
            boolean matchesFilter = true;

            if (status != null && delivery.getOrder() != null && delivery.getOrder().getStatus() != null) {
                matchesFilter = matchesFilter && delivery.getOrder().getStatus().getName().equalsIgnoreCase(status);
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
    // @GetMapping("/{id}")
    // public ResponseEntity<Map<String, Object>> getDeliveryById(@PathVariable Long id) {
    //     // Lấy thông tin giao hàng
    //     Delivery delivery = deliveryService.getDeliveryById(id);

    //     // Chuyển đổi thành Map
    //     Map<String, Object> deliveryMap = new HashMap<>();
    //     deliveryMap.put("id", delivery.getId());
    //     deliveryMap.put("orderId", delivery.getOrder() != null ? delivery.getOrder().getId() : null);
    //     deliveryMap.put("deliveryFee", delivery.getDeliveryFee());
    //     deliveryMap.put("transportMode", delivery.getTransportMode());
    //     deliveryMap.put("serviceType", delivery.getServiceType());
    //     deliveryMap.put("orderDate", delivery.getOrderDate());
    //     deliveryMap.put("pickupDate", delivery.getPickupDate());
    //     deliveryMap.put("scheduleDeliveryTime", delivery.getScheduleDeliveryTime());
    //     deliveryMap.put("actualDeliveryTime", delivery.getActualDeliveryTime());
    //     deliveryMap.put("orderStatus", delivery.getOrder() != null && delivery.getOrder().getStatus() != null ?
    //         delivery.getOrder().getStatus().getName() : null);
    //     deliveryMap.put("driverId", delivery.getDriver() != null ? delivery.getDriver().getId() : null);
    //     deliveryMap.put("vehicleId", delivery.getVehicle() != null ? delivery.getVehicle().getId() : null);
    //     deliveryMap.put("deliveryAttempts", delivery.getDeliveryAttempts());
    //     deliveryMap.put("deliveryNotes", delivery.getDeliveryNotes());

    //     return ResponseEntity.ok(deliveryMap);
    // }
@GetMapping("/{id}")
public ResponseEntity<Map<String, Object>> getDeliveryById(@PathVariable Long id) {
    Delivery delivery = deliveryService.getDeliveryById(id);
    // Sử dụng ObjectMapper để convert entity sang Map
    Map<String, Object> deliveryMap = new com.fasterxml.jackson.databind.ObjectMapper()
            .convertValue(delivery, Map.class);
    return ResponseEntity.ok(deliveryMap);
}

@DeleteMapping("/{id}")
public ResponseEntity<Map<String, Object>> deleteDelivery(@PathVariable Long id) {
    deliveryService.deleteDelivery(id);
    return ResponseEntity.ok(Map.of(
        "id", id,
        "message", "Delivery deleted successfully"
    ));
}
    /**
     * Create a new delivery with automatic fee calculation
     */
    @PostMapping
    public ResponseEntity<Map<String, Object>> createDelivery(
            @Valid @RequestBody CreateDeliveryRequestDTO dto,
            Authentication authentication) {

        Delivery delivery = new Delivery();
        // Map DTO sang entity
        delivery.setOrder(orderService.getOrderById(dto.getOrderId()));
        delivery.setTransportMode(dto.getTransportMode());
        delivery.setServiceType(dto.getServiceType());
        delivery.setPickupDate(dto.getPickupDate());
        delivery.setScheduleDeliveryTime(dto.getScheduleDeliveryTime());
        delivery.setOrderDate(dto.getOrderDate());
        delivery.setLateDeliveryRisk(dto.getLateDeliveryRisk() != null && dto.getLateDeliveryRisk() ? 1 : 0);
        delivery.setDeliveryNotes(dto.getDeliveryNotes());
        if (dto.getVehicleId() != null) {
            delivery.setVehicle(vehicleService.getVehicleById(dto.getVehicleId()));
        }
        if (dto.getDriverId() != null) {
            delivery.setDriver(userService.getUserById(dto.getDriverId()));
        }
        if (dto.getRouteId() != null) {
            delivery.setRoute(routeService.getRouteById(dto.getRouteId()));
        }

        // Sử dụng method với auto-calculation thay vì createDelivery thông thường
        Delivery createdDelivery = deliveryService.createDeliveryWithFeeCalculation(delivery);

        // Chuyển đổi kết quả thành Map
        Map<String, Object> result = new ObjectMapper().convertValue(createdDelivery, Map.class);
        return new ResponseEntity<>(result, HttpStatus.CREATED);
    }

   @PutMapping("/{id}")
public ResponseEntity<Map<String, Object>> updateDelivery(
        @PathVariable Long id,
        @Valid @RequestBody Map<String, Object> deliveryData,
        Authentication authentication) {

    Delivery delivery = deliveryService.getDeliveryById(id); // Lấy entity cũ

    if (deliveryData.get("orderId") != null) {
        delivery.setOrder(orderService.getOrderById(Long.valueOf(deliveryData.get("orderId").toString())));
    }
    if (deliveryData.get("deliveryFee") != null) {
        delivery.setDeliveryFee(new java.math.BigDecimal(deliveryData.get("deliveryFee").toString()));
    }
    if (deliveryData.get("transportMode") != null) {
        delivery.setTransportMode(ktc.spring_project.enums.TransportMode.valueOf(deliveryData.get("transportMode").toString()));
    }
    if (deliveryData.get("serviceType") != null) {
        delivery.setServiceType(ktc.spring_project.enums.ServiceType.valueOf(deliveryData.get("serviceType").toString()));
    }
    if (deliveryData.get("pickupDate") != null) {
        delivery.setPickupDate(java.sql.Timestamp.valueOf(deliveryData.get("pickupDate").toString()));
    }
    if (deliveryData.get("scheduleDeliveryTime") != null) {
        delivery.setScheduleDeliveryTime(java.sql.Timestamp.valueOf(deliveryData.get("scheduleDeliveryTime").toString()));
    }
    if (deliveryData.get("actualDeliveryTime") != null) {
        delivery.setActualDeliveryTime(java.sql.Timestamp.valueOf(deliveryData.get("actualDeliveryTime").toString()));
    }
    if (deliveryData.get("lateDeliveryRisk") != null) {
        delivery.setLateDeliveryRisk(Integer.valueOf(deliveryData.get("lateDeliveryRisk").toString()));
    }
    if (deliveryData.get("deliveryAttempts") != null) {
        delivery.setDeliveryAttempts(Integer.valueOf(deliveryData.get("deliveryAttempts").toString()));
    }
    if (deliveryData.get("deliveryNotes") != null) {
        delivery.setDeliveryNotes(deliveryData.get("deliveryNotes").toString());
    }
    if (deliveryData.get("orderDate") != null) {
        delivery.setOrderDate(java.sql.Timestamp.valueOf(deliveryData.get("orderDate").toString()));
    }
    if (deliveryData.get("vehicleId") != null) {
        delivery.setVehicle(vehicleService.getVehicleById(Long.valueOf(deliveryData.get("vehicleId").toString())));
    }
    if (deliveryData.get("driverId") != null) {
        delivery.setDriver(userService.getUserById(Long.valueOf(deliveryData.get("driverId").toString())));
    }
    if (deliveryData.get("routeId") != null) {
        delivery.setRoute(routeService.getRouteById(Long.valueOf(deliveryData.get("routeId").toString())));
    }

    Delivery updatedDelivery = deliveryService.createDelivery(delivery); // hoặc save/update

    Map<String, Object> result = new com.fasterxml.jackson.databind.ObjectMapper().convertValue(updatedDelivery, Map.class);
    return ResponseEntity.ok(result);
}

// @PostMapping("/{id}/proof")
// public ResponseEntity<Map<String, Object>> uploadDeliveryProof(
//         @PathVariable Long id,
//         @RequestParam("file") MultipartFile file,
//         @RequestParam(value = "note", required = false) String note,
//         Authentication authentication) {

//     // 1. Kiểm tra delivery tồn tại
//     Delivery delivery = deliveryService.getDeliveryById(id);

//     // 2. Lưu file (ví dụ lưu vào thư mục local, hoặc cloud, hoặc DB)
//     // Ở đây chỉ demo lưu tên file, bạn tự xử lý lưu file thực tế
//     String fileName = file.getOriginalFilename();
//     // TODO: Lưu file vào thư mục hoặc cloud storage

//     // 3. Lưu thông tin proof vào delivery (nếu có trường proof, hoặc tạo bảng proof riêng)
//     // Ví dụ: delivery.setProofFileName(fileName);
//     // delivery.setProofNote(note);
//     // deliveryService.save(delivery);

//     // 4. Trả về kết quả
//     Map<String, Object> result = new HashMap<>();
//     result.put("id", delivery.getId());
//     result.put("fileName", fileName);
//     result.put("note", note);
//     result.put("message", "Proof uploaded successfully");

//     return ResponseEntity.ok(result);
// }
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
         Map<String, Object> result = new com.fasterxml.jackson.databind.ObjectMapper().convertValue(updatedDelivery, Map.class);
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
     * Update order status for a delivery
     * TO-DO: This should properly update the order status in the OrderService
     */
    @PatchMapping("/{id}/status")
    public ResponseEntity<Map<String, Object>> updateDeliveryStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Object> statusData,
            Authentication authentication) {

        String statusName = (String) statusData.get("status");
        String notes = (String) statusData.get("notes");

        // Get delivery information
        Delivery delivery = deliveryService.getDeliveryById(id);

        if (delivery.getOrder() == null) {
            return ResponseEntity.badRequest().body(Map.of(
                "message", "Delivery does not have an associated order"
            ));
        }

        // TO-DO: This is a temporary implementation.
        // In the future, status should be retrieved from StatusService
        // Status status = statusService.findByName(statusName);

        // TO-DO: The proper implementation should be:
        // orderService.updateOrderStatus(delivery.getOrder().getId(), statusName);

        // For now, we're updating the delivery notes only
        delivery.setDeliveryNotes(notes);

        // Save the updated delivery with new notes
        Delivery updatedDelivery = deliveryService.updateDelivery(id, delivery);

        // Convert result to Map
        Map<String, Object> result = new HashMap<>();
        result.put("id", updatedDelivery.getId());
        result.put("orderId", updatedDelivery.getOrder().getId());
        result.put("notes", notes);
        result.put("message", "Delivery notes updated. Order status update requires OrderService implementation.");

        return ResponseEntity.ok(result);
    }

    /**
     * Get delivery tracking information
     * TO-DO: Implement getDeliveryTrackingHistory method in DeliveryTrackingService
     */
    @GetMapping("/{id}/tracking")
    public ResponseEntity<List<Map<String, Object>>> getDeliveryTracking(@PathVariable Long id) {
        // TO-DO: This is a temporary implementation.
        // DeliveryTrackingService doesn't have getDeliveryTrackingHistory method yet.

        // Return empty list until the proper tracking service method is implemented
        return ResponseEntity.ok(new ArrayList<>());
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
     * Future implementation will use AI for analytics predictions
     */
    @GetMapping("/stats/by-vehicle-type")
    public ResponseEntity<List<Map<String, Object>>> getStatsByVehicleType(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        // TO-DO: This is a temporary implementation.
        // In the future, this will analyze actual delivery data and calculate real statistics
        // based on vehicle type, time period, and other relevant factors
        // External AI model will be used for predictive analytics

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
     * Future implementation will use AI for analytics predictions
     */
    @GetMapping("/stats/by-service-type")
    public ResponseEntity<List<Map<String, Object>>> getStatsByServiceType(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        // TO-DO: This is a temporary implementation.
        // In the future, this will analyze actual delivery data and calculate real statistics
        // based on service type, revenue, customer satisfaction, and other metrics
        // External AI model will be used for predictive analytics

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
