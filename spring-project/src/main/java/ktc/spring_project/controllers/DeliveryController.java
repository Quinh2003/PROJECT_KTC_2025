
package ktc.spring_project.controllers;

import ktc.spring_project.services.ChecklistService;
import lombok.extern.slf4j.Slf4j;

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
import ktc.spring_project.exceptions.EntityNotFoundException;

import com.fasterxml.jackson.databind.ObjectMapper;

import ktc.spring_project.services.OrderService;
import ktc.spring_project.dtos.delivery.CreateDeliveryRequestDTO;
import ktc.spring_project.dtos.delivery.UpdateDeliveryRequestDTO;

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
@Slf4j
public class DeliveryController {

    @Autowired
    private DeliveryService deliveryService;

    @Autowired
    private ChecklistService checklistService;

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
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) Long orderId) {

        // Lấy tất cả giao hàng
        List<Delivery> deliveries;
        if (orderId != null) {
            deliveries = deliveryService.findByOrderId(orderId);
        } else {
            deliveries = deliveryService.getAllDeliveries();
        }

        // Chuyển đổi từ Delivery sang Map để dễ dàng thêm thông tin bổ sung nếu cần
        List<Map<String, Object>> deliveriesMap = new ArrayList<>();
        for (Delivery delivery : deliveries) {
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
    @GetMapping("/{id}")
    public ResponseEntity<Map<String, Object>> getDeliveryById(@PathVariable Long id) {
        Delivery delivery = deliveryService.getDeliveryById(id);
        // Sử dụng ObjectMapper để convert entity sang Map
        Map<String, Object> deliveryMap = new com.fasterxml.jackson.databind.ObjectMapper()
                .convertValue(delivery, Map.class);
        return ResponseEntity.ok(deliveryMap);
    }

    /**
     * Get deliveries by order ID
     */
    @GetMapping("/order/{orderId}")
    public ResponseEntity<List<Map<String, Object>>> getDeliveriesByOrderId(@PathVariable Long orderId) {
        List<Delivery> deliveries = deliveryService.findByOrderId(orderId);
        
        List<Map<String, Object>> deliveriesMap = new ArrayList<>();
        for (Delivery delivery : deliveries) {
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
            deliveryMap.put("driverId", delivery.getDriver() != null ? delivery.getDriver().getId() : null);
            deliveryMap.put("vehicleId", delivery.getVehicle() != null ? delivery.getVehicle().getId() : null);
            deliveryMap.put("deliveryAttempts", delivery.getDeliveryAttempts());
            deliveryMap.put("deliveryNotes", delivery.getDeliveryNotes());
            deliveriesMap.add(deliveryMap);
        }
        
        return ResponseEntity.ok(deliveriesMap);
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
        
        // Chuyển logic tạo delivery vào service
        Delivery createdDelivery = deliveryService.createDeliveryFromDTO(dto);
        
        Map<String, Object> result = new ObjectMapper().convertValue(createdDelivery, Map.class);
        return new ResponseEntity<>(result, HttpStatus.CREATED);
    }

    @PutMapping("/{id}")
    public ResponseEntity<Map<String, Object>> updateDelivery(
            @PathVariable Long id,
            @Valid @RequestBody UpdateDeliveryRequestDTO dto,
            Authentication authentication) {
        Delivery delivery = deliveryService.getDeliveryById(id);
        if (dto.getDeliveryFee() != null) {
            delivery.setDeliveryFee(dto.getDeliveryFee());
        }
        if (dto.getTransportMode() != null) {
            delivery.setTransportMode(dto.getTransportMode());
        }
        if (dto.getServiceType() != null) {
            delivery.setServiceType(dto.getServiceType());
        }
        if (dto.getPickupDate() != null) {
            delivery.setPickupDate(dto.getPickupDate());
        }
        if (dto.getScheduleDeliveryTime() != null) {
            delivery.setScheduleDeliveryTime(dto.getScheduleDeliveryTime());
        }
        if (dto.getActualDeliveryTime() != null) {
            delivery.setActualDeliveryTime(dto.getActualDeliveryTime());
        }
        if (dto.getLateDeliveryRisk() != null) {
            delivery.setLateDeliveryRisk(dto.getLateDeliveryRisk() ? 1 : 0);
        }
        if (dto.getDeliveryAttempts() != null) {
            delivery.setDeliveryAttempts(dto.getDeliveryAttempts());
        }
        if (dto.getDeliveryNotes() != null) {
            delivery.setDeliveryNotes(dto.getDeliveryNotes());
        }
        if (dto.getVehicleId() != null) {
            delivery.setVehicle(vehicleService.getVehicleById(dto.getVehicleId()));
        }
        if (dto.getDriverId() != null) {
            delivery.setDriver(userService.getUserById(dto.getDriverId()));
        }
        if (dto.getRouteId() != null) {
            delivery.setRoute(routeService.getRouteById(dto.getRouteId()));
        }
        Delivery updatedDelivery = deliveryService.createDelivery(delivery);
        Map<String, Object> result = new ObjectMapper().convertValue(updatedDelivery, Map.class);
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

        // ...ghi log checklist sẽ được tối ưu hóa ở service hoặc AOP...

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

        // ...ghi log checklist sẽ được tối ưu hóa ở service hoặc AOP...

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

    /**
     * Tính tổng doanh thu cho ngày cụ thể từ các delivery đã hoàn thành
     * GET /api/deliveries/revenue-by-date?date=2025-09-15
     */
    @GetMapping("/revenue-by-date")
    public ResponseEntity<Long> getRevenueByDate(@RequestParam String date) {
        try {
            Long revenue = deliveryService.calculateRevenueByDate(date);
            return ResponseEntity.ok(revenue);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(0L);
        }
    }

    /**
     * Tính performance statistics cho 2 tuần (tuần này vs tuần trước)
     * GET /api/deliveries/performance-stats
     */
    @GetMapping("/performance-stats")
    public ResponseEntity<Map<String, Object>> getPerformanceStats() {
        try {
            Map<String, Object> stats = deliveryService.calculatePerformanceStats();
            return ResponseEntity.ok(stats);
        } catch (Exception e) {
            Map<String, Object> errorStats = new HashMap<>();
            errorStats.put("percentage", 0);
            errorStats.put("changePercent", 0.0);
            errorStats.put("trend", "stable");
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorStats);
        }
    }

    /**
     * Tính doanh thu theo tháng trong 12 tháng gần nhất
     * GET /api/deliveries/monthly-revenue
     */
    @GetMapping("/monthly-revenue")
    public ResponseEntity<Map<String, Object>> getMonthlyRevenue() {
        try {
            Map<String, Object> monthlyData = deliveryService.calculateMonthlyRevenue();
            return ResponseEntity.ok(monthlyData);
        } catch (Exception e) {
            Map<String, Object> errorData = new HashMap<>();
            errorData.put("monthlyRevenue", new ArrayList<>());
            errorData.put("totalRevenue", 0L);
            errorData.put("growthPercent", 0.0);
            errorData.put("averageRevenue", 0L);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorData);
        }
    }
}
