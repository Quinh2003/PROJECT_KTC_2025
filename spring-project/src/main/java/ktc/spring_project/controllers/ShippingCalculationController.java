package ktc.spring_project.controllers;

import ktc.spring_project.dtos.DistanceCalculationRequest;
import ktc.spring_project.dtos.ShippingCalculationRequest;
import ktc.spring_project.dtos.ShippingFeeBreakdown;
import ktc.spring_project.entities.OrderItem;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.services.DistanceCalculationService;
import ktc.spring_project.services.OrderItemShippingService;
import ktc.spring_project.services.ShippingCalculationService;
import ktc.spring_project.repositories.OrderItemRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@RestController
@RequestMapping("/api/shipping")
public class ShippingCalculationController {

    @Autowired
    private DistanceCalculationService distanceCalculationService;

    @Autowired
    private ShippingCalculationService shippingCalculationService;

    @Autowired
    private OrderItemShippingService orderItemShippingService;

    @Autowired
    private OrderItemRepository orderItemRepository;

    /**
     * API tính khoảng cách giữa 2 điểm
     * POST /api/shipping/calculate-distance
     */
    @PostMapping("/calculate-distance")
    public ResponseEntity<Map<String, Object>> calculateDistance(@RequestBody DistanceCalculationRequest request) {
        try {
            BigDecimal distance = distanceCalculationService.calculateDistance(request);
            String zoneType = distanceCalculationService.getZoneType(distance);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("distance", distance);
            response.put("zoneType", zoneType);
            response.put("unit", "km");

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * API tính phí shipping dựa trên các tham số
     * POST /api/shipping/calculate-fee
     */
    @PostMapping("/calculate-fee")
    public ResponseEntity<Map<String, Object>> calculateShippingFee(@RequestBody ShippingCalculationRequest request) {
        try {
            ShippingFeeBreakdown breakdown = shippingCalculationService.calculateShippingFee(request);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("breakdown", breakdown);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * API tính và cập nhật shipping fee cho OrderItem
     * PUT /api/shipping/order-item/{orderItemId}/calculate
     */
    @PutMapping("/order-item/{orderItemId}/calculate")
    public ResponseEntity<Map<String, Object>> calculateOrderItemShippingFee(
            @PathVariable Long orderItemId,
            @RequestParam(defaultValue = "STANDARD") ServiceType serviceType) {
        try {
            Optional<OrderItem> orderItemOpt = orderItemRepository.findById(orderItemId);
            if (!orderItemOpt.isPresent()) {
                Map<String, Object> response = new HashMap<>();
                response.put("success", false);
                response.put("error", "Không tìm thấy OrderItem với ID: " + orderItemId);
                return ResponseEntity.notFound().build();
            }

            OrderItem orderItem = orderItemOpt.get();
            ShippingFeeBreakdown breakdown = orderItemShippingService.calculateAndUpdateShippingFee(
                orderItem, serviceType
            );

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("orderItemId", orderItemId);
            response.put("updatedShippingFee", orderItem.getShippingFee());
            response.put("breakdown", breakdown);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * API tính shipping fee cho OrderItem mà không cập nhật database
     * GET /api/shipping/order-item/{orderItemId}/preview
     */
    @GetMapping("/order-item/{orderItemId}/preview")
    public ResponseEntity<Map<String, Object>> previewOrderItemShippingFee(
            @PathVariable Long orderItemId,
            @RequestParam(defaultValue = "STANDARD") ServiceType serviceType) {
        try {
            Optional<OrderItem> orderItemOpt = orderItemRepository.findById(orderItemId);
            if (!orderItemOpt.isPresent()) {
                Map<String, Object> response = new HashMap<>();
                response.put("success", false);
                response.put("error", "Không tìm thấy OrderItem với ID: " + orderItemId);
                return ResponseEntity.notFound().build();
            }

            OrderItem orderItem = orderItemOpt.get();
            ShippingFeeBreakdown breakdown = orderItemShippingService.calculateShippingFeeOnly(
                orderItem, serviceType
            );

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("orderItemId", orderItemId);
            response.put("currentShippingFee", orderItem.getShippingFee());
            response.put("calculatedShippingFee", breakdown.getTotalFee());
            response.put("breakdown", breakdown);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * API tính shipping fee đơn giản với các tham số cơ bản
     * POST /api/shipping/simple-calculate
     */
    @PostMapping("/simple-calculate")
    public ResponseEntity<Map<String, Object>> simpleCalculateShippingFee(@RequestBody Map<String, Object> params) {
        try {
            BigDecimal weight = new BigDecimal(params.get("weight").toString());
            BigDecimal volume = params.get("volume") != null ? new BigDecimal(params.get("volume").toString()) : BigDecimal.ZERO;
            BigDecimal fromLatitude = new BigDecimal(params.get("fromLatitude").toString());
            BigDecimal fromLongitude = new BigDecimal(params.get("fromLongitude").toString());
            BigDecimal toLatitude = new BigDecimal(params.get("toLatitude").toString());
            BigDecimal toLongitude = new BigDecimal(params.get("toLongitude").toString());
            boolean isFragile = params.get("isFragile") != null ? (Boolean) params.get("isFragile") : false;
            ServiceType serviceType = params.get("serviceType") != null ? 
                ServiceType.valueOf(params.get("serviceType").toString()) : ServiceType.STANDARD;
            Integer quantity = params.get("quantity") != null ? 
                Integer.parseInt(params.get("quantity").toString()) : 1;

            BigDecimal shippingFee = orderItemShippingService.calculateSimpleShippingFee(
                weight, volume, fromLatitude, fromLongitude, toLatitude, toLongitude,
                isFragile, serviceType, quantity
            );

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("shippingFee", shippingFee);
            response.put("parameters", params);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * API lấy danh sách các loại dịch vụ và hệ số
     * GET /api/shipping/service-types
     */
    @GetMapping("/service-types")
    public ResponseEntity<Map<String, Object>> getServiceTypes() {
        Map<String, Object> response = new HashMap<>();
        Map<String, Object> serviceTypes = new HashMap<>();

        for (ServiceType serviceType : ServiceType.values()) {
            Map<String, Object> info = new HashMap<>();
            info.put("displayName", serviceType.getDisplayName());
            info.put("multiplier", serviceType.getMultiplier());
            serviceTypes.put(serviceType.name(), info);
        }

        response.put("success", true);
        response.put("serviceTypes", serviceTypes);

        return ResponseEntity.ok(response);
    }
}
