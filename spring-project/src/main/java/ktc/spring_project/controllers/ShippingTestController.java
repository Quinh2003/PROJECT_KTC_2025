package ktc.spring_project.controllers;

import ktc.spring_project.dtos.ShippingCalculationRequest;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.services.DistanceCalculationService;
import ktc.spring_project.services.ShippingCalculationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/api/test")
public class ShippingTestController {

    @Autowired
    private DistanceCalculationService distanceCalculationService;

    @Autowired
    private ShippingCalculationService shippingCalculationService;

    /**
     * Test API với dữ liệu mẫu
     * GET /api/test/shipping-example
     */
    @GetMapping("/shipping-example")
    public ResponseEntity<Map<String, Object>> testShippingExample() {
        Map<String, Object> response = new HashMap<>();

        try {
            // Test case 1: Hàng thường, STANDARD
            Map<String, Object> test1 = runTestCase1();
            response.put("test1_hang_thuong_standard", test1);

            // Test case 2: Hàng dễ vỡ, EXPRESS
            Map<String, Object> test2 = runTestCase2();
            response.put("test2_hang_de_vo_express", test2);

            // Test case 3: Hàng thường, PRIORITY
            Map<String, Object> test3 = runTestCase3();
            response.put("test3_hang_thuong_priority", test3);

            response.put("success", true);
            return ResponseEntity.ok(response);

        } catch (Exception e) {
            response.put("success", false);
            response.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    private Map<String, Object> runTestCase1() {
        // Trường hợp 1: Hàng thường, STANDARD
        // Gói hàng: 3kg, thể tích 60,000 cm³ (50×40×30), 20km
        // is_fragile = false, service_type = STANDARD

        Map<String, Object> result = new HashMap<>();
        result.put("description", "Hàng thường, STANDARD - 3kg, 60,000 cm³, 20km");

        // Tạo request
        ShippingCalculationRequest request = new ShippingCalculationRequest();
        request.setActualWeight(new BigDecimal("3"));
        request.setVolume(new BigDecimal("60000")); // 50×40×30 = 60,000 cm³
        request.setDistance(new BigDecimal("20"));
        request.setFragile(false);
        request.setServiceType(ServiceType.STANDARD);
        request.setQuantity(1);

        var breakdown = shippingCalculationService.calculateShippingFee(request);

        result.put("input", Map.of(
            "weight", "3kg",
            "volume", "60,000 cm³",
            "distance", "20km",
            "isFragile", false,
            "serviceType", "STANDARD"
        ));

        result.put("calculation", Map.of(
            "volumeWeight", breakdown.getVolumeWeight() + "kg",
            "billableWeight", breakdown.getBillableWeight() + "kg",
            "weightFee", breakdown.getWeightFee() + " VNĐ",
            "distanceFee", breakdown.getDistanceFee() + " VNĐ",
            "baseFee", breakdown.getBaseFee() + " VNĐ",
            "riskMultiplier", breakdown.getRiskMultiplier(),
            "serviceMultiplier", breakdown.getServiceMultiplier(),
            "totalFee", breakdown.getTotalFee() + " VNĐ"
        ));

        result.put("details", breakdown.getCalculationDetails());

        return result;
    }

    private Map<String, Object> runTestCase2() {
        // Trường hợp 2: Hàng dễ vỡ, EXPRESS
        // Gói hàng: 1.5kg, thể tích 11,250 cm³ (30×25×15), 12km
        // is_fragile = true, service_type = EXPRESS

        Map<String, Object> result = new HashMap<>();
        result.put("description", "Hàng dễ vỡ, EXPRESS - 1.5kg, 11,250 cm³, 12km");

        ShippingCalculationRequest request = new ShippingCalculationRequest();
        request.setActualWeight(new BigDecimal("1.5"));
        request.setVolume(new BigDecimal("11250")); // 30×25×15 = 11,250 cm³
        request.setDistance(new BigDecimal("12"));
        request.setFragile(true);
        request.setServiceType(ServiceType.EXPRESS);
        request.setQuantity(1);

        var breakdown = shippingCalculationService.calculateShippingFee(request);

        result.put("input", Map.of(
            "weight", "1.5kg",
            "volume", "11,250 cm³",
            "distance", "12km",
            "isFragile", true,
            "serviceType", "EXPRESS"
        ));

        result.put("calculation", Map.of(
            "volumeWeight", breakdown.getVolumeWeight() + "kg",
            "billableWeight", breakdown.getBillableWeight() + "kg",
            "weightFee", breakdown.getWeightFee() + " VNĐ",
            "distanceFee", breakdown.getDistanceFee() + " VNĐ",
            "baseFee", breakdown.getBaseFee() + " VNĐ",
            "riskMultiplier", breakdown.getRiskMultiplier(),
            "serviceMultiplier", breakdown.getServiceMultiplier(),
            "totalFee", breakdown.getTotalFee() + " VNĐ"
        ));

        result.put("details", breakdown.getCalculationDetails());

        return result;
    }

    private Map<String, Object> runTestCase3() {
        // Trường hợp 3: Hàng thường, PRIORITY
        // Gói hàng: 0.5kg, thể tích 3,000 cm³ (20×15×10), 8km
        // is_fragile = false, service_type = PRIORITY

        Map<String, Object> result = new HashMap<>();
        result.put("description", "Hàng thường, PRIORITY - 0.5kg, 3,000 cm³, 8km");

        ShippingCalculationRequest request = new ShippingCalculationRequest();
        request.setActualWeight(new BigDecimal("0.5"));
        request.setVolume(new BigDecimal("3000")); // 20×15×10 = 3,000 cm³
        request.setDistance(new BigDecimal("8"));
        request.setFragile(false);
        request.setServiceType(ServiceType.PRIORITY);
        request.setQuantity(1);

        var breakdown = shippingCalculationService.calculateShippingFee(request);

        result.put("input", Map.of(
            "weight", "0.5kg",
            "volume", "3,000 cm³",
            "distance", "8km",
            "isFragile", false,
            "serviceType", "PRIORITY"
        ));

        result.put("calculation", Map.of(
            "volumeWeight", breakdown.getVolumeWeight() + "kg",
            "billableWeight", breakdown.getBillableWeight() + "kg",
            "weightFee", breakdown.getWeightFee() + " VNĐ",
            "distanceFee", breakdown.getDistanceFee() + " VNĐ",
            "baseFee", breakdown.getBaseFee() + " VNĐ",
            "riskMultiplier", breakdown.getRiskMultiplier(),
            "serviceMultiplier", breakdown.getServiceMultiplier(),
            "totalFee", breakdown.getTotalFee() + " VNĐ"
        ));

        result.put("details", breakdown.getCalculationDetails());

        return result;
    }

    /**
     * Test tính khoảng cách giữa 2 điểm
     * GET /api/test/distance-example
     */
    @GetMapping("/distance-example")
    public ResponseEntity<Map<String, Object>> testDistanceExample() {
        Map<String, Object> response = new HashMap<>();

        try {
            // Tọa độ mẫu: Hà Nội đến TP.HCM
            BigDecimal hanoiLat = new BigDecimal("21.0285");
            BigDecimal hanoiLon = new BigDecimal("105.8542");
            BigDecimal hcmLat = new BigDecimal("10.8231");
            BigDecimal hcmLon = new BigDecimal("106.6297");

            BigDecimal distance = distanceCalculationService.calculateDistance(
                hanoiLat, hanoiLon, hcmLat, hcmLon
            );

            String zoneType = distanceCalculationService.getZoneType(distance);

            response.put("success", true);
            response.put("from", "Hà Nội (21.0285, 105.8542)");
            response.put("to", "TP.HCM (10.8231, 106.6297)");
            response.put("distance", distance + " km");
            response.put("zoneType", zoneType);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            response.put("success", false);
            response.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * Test với tọa độ tùy chỉnh
     * GET /api/test/custom-distance
     */
    @GetMapping("/custom-distance")
    public ResponseEntity<Map<String, Object>> testCustomDistance(
            @RequestParam BigDecimal fromLat,
            @RequestParam BigDecimal fromLon,
            @RequestParam BigDecimal toLat,
            @RequestParam BigDecimal toLon) {
        
        Map<String, Object> response = new HashMap<>();

        try {
            BigDecimal distance = distanceCalculationService.calculateDistance(
                fromLat, fromLon, toLat, toLon
            );

            String zoneType = distanceCalculationService.getZoneType(distance);

            response.put("success", true);
            response.put("from", String.format("(%s, %s)", fromLat, fromLon));
            response.put("to", String.format("(%s, %s)", toLat, toLon));
            response.put("distance", distance + " km");
            response.put("zoneType", zoneType);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            response.put("success", false);
            response.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }
}
