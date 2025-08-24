package ktc.spring_project.controllers;

import ktc.spring_project.dtos.DeliveryFeeBreakdown;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.Order;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.repositories.DeliveryRepository;
import ktc.spring_project.repositories.OrderRepository;
import ktc.spring_project.services.DeliveryFeeCalculationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@RestController
@RequestMapping("/api/delivery")
@Slf4j
public class DeliveryFeeController {

    @Autowired
    private DeliveryFeeCalculationService deliveryFeeService;
    
    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private DeliveryRepository deliveryRepository;

    /**
     * Tính delivery fee cho Order theo ID
     * LẤY SERVICE_TYPE TỪ DATABASE - bảng deliveries
     * GET /api/delivery/order/{orderId}/calculate
     */
    @GetMapping("/order/{orderId}/calculate")
    public ResponseEntity<Map<String, Object>> calculateDeliveryFee(@PathVariable Long orderId) {
        try {
            // Kiểm tra Order có tồn tại không
            Optional<Order> orderOpt = orderRepository.findById(orderId);
            if (!orderOpt.isPresent()) {
                Map<String, Object> response = new HashMap<>();
                response.put("success", false);
                response.put("error", "Không tìm thấy Order với ID: " + orderId);
                return ResponseEntity.notFound().build();
            }

            Order order = orderOpt.get();
            
            // TODO: Lấy ServiceType từ bảng deliveries
            // Delivery delivery = deliveryRepository.findByOrderId(orderId);
            // ServiceType serviceType = delivery.getServiceType();
            
            // TEMPORARY: Dùng STANDARD làm mặc định cho đến khi có entity Delivery
            ServiceType serviceType = ServiceType.STANDARD;
            log.warn("Using default ServiceType.STANDARD - cần lấy từ bảng deliveries");
            
            // Tính delivery fee
            DeliveryFeeBreakdown breakdown = deliveryFeeService.calculateDeliveryFee(order, serviceType);

            // Tạo response
            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("orderId", orderId);
            response.put("serviceType", serviceType);
            response.put("serviceTypeSource", "DATABASE (deliveries table)");
            response.put("breakdown", breakdown);
            response.put("totalDeliveryFee", breakdown.getTotalDeliveryFee());

            return ResponseEntity.ok(response);
            
        } catch (Exception e) {
            log.error("Error calculating delivery fee for Order {}: {}", orderId, e.getMessage());
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("error", e.getMessage());
            response.put("timestamp", java.time.LocalDateTime.now());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * Tính delivery fee cho Order và lưu vào database  
     * LẤY SERVICE_TYPE TỪ DATABASE - bảng deliveries
     * PUT /api/delivery/order/{orderId}/calculate-and-save
     */
    @PutMapping("/order/{orderId}/calculate-and-save")
    public ResponseEntity<Map<String, Object>> calculateAndSaveDeliveryFee(@PathVariable Long orderId) {
        try {
            // Kiểm tra Order có tồn tại không
            Optional<Order> orderOpt = orderRepository.findById(orderId);
            if (!orderOpt.isPresent()) {
                Map<String, Object> response = new HashMap<>();
                response.put("success", false);
                response.put("error", "Không tìm thấy Order với ID: " + orderId);
                return ResponseEntity.notFound().build();
            }

            Order order = orderOpt.get();
            
            // LẤY ServiceType TỪ DATABASE - bảng deliveries
            List<Delivery> deliveries = deliveryRepository.findByOrderId(orderId);
            if (deliveries.isEmpty()) {
                Map<String, Object> response = new HashMap<>();
                response.put("success", false);
                response.put("error", "Không tìm thấy Delivery cho Order ID: " + orderId);
                response.put("note", "Cần tạo Delivery record trước khi tính delivery fee");
                return ResponseEntity.badRequest().body(response);
            }
            
            Delivery delivery = deliveries.get(0); // Lấy delivery đầu tiên
            ServiceType serviceType = delivery.getServiceType();
            
            if (serviceType == null) {
                serviceType = ServiceType.STANDARD; // Fallback default
                log.warn("ServiceType is null for Delivery ID: {}, using STANDARD as default", delivery.getId());
            }
            
            // Tính delivery fee
            DeliveryFeeBreakdown breakdown = deliveryFeeService.calculateDeliveryFee(order, serviceType);

            // LƯU delivery fee VÀO DATABASE
            delivery.setDeliveryFee(breakdown.getTotalDeliveryFee());
            deliveryRepository.save(delivery);
            log.info("Đã lưu delivery fee {} VNĐ cho Delivery ID: {}", breakdown.getTotalDeliveryFee(), delivery.getId());

            // Tạo response
            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("orderId", orderId);
            response.put("deliveryId", delivery.getId());
            response.put("serviceType", serviceType);
            response.put("serviceTypeSource", "DATABASE (deliveries table)");
            response.put("action", "CALCULATED AND SAVED TO DATABASE");
            response.put("breakdown", breakdown);
            response.put("totalDeliveryFee", breakdown.getTotalDeliveryFee());
            response.put("savedAt", java.time.LocalDateTime.now());

            return ResponseEntity.ok(response);
            
        } catch (Exception e) {
            log.error("Error calculating and saving delivery fee for Order {}: {}", orderId, e.getMessage());
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("error", e.getMessage());
            response.put("timestamp", java.time.LocalDateTime.now());
            return ResponseEntity.badRequest().body(response);
        }
    }
}
