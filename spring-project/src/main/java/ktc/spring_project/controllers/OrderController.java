package ktc.spring_project.controllers;

import ktc.spring_project.dtos.order.CreateDeliveryOrderRequestDTO;
import ktc.spring_project.dtos.order.DeliveryOrderResponseDTO;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.entities.Status;
import ktc.spring_project.entities.Route;
import ktc.spring_project.services.OrderService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.VehicleService;
import ktc.spring_project.services.DeliveryTrackingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/orders")
public class OrderController {

    @Autowired
    private OrderService orderService;

    @Autowired
    private UserService userService;

    @Autowired
    private VehicleService vehicleService;

    @Autowired
    private DeliveryTrackingService deliveryTrackingService;

    /**
     * Tạo đơn hàng mới
     */
    @PostMapping
    public ResponseEntity<Order> createOrder(@RequestBody Order order) {
        try {
            // Đặt giá trị mặc định nếu không được cung cấp
            if (order.getBenefitPerOrder() == null) {
                order.setBenefitPerOrder(BigDecimal.ZERO);
            }
            if (order.getOrderProfitPerOrder() == null) {
                order.setOrderProfitPerOrder(BigDecimal.ZERO);
            }
            if (order.getTotalAmount() == null) {
                order.setTotalAmount(BigDecimal.valueOf(500.00)); // Giá trị mặc định từ JSON của bạn
            }
            
            Order createdOrder = orderService.createOrder(order);
            return new ResponseEntity<>(createdOrder, HttpStatus.CREATED);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
        } catch (Exception e) {
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Lấy tất cả đơn hàng
     */
    @GetMapping
    public ResponseEntity<List<Order>> getAllOrders(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Long driverId,
            @RequestParam(required = false) Long vehicleId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        try {
            List<Order> allOrders = orderService.getAllOrders();
            return ResponseEntity.ok(allOrders);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(List.of());
        }
    }

    /**
     * Lấy đơn hàng theo ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Order> getOrderById(@PathVariable Long id) {
        try {
            Order order = orderService.getOrderById(id);
            return ResponseEntity.ok(order);
        } catch (Exception e) {
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * Cập nhật đơn hàng
     */
    @PatchMapping("/{id}")
    public ResponseEntity<Order> patchOrder(
            @PathVariable Long id,
            @RequestBody Order orderDetails) {
        try {
            Order updatedOrder = orderService.updateOrder(id, orderDetails);
            return ResponseEntity.ok(updatedOrder);
        } catch (Exception e) {
            return ResponseEntity.notFound().build();
        }
    }

    @PutMapping("/{id}")
public ResponseEntity<Order> putOrder(
        @PathVariable Long id,
        @RequestBody Order orderDetails) {
    try {
        Order updatedOrder = orderService.updateOrder(id, orderDetails);
        return ResponseEntity.ok(updatedOrder);
    } catch (Exception e) {
        return ResponseEntity.notFound().build();
    }
}
// ...existing code...

// DTO đơn giản để nhận statusId
public static class UpdateOrderStatusDTO {
    public Long statusId;
}

// API cập nhật trạng thái đơn hàng
@PatchMapping("/{id}/status")
public ResponseEntity<Order> updateOrderStatus(
        @PathVariable Long id,
        @RequestBody UpdateOrderStatusDTO dto) {
    try {
        Order order = orderService.getOrderById(id);
        if (dto.statusId != null) {
            Status status = new Status();
            status.setId(dto.statusId != null ? dto.statusId.shortValue() : null);
            order.setStatus(status);
        }
        Order updatedOrder = orderService.createOrder(order); // hoặc orderService.save(order)
        return ResponseEntity.ok(updatedOrder);
    } catch (Exception e) {
        return ResponseEntity.notFound().build();
    }
}
@GetMapping("/{id}/tracking")
public ResponseEntity<?> getOrderTracking(@PathVariable Long id) {
    try {
        // Giả sử bạn có service lấy tracking info
        Object trackingInfo = orderService.getOrderTrackingInfo(id);
        if (trackingInfo == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(trackingInfo);
    } catch (Exception e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error: " + e.getMessage());
    }
}

}