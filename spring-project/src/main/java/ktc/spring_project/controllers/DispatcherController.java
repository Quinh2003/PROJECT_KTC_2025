package ktc.spring_project.controllers;

import ktc.spring_project.dtos.timeline.OrderTimelineResponse;
import ktc.spring_project.entities.User;
import ktc.spring_project.services.OrderService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.DispatcherService;
import java.util.Map;
import java.util.HashMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

/**
 * Controller chuyên biệt cho Dispatcher workflow
 * Quản lý tất cả các chức năng liên quan đến dispatcher trong hệ thống giao hàng
 */
@RestController
@RequestMapping("/api/dispatcher")
@CrossOrigin(originPatterns = {"http://localhost:*", "https://localhost:*"}, allowCredentials = "true")
public class DispatcherController {
    @Autowired
    private DispatcherService dispatcherService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private UserService userService;

    /**
     * Lấy danh sách đơn hàng theo status cho dispatcher
     * @param status Trạng thái đơn hàng (pending, processing, shipped, delivered, ...)
     * @param page Số trang (mặc định 1)
     * @param size Kích thước trang (mặc định 10)
     * @return Danh sách đơn hàng có phân trang
     */
    @GetMapping("/orders/status/{status}")
    @PreAuthorize("hasRole('DISPATCHER') or hasRole('ADMIN')")
    public ResponseEntity<Page<OrderTimelineResponse>> getOrdersByStatus(
            @PathVariable String status,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        try {
            Page<OrderTimelineResponse> orderPage = orderService.getOrderTimelineByStatusPaginated(status, page, size);
            return ResponseEntity.ok(orderPage);
        } catch (Exception e) {
            System.err.println("Error in getOrdersByStatus: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }
    }

    /**
     * Dispatcher nhận đơn hàng và chuyển sang trạng thái "Processing"
     * @param id ID của đơn hàng
     * @param authentication Thông tin xác thực của dispatcher
     * @return Thông tin đơn hàng đã được nhận
     */
    @PostMapping("/orders/{id}/accept")
    @PreAuthorize("hasRole('DISPATCHER')")
    public ResponseEntity<OrderTimelineResponse> acceptOrder(@PathVariable Long id, Authentication authentication) {
        try {
            // Lấy thông tin dispatcher từ authentication
            String username = authentication.getName();
            User dispatcher = userService.findByUsername(username);
            
            if (dispatcher == null) {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(null);
            }
            
            OrderTimelineResponse acceptedOrder = orderService.acceptOrderByDispatcher(id, dispatcher.getId());
            return ResponseEntity.ok(acceptedOrder);
        } catch (Exception e) {
            System.err.println("Error accepting order: " + e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }
    }

    /**
     * Dispatcher gán tài xế cho đơn hàng và chuyển sang trạng thái "Shipped"
     * @param id ID của đơn hàng
     * @param request Thông tin yêu cầu gán tài xế
     * @param authentication Thông tin xác thực của dispatcher
     * @return Thông tin đơn hàng đã được gán tài xế
     */
    @PostMapping("/orders/{id}/assign-driver")
    @PreAuthorize("hasRole('DISPATCHER')")
    public ResponseEntity<?> assignDriverToOrder(
            @PathVariable Long id,
            @RequestBody ktc.spring_project.dtos.AssignDriverRequestDTO request,
            Authentication authentication) {
        try {
            String username = authentication.getName();
            User dispatcher = userService.findByUsername(username);
            if (dispatcher == null) {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of("error", "Unauthorized"));
            }
            Long driverId = request.getDriverId();
            Long vehicleId = request.getVehicleId();
            Map<String, Object> result = dispatcherService.assignDriverAndVehicle(id, driverId, vehicleId, dispatcher.getId());
            if (result.containsKey("error")) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(result);
            }
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(Map.of("error", e.getMessage()));
        }
    }

    /**
     * Lấy dashboard tổng quan cho dispatcher
     * @return Thông tin tổng quan về đơn hàng, tài xế, xe
     */
    @GetMapping("/dashboard")
    @PreAuthorize("hasRole('DISPATCHER') or hasRole('ADMIN')")
    public ResponseEntity<?> getDashboard() {
        try {
            // Implement logic lấy thông tin dashboard
            // Có thể bao gồm: số đơn hàng theo status, số tài xế available, etc.
            return ResponseEntity.ok("Dashboard functionality - To be implemented");
        } catch (Exception e) {
            System.err.println("Error getting dispatcher dashboard: " + e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }
    }

    /**
     * Lấy danh sách tài xế có sẵn để gán đơn hàng
     * @return Danh sách tài xế available
     */
    @GetMapping("/available-drivers")
    @PreAuthorize("hasRole('DISPATCHER') or hasRole('ADMIN')")
    public ResponseEntity<?> getAvailableDrivers() {
        try {
            // Implement logic lấy danh sách tài xế available
            return ResponseEntity.ok("Available drivers functionality - To be implemented");
        } catch (Exception e) {
            System.err.println("Error getting available drivers: " + e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }
    }

    /**
     * Lấy danh sách xe có sẵn để gán đơn hàng
     * @return Danh sách xe available
     */
    @GetMapping("/available-vehicles")
    @PreAuthorize("hasRole('DISPATCHER') or hasRole('ADMIN')")
    public ResponseEntity<?> getAvailableVehicles() {
        try {
            // Implement logic lấy danh sách xe available
            return ResponseEntity.ok("Available vehicles functionality - To be implemented");
        } catch (Exception e) {
            System.err.println("Error getting available vehicles: " + e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }
    }
}