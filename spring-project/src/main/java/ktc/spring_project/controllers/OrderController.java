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
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Controller responsible for managing delivery orders
 * Based on user stories:
 * - US-ORDER-CREATE-01: Create Delivery Order
 * - US-ORDER-ASSIGN-01: Assign Vehicle & Driver
 * - US-ORDER-TRACK-01: Real-Time Order Tracking
 */
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
     * Create a new delivery order
     * US-ORDER-CREATE-01
     */
    @PostMapping
    public ResponseEntity<DeliveryOrderResponseDTO> createOrder(
            @Valid @RequestBody CreateDeliveryOrderRequestDTO requestDTO,
            Authentication authentication) {

        // Get current user (dispatcher) from authentication
        User currentUser = userService.getCurrentUser(authentication);

        // Create order and return response
        DeliveryOrderResponseDTO responseDTO = orderService.createOrder(requestDTO, currentUser);
        return new ResponseEntity<>(responseDTO, HttpStatus.CREATED);
    }

    /**
     * Get all orders with optional filters
     */
    @GetMapping
    public ResponseEntity<List<DeliveryOrderResponseDTO>> getAllOrders(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Long driverId,
            @RequestParam(required = false) Long vehicleId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        List<DeliveryOrderResponseDTO> orders = orderService.getFilteredOrders(
                status, driverId, vehicleId, dateFrom, dateTo);

        return ResponseEntity.ok(orders);
    }

    /**
     * Get order by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<DeliveryOrderResponseDTO> getOrderById(@PathVariable Long id) {
        DeliveryOrderResponseDTO order = orderService.getOrderById(id);
        return ResponseEntity.ok(order);
    }

    /**
     * Assign vehicle and driver to an order
     * US-ORDER-ASSIGN-01
     */
    @PatchMapping("/{id}/assign")
    public ResponseEntity<DeliveryOrderResponseDTO> assignVehicleAndDriver(
            @PathVariable Long id,
            @RequestBody Map<String, Long> assignmentData,
            Authentication authentication) {

        Long vehicleId = assignmentData.get("vehicleId");
        Long driverId = assignmentData.get("driverId");

        // Check if there are scheduling conflicts
        boolean hasConflicts = orderService.checkSchedulingConflicts(id, vehicleId, driverId);

        if (hasConflicts) {
            return ResponseEntity.badRequest().build();
        }

        User currentUser = userService.getCurrentUser(authentication);
        DeliveryOrderResponseDTO updatedOrder = orderService.assignVehicleAndDriver(
                id, vehicleId, driverId, currentUser);

        return ResponseEntity.ok(updatedOrder);
    }

    /**
     * Get available vehicles for a specific time slot
     * Supporting US-ORDER-ASSIGN-01
     */
    @GetMapping("/available-vehicles")
    public ResponseEntity<List<Vehicle>> getAvailableVehicles(
            @RequestParam String scheduledDate,
            @RequestParam(required = false) String scheduledTime) {

        List<Vehicle> availableVehicles = vehicleService.getAvailableVehicles(scheduledDate, scheduledTime);
        return ResponseEntity.ok(availableVehicles);
    }

    /**
     * Get available drivers for a specific time slot
     * Supporting US-ORDER-ASSIGN-01
     */
    @GetMapping("/available-drivers")
    public ResponseEntity<List<User>> getAvailableDrivers(
            @RequestParam String scheduledDate,
            @RequestParam(required = false) String scheduledTime) {

        List<User> availableDrivers = userService.getAvailableDrivers(scheduledDate, scheduledTime);
        return ResponseEntity.ok(availableDrivers);
    }

    /**
     * Update order status
     */
    @PatchMapping("/{id}/status")
    public ResponseEntity<DeliveryOrderResponseDTO> updateOrderStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Long> statusData,
            Authentication authentication) {

        Long statusId = statusData.get("statusId");
        User currentUser = userService.getCurrentUser(authentication);

        DeliveryOrderResponseDTO updatedOrder = orderService.updateOrderStatus(id, statusId, currentUser);
        return ResponseEntity.ok(updatedOrder);
    }

    /**
     * Get real-time tracking information for an order
     * US-ORDER-TRACK-01
     */
    @GetMapping("/{id}/tracking")
    public ResponseEntity<?> getOrderTracking(@PathVariable Long id) {
        Map<String, Object> trackingData = deliveryTrackingService.getOrderTrackingData(id);
        return ResponseEntity.ok(trackingData);
    }

    /**
     * Cancel an order
     */
    @PatchMapping("/{id}/cancel")
    public ResponseEntity<DeliveryOrderResponseDTO> cancelOrder(
            @PathVariable Long id,
            @RequestBody(required = false) Map<String, String> cancelData,
            Authentication authentication) {

        String cancelReason = cancelData != null ? cancelData.get("reason") : null;
        User currentUser = userService.getCurrentUser(authentication);

        DeliveryOrderResponseDTO cancelledOrder = orderService.cancelOrder(id, cancelReason, currentUser);
        return ResponseEntity.ok(cancelledOrder);
    }
}
