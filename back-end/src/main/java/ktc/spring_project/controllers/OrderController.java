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
     * TO-DO: Implement createOrder method in OrderService that accepts DTO and User
     */
    @PostMapping
    public ResponseEntity<Order> createOrder(
            @Valid @RequestBody CreateDeliveryOrderRequestDTO requestDTO,
            Authentication authentication) {

        // TO-DO: Replace with actual implementation when DTO-based createOrder is implemented
        // User currentUser = userService.getCurrentUser(authentication);
        // DeliveryOrderResponseDTO responseDTO = orderService.createOrder(requestDTO, currentUser);

        // Temporary implementation using existing service method
        Order order = new Order();
        // Set basic properties from requestDTO
        // This is a simplified version until proper DTO handling is implemented

        Order createdOrder = orderService.createOrder(order);
        return new ResponseEntity<>(createdOrder, HttpStatus.CREATED);
    }

    /**
     * Get all orders with optional filters
     * TO-DO: Implement getFilteredOrders method in OrderService
     */
    @GetMapping
    public ResponseEntity<List<Order>> getAllOrders(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Long driverId,
            @RequestParam(required = false) Long vehicleId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        // TO-DO: Replace with actual implementation when getFilteredOrders is implemented in OrderService
        // List<DeliveryOrderResponseDTO> orders = orderService.getFilteredOrders(status, driverId, vehicleId, dateFrom, dateTo);

        // Temporary implementation using basic service method
        List<Order> allOrders = orderService.getAllOrders();

        // Apply basic filtering logic here until proper service method is implemented
        // This is just a temporary solution

        return ResponseEntity.ok(allOrders);
    }

    /**
     * Get order by ID
     * TO-DO: Implement getOrderById method in OrderService that returns DeliveryOrderResponseDTO
     */
    @GetMapping("/{id}")
    public ResponseEntity<Order> getOrderById(@PathVariable Long id) {
        // TO-DO: Replace with actual implementation when getOrderById returning DTO is implemented
        // DeliveryOrderResponseDTO order = orderService.getOrderById(id);

        // Temporary implementation using existing service method
        Order order = orderService.getOrderById(id);
        return ResponseEntity.ok(order);
    }
@PatchMapping("/{id}")
    public ResponseEntity<Order> patchOrder(
            @PathVariable Long id,
            @RequestBody Order orderDetails) {
        Order updatedOrder = orderService.updateOrder(id, orderDetails);
        return ResponseEntity.ok(updatedOrder);
    }
    /**
     * Assign vehicle and driver to an order
     * US-ORDER-ASSIGN-01
     * TO-DO: Implement checkSchedulingConflicts and assignVehicleAndDriver methods in OrderService
     */
    @PatchMapping("/{id}/assign")
    public ResponseEntity<Order> assignVehicleAndDriver(
            @PathVariable Long id,
            @RequestBody Map<String, Long> assignmentData,
            Authentication authentication) {

        Long vehicleId = assignmentData.get("vehicleId");
        Long driverId = assignmentData.get("driverId");

        // TO-DO: Replace with actual implementation when these methods are implemented
        // boolean hasConflicts = orderService.checkSchedulingConflicts(id, vehicleId, driverId);
        // DeliveryOrderResponseDTO updatedOrder = orderService.assignVehicleAndDriver(id, vehicleId, driverId, currentUser);

        // Temporary implementation
        Order order = orderService.getOrderById(id);
        // Basic update logic can be added here

        return ResponseEntity.ok(order);
    }

    /**
     * Get available vehicles for a specific time slot
     * Supporting US-ORDER-ASSIGN-01
     * TO-DO: Implement getAvailableVehicles method in VehicleService
     */
    @GetMapping("/available-vehicles")
    public ResponseEntity<List<Vehicle>> getAvailableVehicles(
            @RequestParam String scheduledDate,
            @RequestParam(required = false) String scheduledTime) {

        // TO-DO: Replace with actual implementation when this method is implemented
        // List<Vehicle> availableVehicles = vehicleService.getAvailableVehicles(scheduledDate, scheduledTime);

        // Temporary implementation
        List<Vehicle> allVehicles = vehicleService.getAllVehicles();

        return ResponseEntity.ok(allVehicles);
    }

    /**
     * Get available drivers for a specific time slot
     * Supporting US-ORDER-ASSIGN-01
     * TO-DO: Implement getAvailableDrivers method in UserService
     */
    @GetMapping("/available-drivers")
    public ResponseEntity<List<User>> getAvailableDrivers(
            @RequestParam String scheduledDate,
            @RequestParam(required = false) String scheduledTime) {

        // TO-DO: Replace with actual implementation when this method is implemented
        // List<User> availableDrivers = userService.getAvailableDrivers(scheduledDate, scheduledTime);

        // Temporary implementation to return all users with driver role
        List<User> filteredDrivers = userService.getFilteredUsers("DRIVER", null, null);
        if (filteredDrivers == null || filteredDrivers.isEmpty()) {
            // Fallback if no drivers found
            return ResponseEntity.ok(List.of());
        }

        return ResponseEntity.ok(filteredDrivers);
    }

    /**
     * Update order status
     * TO-DO: Implement updateOrderStatus method in OrderService
     */
    @PatchMapping("/{id}/status")
    public ResponseEntity<Order> updateOrderStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Long> statusData,
            Authentication authentication) {

        Long statusId = statusData.get("statusId");

        // TO-DO: Replace with actual implementation when this method is implemented
        // User currentUser = userService.getCurrentUser(authentication);
        // DeliveryOrderResponseDTO updatedOrder = orderService.updateOrderStatus(id, statusId, currentUser);

        // Temporary implementation
        Order order = orderService.getOrderById(id);
        // Set status ID if needed
        // Ideally, we would fetch the Status entity and set it on the order

        Order updatedOrder = orderService.updateOrder(id, order);
        return ResponseEntity.ok(updatedOrder);
    }

    /**
     * Get real-time tracking information for an order
     * US-ORDER-TRACK-01
     * TO-DO: Implement getOrderTrackingData method in DeliveryTrackingService
     */
    @GetMapping("/{id}/tracking")
    public ResponseEntity<Map<String, Object>> getOrderTracking(@PathVariable Long id) {
        // TO-DO: Replace with actual implementation when this method is implemented
        // Map<String, Object> trackingData = deliveryTrackingService.getOrderTrackingData(id);

        // Temporary placeholder response
        Map<String, Object> trackingData = Map.of(
            "orderId", id,
            "message", "Real-time tracking will be implemented in a future update",
            "status", "PENDING"
        );

        return ResponseEntity.ok(trackingData);
    }

    /**
     * Cancel an order
     * TO-DO: Implement cancelOrder method in OrderService
     */
    @PatchMapping("/{id}/cancel")
    public ResponseEntity<Order> cancelOrder(
            @PathVariable Long id,
            @RequestBody(required = false) Map<String, String> cancelData,
            Authentication authentication) {

        String cancelReason = cancelData != null ? cancelData.get("reason") : null;

        // TO-DO: Replace with actual implementation when this method is implemented
        // User currentUser = userService.getCurrentUser(authentication);
        // DeliveryOrderResponseDTO cancelledOrder = orderService.cancelOrder(id, cancelReason, currentUser);

        // Temporary implementation
        Order order = orderService.getOrderById(id);
        // Update order status to canceled
        // Ideally, we would fetch the 'CANCELED' Status entity and set it

        Order updatedOrder = orderService.updateOrder(id, order);
        return ResponseEntity.ok(updatedOrder);
    }
}
