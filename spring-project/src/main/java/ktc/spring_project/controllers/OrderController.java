package ktc.spring_project.controllers;

import ktc.spring_project.dtos.common.ApiResponse;
import ktc.spring_project.dtos.order.CreateDeliveryOrderRequestDTO;
import ktc.spring_project.dtos.order.DeliveryOrderResponseDTO;
import ktc.spring_project.dtos.order.OrderByStoreResponseDTO;
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
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;
import java.util.stream.Collectors;
import ktc.spring_project.dtos.order.PaginatedOrderResponseDto;

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
    public ResponseEntity<Order> createOrder(@Valid @RequestBody CreateDeliveryOrderRequestDTO dto) {
        try {
            Order order = new Order();
            order.setDescription(dto.getDescription());
            order.setNotes(dto.getNotes());
            order.setTotalAmount(dto.getTotalAmount());
            order.setBenefitPerOrder(BigDecimal.ZERO); // hoặc tính toán nếu cần
            order.setOrderProfitPerOrder(BigDecimal.ZERO); // hoặc tính toán nếu cần
            // Map các trường khác nếu cần
            // Ví dụ: set address, vehicle, status, store, createdBy nếu có logic
            Order createdOrder = orderService.createOrder(order);
            return new ResponseEntity<>(createdOrder, HttpStatus.CREATED);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
        } catch (Exception e) {
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Lấy tất cả đơn hàng (có phân trang)
     */
    @GetMapping
    public ResponseEntity<PaginatedOrderResponseDto> getAllOrders(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "5") int size,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Long driverId,
            @RequestParam(required = false) Long vehicleId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {
        try {
            // Sử dụng phân trang thật từ database với sắp xếp theo createdAt descending
            Page<Order> orderPage = orderService.getOrdersPaginated(page, size);
            
            // Chuyển đổi Order entities thành DTO để tránh circular reference
            List<Map<String, Object>> orderDTOs = orderPage.getContent().stream().map(order -> {
                Map<String, Object> orderDTO = new HashMap<>();
                orderDTO.put("id", order.getId());
                orderDTO.put("description", order.getDescription());
                orderDTO.put("notes", order.getNotes());
                orderDTO.put("totalAmount", order.getTotalAmount());
                orderDTO.put("benefitPerOrder", order.getBenefitPerOrder());
                orderDTO.put("orderProfitPerOrder", order.getOrderProfitPerOrder());
                orderDTO.put("createdAt", order.getCreatedAt());
                orderDTO.put("updatedAt", order.getUpdatedAt());
                // Status info
                if (order.getStatus() != null) {
                    Map<String, Object> statusDTO = new HashMap<>();
                    statusDTO.put("id", order.getStatus().getId());
                    statusDTO.put("name", order.getStatus().getName());
                    statusDTO.put("statusType", order.getStatus().getStatusType());
                    orderDTO.put("status", statusDTO);
                }
                // Vehicle info
                if (order.getVehicle() != null) {
                    Map<String, Object> vehicleDTO = new HashMap<>();
                    vehicleDTO.put("id", order.getVehicle().getId());
                    vehicleDTO.put("licensePlate", order.getVehicle().getLicensePlate());
                    vehicleDTO.put("vehicleType", order.getVehicle().getVehicleType());
                    // Current driver info
                    if (order.getVehicle().getCurrentDriver() != null) {
                        Map<String, Object> driverDTO = new HashMap<>();
                        driverDTO.put("id", order.getVehicle().getCurrentDriver().getId());
                        driverDTO.put("fullName", order.getVehicle().getCurrentDriver().getFullName());
                        driverDTO.put("phone", order.getVehicle().getCurrentDriver().getPhone());
                        vehicleDTO.put("currentDriver", driverDTO);
                    }
                    orderDTO.put("vehicle", vehicleDTO);
                }
                // Address info
                if (order.getAddress() != null) {
                    Map<String, Object> addressDTO = new HashMap<>();
                    addressDTO.put("id", order.getAddress().getId());
                    addressDTO.put("address", order.getAddress().getAddress());
                    addressDTO.put("city", order.getAddress().getCity());
                    addressDTO.put("latitude", order.getAddress().getLatitude());
                    addressDTO.put("longitude", order.getAddress().getLongitude());
                    orderDTO.put("address", addressDTO);
                }
                // Store info
                if (order.getStore() != null) {
                    Map<String, Object> storeDTO = new HashMap<>();
                    storeDTO.put("id", order.getStore().getId());
                    storeDTO.put("storeName", order.getStore().getStoreName());
                    storeDTO.put("address", order.getStore().getAddress());
                    storeDTO.put("phone", order.getStore().getPhone());
                    storeDTO.put("latitude", order.getStore().getLatitude());
                    storeDTO.put("longitude", order.getStore().getLongitude());
                    orderDTO.put("store", storeDTO);
                }
                // Created by user info
                if (order.getCreatedBy() != null) {
                    Map<String, Object> createdByDTO = new HashMap<>();
                    createdByDTO.put("id", order.getCreatedBy().getId());
                    createdByDTO.put("fullName", order.getCreatedBy().getFullName());
                    createdByDTO.put("username", order.getCreatedBy().getUsername());
                    orderDTO.put("createdBy", createdByDTO);
                }
                return orderDTO;
            }).collect(Collectors.toList());

            PaginatedOrderResponseDto response = PaginatedOrderResponseDto.builder()
                    .data(orderDTOs)
                    .pageNumber(page)
                    .pageSize(size)
                    .totalRecords(orderPage.getTotalElements())
                    .totalPages(orderPage.getTotalPages())
                    .hasNext(orderPage.hasNext())
                    .hasPrevious(orderPage.hasPrevious())
                    .build();
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            System.err.println("Error in getAllOrders: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
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

    // DTO đơn giản để nhận vehicleId
    public static class UpdateOrderVehicleDTO {
        public Long vehicleId;
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

    // API cập nhật vehicle cho đơn hàng
    @PatchMapping("/{id}/vehicle")
    public ResponseEntity<Order> updateOrderVehicle(
            @PathVariable Long id,
            @RequestBody UpdateOrderVehicleDTO dto) {
        try {
            Order order = orderService.getOrderById(id);
            if (dto.vehicleId != null && dto.vehicleId > 0) {
                // Assign vehicle
                Vehicle vehicle = new Vehicle();
                vehicle.setId(dto.vehicleId);
                order.setVehicle(vehicle);
            } else {
                // Unassign vehicle (vehicleId is null or 0)
                order.setVehicle(null);
            }
            Order updatedOrder = orderService.createOrder(order);
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