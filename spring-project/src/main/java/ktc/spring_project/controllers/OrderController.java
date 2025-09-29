

package ktc.spring_project.controllers;

import ktc.spring_project.dtos.order.CreateDeliveryOrderRequestDTO;
import ktc.spring_project.dtos.order.DeliveryOrderResponseDTO;
import ktc.spring_project.dtos.order.OrderSummaryDTO;
import ktc.spring_project.dtos.order.OrderStatsDto;
import ktc.spring_project.dtos.order.PaginatedOrderSummaryResponseDto;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.entities.Status;
import ktc.spring_project.entities.Route;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.DeliveryTracking;
import ktc.spring_project.services.OrderService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.VehicleService;
import ktc.spring_project.services.DeliveryTrackingService;
import ktc.spring_project.services.DeliveryService;
import ktc.spring_project.services.ChecklistService;
import ktc.spring_project.services.StatusService;
import ktc.spring_project.dtos.ChecklistProgressResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import org.springframework.data.domain.PageRequest;

import jakarta.validation.Valid;
import java.util.Map;
import java.util.Optional;

import jakarta.validation.Valid;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
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
    private StatusService statusService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private UserService userService;

    @Autowired
    private VehicleService vehicleService;

    @Autowired
    private DeliveryService deliveryService;


    @Autowired
    private DeliveryTrackingService deliveryTrackingService;


    @Autowired
    private ChecklistService checklistService;

    /**
 * Get order by ID with items included
 */
@GetMapping("/{orderId}")
public ResponseEntity<Order> getOrder(@PathVariable Long orderId) {
    Order order = orderService.getOrderById(orderId);
    if (order == null) {
        return ResponseEntity.notFound().build();
    }
    return ResponseEntity.ok(order);
}

    /**
     * API: Lấy tất cả đơn hàng chưa completed, sort id giảm dần (không phân trang)
     */
    @GetMapping("/not-completed/all")
    public ResponseEntity<List<Order>> getAllNotCompletedOrdersSortedByIdDesc() {
        List<Order> orders = orderService.getAllNotCompletedOrdersSortedByIdDesc();
        return ResponseEntity.ok(orders);
    }
/**
     * API: Lấy đơn hàng đã giao thành công (status_id = 2) có phân trang
     */
    @GetMapping("/completed/paginated")
    public ResponseEntity<Page<Order>> getCompletedOrdersPaginated(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        // Lấy đơn hàng có status name là 'Completed' hoặc 'COMPLETED'
        Page<Order> completedOrders = orderService.getCompletedOrdersPaginated(PageRequest.of(page - 1, size, org.springframework.data.domain.Sort.by(org.springframework.data.domain.Sort.Direction.DESC, "id")));
        return ResponseEntity.ok(completedOrders);
    }
    /**
     * API: Lấy tất cả đơn hàng đã giao thành công (status_id = 2)
     */
    @GetMapping("/completed")
    public ResponseEntity<List<Order>> getCompletedOrders() {
        // Lấy tất cả đơn hàng có status_id = 2
        List<Order> completedOrders = orderService.getAllOrders().stream()
            .filter(order -> order.getStatus() != null && order.getStatus().getId() == 2)
            .collect(Collectors.toList());
        return ResponseEntity.ok(completedOrders);
    }
    @GetMapping("/not-completed")
    public ResponseEntity<Page<Order>> getNotCompletedOrders(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        PageRequest pageRequest = PageRequest.of(page - 1, size, org.springframework.data.domain.Sort.by(org.springframework.data.domain.Sort.Direction.DESC, "id"));
        Page<Order> notCompletedOrders = orderService.getNotCompletedOrdersPaginated(pageRequest);
        return ResponseEntity.ok(notCompletedOrders);
    }
    /**
     * Tạo đơn hàng mới
     */
    @PostMapping
    public ResponseEntity<Order> createOrder(@Valid @RequestBody CreateDeliveryOrderRequestDTO dto) {
        try {
            // Log thông tin tạo đơn hàng nếu cần
            System.out.println("Creating order with DTO: " + dto.getOrderCode());
            if (dto.getStore() != null) System.out.println("Store ID: " + dto.getStore().getId());
            if (dto.getCreatedBy() != null) System.out.println("CreatedBy ID: " + dto.getCreatedBy().getId());
            if (dto.getStatus() != null) System.out.println("Status ID: " + dto.getStatus().getId());
            if (dto.getAddress() != null) System.out.println("Address ID: " + dto.getAddress().getId());

            Order createdOrder = orderService.createOrderFromDTO(dto);

            // Log thông tin đơn hàng đã tạo
            System.out.println("Order created successfully:");
            System.out.println("- Order ID: " + createdOrder.getId());
            System.out.println("- Store ID: " + (createdOrder.getStore() != null ? createdOrder.getStore().getId() : "null"));
            System.out.println("- Status ID: " + (createdOrder.getStatus() != null ? createdOrder.getStatus().getId() : "null"));
            System.out.println("- Created By ID: " + (createdOrder.getCreatedBy() != null ? createdOrder.getCreatedBy().getId() : "null"));
            System.out.println("- Address ID: " + (createdOrder.getAddress() != null ? createdOrder.getAddress().getId() : "null"));
            if (dto.getPickupDate() != null) {
                System.out.println("- Pickup Date: " + dto.getPickupDate() + " (will be handled by separate delivery creation)");
            }

            return new ResponseEntity<>(createdOrder, HttpStatus.CREATED);
        } catch (IllegalArgumentException e) {
            System.err.println("Bad request: " + e.getMessage());
            e.printStackTrace();
            return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
        } catch (Exception e) {
            System.err.println("===== DETAILED ERROR =====");
            System.err.println("Error creating order: " + e.getMessage());
            System.err.println("Exception type: " + e.getClass().getName());
            System.err.println("Cause: " + (e.getCause() != null ? e.getCause().getMessage() : "null"));
            e.printStackTrace();
            System.err.println("===========================");
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Lấy số lượng đơn hàng theo ngày (tối ưu cho dashboard)
     */
    @GetMapping("/count-by-date")
    public ResponseEntity<Map<String, Object>> getOrderCountByDate(
            @RequestParam(required = false) String date) {
        try {
            // Nếu không có date parameter, tính cho hôm nay và hôm qua
            java.time.LocalDate targetDate = date != null ? 
                java.time.LocalDate.parse(date) : java.time.LocalDate.now();
            java.time.LocalDate yesterday = targetDate.minusDays(1);
            
            // Sử dụng database query trực tiếp thay vì load toàn bộ data
            long todayCount = orderService.countOrdersByDate(targetDate);
            long yesterdayCount = orderService.countOrdersByDate(yesterday);
            
            // Tính phần trăm thay đổi
            double changePercent = 0;
            String trend = "stable";
            
            if (yesterdayCount > 0) {
                changePercent = ((double)(todayCount - yesterdayCount) / yesterdayCount) * 100;
            } else if (todayCount > 0) {
                changePercent = 100;
            }
            
            if (changePercent > 0) {
                trend = "increase";
            } else if (changePercent < 0) {
                trend = "decrease";
            }
            
            Map<String, Object> result = new HashMap<>();
            result.put("date", targetDate.toString());
            result.put("count", todayCount);
            result.put("previousCount", yesterdayCount);
            result.put("changePercent", Math.round(changePercent * 10.0) / 10.0);
            result.put("trend", trend);
            
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            System.err.println("Error getting order count by date: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Lấy tất cả đơn hàng (có phân trang)
     */
    @GetMapping
    public ResponseEntity<PaginatedOrderResponseDto> getAllOrders(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size,
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
                    statusDTO.put("id", order.getStatus().getId().longValue());
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
     * Note: This is a duplicate of the method above. The original method has been kept.
     */
    /*
    @GetMapping("/{id}")
    public ResponseEntity<Order> getOrderById(@PathVariable Long id) {
        try {
            Order order = orderService.getOrderById(id);
            return ResponseEntity.ok(order);
        } catch (Exception e) {
            return ResponseEntity.notFound().build();
        }
    }
    */

    /**
     * Cập nhật đơn hàng
     */
    @PatchMapping("/{id}")
    public ResponseEntity<Order> patchOrder(
            @PathVariable Long id,
            @Valid @RequestBody Order orderDetails) {
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
    @Valid @RequestBody Order orderDetails) {
    try {
        Order updatedOrder = orderService.updateOrder(id, orderDetails);
        return ResponseEntity.ok(updatedOrder);
    } catch (Exception e) {
        return ResponseEntity.notFound().build();
    }
}
// ...existing code...

    // DTO đơn giản để nhận statusId
    static class UpdateOrderStatusDTO {
        public Long statusId;
    }

    // DTO đơn giản để nhận vehicleId
    static class UpdateOrderVehicleDTO {
        public Long vehicleId;
    }

    // API cập nhật trạng thái đơn hàng
    @PatchMapping("/{id}/status")
    public ResponseEntity<?> updateOrderStatus(
            @PathVariable Long id,
            @Valid @RequestBody UpdateOrderStatusDTO dto,
            @RequestParam(required = false) Long dispatcherId) {
        Map<String, Object> result = new HashMap<>();
        try {
            Order order = orderService.getOrderById(id);
            // Nếu stepName là DRIVER_RECEIVE_ORDER thì chuyển trạng thái sang Shipped
            Optional<Status> shippedStatus = statusService.getStatusByTypeAndName("ORDER", "Shipped");
            if (shippedStatus.isPresent()) {
                order.setStatus(shippedStatus.get());
            }
            Order updatedOrder = orderService.createOrder(order);
            checklistService.markStepCompleted(
                dispatcherId != null ? dispatcherId : (order.getCreatedBy() != null ? order.getCreatedBy().getId() : null),
                id,
                "DRIVER_RECEIVE_ORDER",
                "Driver nhận đơn, chuyển trạng thái sang Shipped"
            );
            ChecklistProgressResponse checklistLog = checklistService.getProgressByOrder(id);
            result.put("order", updatedOrder);
            result.put("checklistLog", checklistLog);
            result.put("message", "Cập nhật trạng thái thành công và đã ghi log checklist!");
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            result.put("error", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(result);
        }
    }

    // API cập nhật vehicle cho đơn hàng
    @PatchMapping("/{id}/vehicle")
    public ResponseEntity<?> updateOrderVehicle(
            @PathVariable Long id,
            @Valid @RequestBody UpdateOrderVehicleDTO dto) {
        try {
            System.out.println("🚗 OrderController: Updating vehicle for order " + id + " with vehicleId: " + dto.vehicleId);
            Order order = orderService.getOrderById(id);

            org.springframework.security.core.Authentication auth = org.springframework.security.core.context.SecurityContextHolder.getContext().getAuthentication();
            ktc.spring_project.entities.User actor = null;
            if (auth != null && auth.getPrincipal() instanceof ktc.spring_project.entities.User) {
                actor = (ktc.spring_project.entities.User) auth.getPrincipal();
            }

            if (dto.vehicleId != null && dto.vehicleId > 0) {
                Vehicle vehicle = vehicleService.getVehicleById(dto.vehicleId);
                if (vehicle == null) {
                    return ResponseEntity.badRequest().build();
                }
                order.setVehicle(vehicle);
                System.out.println("Assigning vehicle " + vehicle.getLicensePlate() + " to order " + id);

                // Ghi log checklist: luôn lấy actor là user thực hiện thao tác
                String vehicleInfo = (vehicle != null) ? ("Vehicle: " + vehicle.getLicensePlate()) : "Vehicle: null";
                String driverInfo = (vehicle != null && vehicle.getCurrentDriver() != null) ? ("Driver: " + vehicle.getCurrentDriver().getFullName() + " (" + vehicle.getCurrentDriver().getPhone() + ")") : "Driver: null";
                String details = "Assigned " + vehicleInfo + " to order " + id + ". " + driverInfo;
                if (actor != null) {
                    checklistService.markStepCompleted(actor.getId(), id, "DISPATCHER_ASSIGN_DRIVER", details);
                }

                // Cập nhật driver vào order
                if (vehicle != null && vehicle.getCurrentDriver() != null) {
                    order.setDriver(vehicle.getCurrentDriver());
                }

                // Tìm hoặc tạo delivery record để lưu driver
                try {
                    List<Delivery> deliveries = deliveryService.findByOrderId(id);
                    Delivery delivery = null;
                    if (deliveries != null && !deliveries.isEmpty()) {
                        delivery = deliveries.get(0);
                    }
                    if (delivery == null) {
                        delivery = new Delivery();
                        delivery.setOrder(order);
                        delivery.setVehicle(vehicle);
                        if (vehicle.getCurrentDriver() != null) {
                            delivery.setDriver(vehicle.getCurrentDriver());
                            System.out.println("Creating new delivery with driver: " + vehicle.getCurrentDriver().getFullName());
                        }
                        delivery.setOrderDate(new java.sql.Timestamp(System.currentTimeMillis()));
                        delivery.setLateDeliveryRisk(0);
                        deliveryService.save(delivery);
                    } else {
                        delivery.setVehicle(vehicle);
                        if (vehicle.getCurrentDriver() != null) {
                            delivery.setDriver(vehicle.getCurrentDriver());
                            System.out.println("Updating delivery with driver: " + vehicle.getCurrentDriver().getFullName());
                        }
                        deliveryService.save(delivery);
                    }
                    createInitialTrackingForVehicleAssignment(vehicle, delivery, order);
                } catch (Exception deliveryException) {
                    System.err.println("Error managing delivery: " + deliveryException.getMessage());
                }
            } else {
                order.setVehicle(null);
                System.out.println("Unassigning vehicle from order " + id);
                try {
                    List<Delivery> deliveries = deliveryService.findByOrderId(id);
                    if (deliveries != null && !deliveries.isEmpty()) {
                        Delivery delivery = deliveries.get(0);
                        delivery.setVehicle(null);
                        delivery.setDriver(null);
                        deliveryService.save(delivery);
                        System.out.println("Unassigned vehicle and driver from delivery");
                    }
                } catch (Exception deliveryException) {
                    System.err.println("Error unassigning delivery: " + deliveryException.getMessage());
                }
            }

            Order updatedOrder = orderService.updateOrder(id, order);
            if (dto.vehicleId != null && dto.vehicleId > 0) {
                Vehicle responseVehicle = order.getVehicle();
                if (responseVehicle != null && responseVehicle.getCurrentDriver() != null) {
                    updatedOrder.setDriver(responseVehicle.getCurrentDriver());
                }
            }
            return ResponseEntity.ok(updatedOrder);
        } catch (Exception e) {
            System.err.println("Error updating order vehicle: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
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

    /**
     * Lấy danh sách đơn hàng theo store ID
     */
    @GetMapping("/store/{storeId}")
    public ResponseEntity<List<Order>> getOrdersByStore(@PathVariable Long storeId) {
        try {
            List<Order> orders = orderService.getOrdersByStoreId(storeId);
            if (orders.isEmpty()) {
                return ResponseEntity.noContent().build();
            }
            return ResponseEntity.ok(orders);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Lấy thông tin tóm tắt của đơn hàng theo store ID, bao gồm:
     * - Store ID
     * - Ngày tạo
     * - Địa chỉ nhận hàng
     * - Số lượng sản phẩm
     * - Phí vận chuyển
     * - Trạng thái đơn hàng
     */
    @GetMapping("/store/{storeId}/summary")
    public ResponseEntity<List<OrderSummaryDTO>> getOrderSummariesByStore(@PathVariable Long storeId) {
        try {
            List<OrderSummaryDTO> orderSummaries = orderService.getOrderSummariesByStoreId(storeId);
            if (orderSummaries.isEmpty()) {
                return ResponseEntity.noContent().build();
            }
            return ResponseEntity.ok(orderSummaries);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Lấy thông tin tóm tắt của đơn hàng theo store ID với phân trang (10 items/page)
     */
    @GetMapping("/store/{storeId}/summary/paginated")
    public ResponseEntity<PaginatedOrderSummaryResponseDto> getOrderSummariesByStorePaginated(
            @PathVariable Long storeId,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        try {
            Page<OrderSummaryDTO> orderSummariesPage = orderService.getOrderSummariesByStoreIdPaginated(storeId, page, size);
            
            PaginatedOrderSummaryResponseDto response = PaginatedOrderSummaryResponseDto.builder()
                    .data(orderSummariesPage.getContent())
                    .pageNumber(page)
                    .pageSize(size)
                    .totalRecords(orderSummariesPage.getTotalElements())
                    .totalPages(orderSummariesPage.getTotalPages())
                    .hasNext(orderSummariesPage.hasNext())
                    .hasPrevious(orderSummariesPage.hasPrevious())
                    .build();
            
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Lấy thông tin tóm tắt của tất cả đơn hàng theo user ID
     * User ID sẽ được dùng để tìm các store mà user đó tạo
     * Sau đó lấy tất cả order của các store đó
     */
    @GetMapping("/user/{userId}/summary")
    public ResponseEntity<List<OrderSummaryDTO>> getOrderSummariesByUser(@PathVariable Long userId) {
        try {
            List<OrderSummaryDTO> orderSummaries = orderService.getOrderSummariesByUserId(userId);
            if (orderSummaries.isEmpty()) {
                return ResponseEntity.noContent().build();
            }
            return ResponseEntity.ok(orderSummaries);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Tự động tạo tracking record khi assign vehicle cho order
     */
    private void createInitialTrackingForVehicleAssignment(Vehicle vehicle, Delivery delivery, Order order) {
        try {
            System.out.println("🚀 Creating initial tracking for vehicle assignment...");
            
            // Kiểm tra xem đã có tracking cho vehicle+delivery này chưa
            List<DeliveryTracking> existingTrackings = deliveryTrackingService.findByVehicleIdAndDeliveryId(
                vehicle.getId(), delivery.getId());
            
            if (existingTrackings != null && !existingTrackings.isEmpty()) {
                System.out.println("✅ Tracking already exists for vehicle " + vehicle.getId() + 
                    " and delivery " + delivery.getId() + ", skipping creation");
                return;
            }
            
            // Tạo tracking record mới với tọa độ store (pickup location)
            DeliveryTracking tracking = new DeliveryTracking();
            tracking.setVehicle(vehicle);
            tracking.setDelivery(delivery);
            
            // Sử dụng tọa độ store làm vị trí ban đầu
            if (order.getStore() != null && 
                order.getStore().getLatitude() != null && 
                order.getStore().getLongitude() != null) {
                
                tracking.setLatitude(order.getStore().getLatitude());
                tracking.setLongitude(order.getStore().getLongitude());
                tracking.setLocation("At store: " + order.getStore().getStoreName());
            } else {
                // Fallback coordinates (Hồ Chí Minh City center)
                tracking.setLatitude(new java.math.BigDecimal("10.762622"));
                tracking.setLongitude(new java.math.BigDecimal("106.660172"));
                tracking.setLocation("Default location - Store coordinates not available");
            }
            
            tracking.setNotes("Auto-created tracking for vehicle assignment - Order #" + order.getId());
            tracking.setTimestamp(new java.sql.Timestamp(System.currentTimeMillis()));
            
            // Set default status if needed (status ID 1)
            // statusService.getStatusById((short) 1).ifPresent(tracking::setStatus);
            
            DeliveryTracking saved = deliveryTrackingService.save(tracking);
            
            System.out.println("✅ Initial tracking created successfully: ID=" + saved.getId() + 
                ", Vehicle=" + vehicle.getLicensePlate() + 
                ", Delivery=" + delivery.getId() + 
                ", Location=[" + tracking.getLatitude() + ", " + tracking.getLongitude() + "]");
                
        } catch (Exception e) {
            System.err.println("❌ Error creating initial tracking: " + e.getMessage());
            e.printStackTrace();
            // Không throw exception để không làm gián đoạn vehicle assignment
        }
    }

    /**
     * Lấy thông tin tóm tắt của đơn hàng theo user ID với phân trang (10 items/page)
     */
    @GetMapping("/user/{userId}/summary/paginated")
    public ResponseEntity<PaginatedOrderSummaryResponseDto> getOrderSummariesByUserPaginated(
            @PathVariable Long userId,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        try {
            Page<OrderSummaryDTO> orderSummariesPage = orderService.getOrderSummariesByUserIdPaginated(userId, page, size);
            
            PaginatedOrderSummaryResponseDto response = PaginatedOrderSummaryResponseDto.builder()
                    .data(orderSummariesPage.getContent())
                    .pageNumber(page)
                    .pageSize(size)
                    .totalRecords(orderSummariesPage.getTotalElements())
                    .totalPages(orderSummariesPage.getTotalPages())
                    .hasNext(orderSummariesPage.hasNext())
                    .hasPrevious(orderSummariesPage.hasPrevious())
                    .build();
            
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Unified search API for orders with multiple criteria
     * API: GET /api/orders/search?storeId=xxx&orderId=xxx&fromDate=yyyy-MM-dd&toDate=yyyy-MM-dd&status=xxx,yyy&page=1&size=10
     * All parameters except storeId are optional
     * status parameter can accept multiple values separated by comma
     */
    @GetMapping("/search")
    public ResponseEntity<PaginatedOrderSummaryResponseDto> searchOrdersUnified(
            @RequestParam Long storeId,
            @RequestParam(required = false) Long orderId,
            @RequestParam(required = false) String fromDate,
            @RequestParam(required = false) String toDate,
            @RequestParam(required = false) List<String> status,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        try {
            LocalDateTime fromDateTime = null;
            LocalDateTime toDateTime = null;
            
            // Parse fromDate nếu có
            if (fromDate != null && !fromDate.trim().isEmpty()) {
                try {
                    fromDateTime = LocalDate.parse(fromDate.trim()).atStartOfDay();
                } catch (Exception e) {
                    System.err.println("Invalid fromDate format: " + fromDate);
                    return ResponseEntity.badRequest().build();
                }
            }
            
            // Parse toDate nếu có  
            if (toDate != null && !toDate.trim().isEmpty()) {
                try {
                    toDateTime = LocalDate.parse(toDate.trim()).atTime(23, 59, 59);
                } catch (Exception e) {
                    System.err.println("Invalid toDate format: " + toDate);
                    return ResponseEntity.badRequest().build();
                }
            }

            // Filter out empty status values
            List<String> statusList = null;
            if (status != null && !status.isEmpty()) {
                statusList = status.stream()
                    .filter(s -> s != null && !s.trim().isEmpty())
                    .collect(java.util.stream.Collectors.toList());
                if (statusList.isEmpty()) {
                    statusList = null;
                }
            }

            System.out.println("Unified search with: storeId=" + storeId + ", orderId=" + orderId + 
                    ", fromDate=" + fromDateTime + ", toDate=" + toDateTime + ", statusList=" + statusList +
                    ", page=" + page + ", size=" + size);
            
            Page<OrderSummaryDTO> ordersPage = orderService.searchOrdersByStoreIdWithFiltersPaginated(
                storeId, orderId, fromDateTime, toDateTime, statusList, page, size);
            
            PaginatedOrderSummaryResponseDto response = new PaginatedOrderSummaryResponseDto(
                    ordersPage.getContent(),
                    ordersPage.getNumber() + 1, // Convert 0-based to 1-based 
                    ordersPage.getSize(),
                    ordersPage.getTotalElements(),
                    ordersPage.getTotalPages(),
                    ordersPage.hasNext(),
                    ordersPage.hasPrevious()
            );
            
            System.out.println("Found " + ordersPage.getTotalElements() + " total orders, returning page " + page + " of " + ordersPage.getTotalPages());
            
            return ResponseEntity.ok(response);
            
        } catch (IllegalArgumentException e) {
            System.err.println("Bad request: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.badRequest().build();
        } catch (Exception e) {
            System.err.println("===== DETAILED ERROR =====");
            System.err.println("Error in unified search: " + e.getMessage());
            System.err.println("Exception type: " + e.getClass().getName());
            System.err.println("Cause: " + (e.getCause() != null ? e.getCause().getMessage() : "null"));
            e.printStackTrace();
            System.err.println("===========================");
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * @deprecated Use searchOrdersUnified instead
     * Tìm kiếm đơn hàng theo ID đơn hàng với phân trang
     * API: GET /api/orders/search?storeId=xxx&orderId=xxx&page=1&size=10
     */
    @GetMapping("/search-by-order-id")
    public ResponseEntity<PaginatedOrderSummaryResponseDto> searchOrdersByOrderId(
            @RequestParam Long storeId,
            @RequestParam Long orderId,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        try {
            System.out.println("Searching orders with storeId: " + storeId + ", orderId: " + orderId + ", page: " + page + ", size: " + size);
            
            Page<OrderSummaryDTO> ordersPage = orderService.searchOrdersByStoreIdAndOrderIdPaginated(storeId, orderId, page, size);
            
            PaginatedOrderSummaryResponseDto response = new PaginatedOrderSummaryResponseDto(
                    ordersPage.getContent(),
                    ordersPage.getNumber() + 1, // Convert 0-based to 1-based 
                    ordersPage.getSize(),
                    ordersPage.getTotalElements(),
                    ordersPage.getTotalPages(),
                    ordersPage.hasNext(),
                    ordersPage.hasPrevious()
            );
            
            System.out.println("Found " + ordersPage.getTotalElements() + " total orders, returning page " + page + " of " + ordersPage.getTotalPages());
            
            return ResponseEntity.ok(response);
            
        } catch (Exception e) {
            System.err.println("Error searching orders by orderId: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Tìm kiếm đơn hàng theo khoảng thời gian với phân trang
     * API: GET /api/orders/search-by-date?storeId=xxx&fromDate=yyyy-MM-dd&toDate=yyyy-MM-dd&page=1&size=10
     */
    @GetMapping("/search-by-date")
    public ResponseEntity<PaginatedOrderSummaryResponseDto> searchOrdersByDateRange(
            @RequestParam Long storeId,
            @RequestParam(required = false) String fromDate,
            @RequestParam(required = false) String toDate,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        try {
            LocalDateTime fromDateTime = null;
            LocalDateTime toDateTime = null;
            
            // Parse fromDate nếu có
            if (fromDate != null && !fromDate.trim().isEmpty()) {
                try {
                    LocalDate parsedFromDate = LocalDate.parse(fromDate);
                    fromDateTime = parsedFromDate.atStartOfDay(); // 00:00:00 của ngày đó
                    System.out.println("Parsed fromDate: " + fromDateTime);
                } catch (Exception e) {
                    System.err.println("Invalid fromDate format: " + fromDate + " - " + e.getMessage());
                    return ResponseEntity.badRequest().build();
                }
            }
            
            // Parse toDate nếu có
            if (toDate != null && !toDate.trim().isEmpty()) {
                try {
                    LocalDate parsedToDate = LocalDate.parse(toDate);
                    toDateTime = parsedToDate.atTime(23, 59, 59); // 23:59:59 của ngày đó
                    System.out.println("Parsed toDate: " + toDateTime);
                } catch (Exception e) {
                    System.err.println("Invalid toDate format: " + toDate + " - " + e.getMessage());
                    return ResponseEntity.badRequest().build();
                }
            }
            
            System.out.println("Searching orders with storeId: " + storeId + 
                    ", fromDateTime: " + fromDateTime + ", toDateTime: " + toDateTime + 
                    ", page: " + page + ", size: " + size);
            
            Page<OrderSummaryDTO> ordersPage = orderService.searchOrdersByStoreIdAndDateRangePaginated(storeId, fromDateTime, toDateTime, page, size);
            
            PaginatedOrderSummaryResponseDto response = new PaginatedOrderSummaryResponseDto(
                    ordersPage.getContent(),
                    ordersPage.getNumber() + 1, // Convert 0-based to 1-based 
                    ordersPage.getSize(),
                    ordersPage.getTotalElements(),
                    ordersPage.getTotalPages(),
                    ordersPage.hasNext(),
                    ordersPage.hasPrevious()
            );
            
            System.out.println("Found " + ordersPage.getTotalElements() + " total orders, returning page " + page + " of " + ordersPage.getTotalPages());
            
            return ResponseEntity.ok(response);
            
        } catch (IllegalArgumentException e) {
            System.err.println("Bad request: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.badRequest().build();
        } catch (Exception e) {
            System.err.println("===== DETAILED ERROR =====");
            System.err.println("Error searching orders by date range: " + e.getMessage());
            System.err.println("Exception type: " + e.getClass().getName());
            System.err.println("Cause: " + (e.getCause() != null ? e.getCause().getMessage() : "null"));
            e.printStackTrace();
            System.err.println("===========================");
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Tìm kiếm đơn hàng theo khoảng thời gian với phân trang
     * API: GET /api/orders/search-by-date-paginated?storeId=xxx&fromDate=yyyy-MM-dd&toDate=yyyy-MM-dd&page=1&size=10
     */
    @GetMapping("/search-by-date-paginated")
    public ResponseEntity<PaginatedOrderSummaryResponseDto> searchOrdersByDateRangePaginated(
            @RequestParam Long storeId,
            @RequestParam(required = false) String fromDate,
            @RequestParam(required = false) String toDate,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        try {
            LocalDateTime fromDateTime = null;
            LocalDateTime toDateTime = null;
            
            // Parse fromDate nếu có
            if (fromDate != null && !fromDate.trim().isEmpty()) {
                try {
                    LocalDate parsedFromDate = LocalDate.parse(fromDate);
                    fromDateTime = parsedFromDate.atStartOfDay(); // 00:00:00 của ngày đó
                    System.out.println("Parsed fromDate: " + fromDateTime);
                } catch (Exception e) {
                    System.err.println("Invalid fromDate format: " + fromDate + " - " + e.getMessage());
                    return ResponseEntity.badRequest().build();
                }
            }
            
            // Parse toDate nếu có
            if (toDate != null && !toDate.trim().isEmpty()) {
                try {
                    LocalDate parsedToDate = LocalDate.parse(toDate);
                    toDateTime = parsedToDate.atTime(23, 59, 59); // 23:59:59 của ngày đó
                    System.out.println("Parsed toDate: " + toDateTime);
                } catch (Exception e) {
                    System.err.println("Invalid toDate format: " + toDate + " - " + e.getMessage());
                    return ResponseEntity.badRequest().build();
                }
            }
            
            System.out.println("Searching orders paginated with storeId: " + storeId + 
                    ", fromDateTime: " + fromDateTime + ", toDateTime: " + toDateTime + 
                    ", page: " + page + ", size: " + size);
            
            Page<OrderSummaryDTO> ordersPage = orderService.searchOrdersByStoreIdAndDateRangePaginated(storeId, fromDateTime, toDateTime, page, size);
            
            PaginatedOrderSummaryResponseDto response = new PaginatedOrderSummaryResponseDto(
                    ordersPage.getContent(),
                    ordersPage.getNumber() + 1, // Convert 0-based to 1-based 
                    ordersPage.getSize(),
                    ordersPage.getTotalElements(),
                    ordersPage.getTotalPages(),
                    ordersPage.hasNext(),
                    ordersPage.hasPrevious()
            );
            
            System.out.println("Found " + ordersPage.getTotalElements() + " total orders, returning page " + page + " of " + ordersPage.getTotalPages());
            
            return ResponseEntity.ok(response);
            
        } catch (IllegalArgumentException e) {
            System.err.println("Bad request: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.badRequest().build();
        } catch (Exception e) {
            System.err.println("===== DETAILED ERROR (PAGINATED) =====");
            System.err.println("Error searching orders by date range paginated: " + e.getMessage());
            System.err.println("Exception type: " + e.getClass().getName());
            System.err.println("Cause: " + (e.getCause() != null ? e.getCause().getMessage() : "null"));
            e.printStackTrace();
            System.err.println("===========================");
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Get order statistics by store ID from logged-in user
     * API: GET /api/orders/user/{userId}/stats
     * Returns total orders, processing orders, and completed orders count
     */
    @GetMapping("/user/{userId}/stats")
    public ResponseEntity<OrderStatsDto> getUserOrderStats(@PathVariable Long userId) {
        try {
            // First get the store ID associated with this user
            // For now, we'll assume storeId = userId for simplicity
            // In real implementation, you might need to get storeId from user entity
            Long storeId = userId; // This should be modified based on your user-store relationship
            
            System.out.println("Getting order stats for userId: " + userId + ", using storeId: " + storeId);
            
            OrderStatsDto stats = orderService.getOrderStatsByStoreId(storeId);
            
            System.out.println("Order stats: total=" + stats.getTotalOrders() + 
                    ", processing=" + stats.getProcessingOrders() + 
                    ", completed=" + stats.getCompletedOrders());
            
            return ResponseEntity.ok(stats);
            
        } catch (Exception e) {
            System.err.println("Error getting order stats: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

}
