package ktc.spring_project.services;

import ktc.spring_project.dtos.ChecklistProgressResponse;

import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.entities.Status;
import ktc.spring_project.repositories.OrderRepository;
import ktc.spring_project.repositories.UserRepository;
import ktc.spring_project.repositories.VehicleRepository;
import ktc.spring_project.repositories.ChecklistProgressRepository;
import ktc.spring_project.repositories.ChecklistStepRepository;
import ktc.spring_project.entities.ChecklistStep;
import ktc.spring_project.entities.ChecklistProgress;
// Đã loại bỏ ChecklistService
import ktc.spring_project.dtos.timeline.OrderTimelineResponse;
import ktc.spring_project.dtos.timeline.OrderStatusDto;
import ktc.spring_project.dtos.timeline.ActorDto;
import ktc.spring_project.exceptions.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import java.util.*;

@Slf4j
@Service
public class DispatcherService {
    // Helper method: chuyển Order sang OrderTimelineResponse
    public OrderTimelineResponse convertToOrderTimelineResponse(Order order) {
        if (order == null) return null;
        OrderTimelineResponse response = new OrderTimelineResponse();
        response.setOrderId(order.getId());
        response.setOrderCode(order.getOrderCode());
        response.setCreatedAt(order.getCreatedAt() != null ? order.getCreatedAt().toLocalDateTime() : null);
        response.setUpdatedAt(order.getUpdatedAt() != null ? order.getUpdatedAt().toLocalDateTime() : null);
        // Set order status
        if (order.getStatus() != null) {
            OrderStatusDto orderStatus = OrderStatusDto.builder()
                .statusId(order.getStatus().getId())
                .statusName(order.getStatus().getName())
                .statusDescription(order.getStatus().getDescription())
                .build();
            response.setOrderStatus(orderStatus);
        }
        // Set customer info
        if (order.getCreatedBy() != null) {
            ActorDto customer = ActorDto.builder()
                .userId(order.getCreatedBy().getId())
                .fullName(order.getCreatedBy().getFullName())
                .role("CUSTOMER")
                .phone(order.getCreatedBy().getPhone())
                .build();
            response.setCustomer(customer);
        }
        // Set driver info (if assigned)
        if (order.getDriver() != null) {
            ActorDto driver = ActorDto.builder()
                .userId(order.getDriver().getId())
                .fullName(order.getDriver().getFullName())
                .role("DRIVER")
                .phone(order.getDriver().getPhone())
                .build();
            response.setDriver(driver);
        }
        // TODO: Set timeline steps nếu cần
        return response;
    }
    @Autowired
    private ChecklistService checklistService;
    @Autowired
    private OrderRepository orderRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private VehicleRepository vehicleRepository;
    
    @Autowired
    private StatusService statusService;

    @Autowired
    private ChecklistProgressRepository checklistProgressRepository;

    @Autowired
    private ChecklistStepRepository checklistStepRepository;

    /**
     * Gán tài xế và xe cho đơn hàng, đồng thời cập nhật checklist_progress
     */
    public Map<String, Object> assignDriverAndVehicle(Long orderId, Long driverId, Long vehicleId, Long dispatcherId) {
        log.info("=== assignDriverAndVehicle START === orderId={}, driverId={}, vehicleId={}, dispatcherId={}", orderId, driverId, vehicleId, dispatcherId);
        Map<String, Object> result = new HashMap<>();
        try {
            // ...existing code...
            if (dispatcherId == null) throw new EntityNotFoundException("Dispatcher ID is required");
            if (orderId == null) throw new EntityNotFoundException("Order ID is required");
            if (driverId == null) throw new EntityNotFoundException("Driver ID is required");
            if (vehicleId == null) throw new EntityNotFoundException("Vehicle ID is required");

            Order order = orderRepository.findById(orderId)
                    .orElseThrow(() -> new EntityNotFoundException("Order not found with id: " + orderId));
            User driver = userRepository.findById(driverId)
                    .orElseThrow(() -> new EntityNotFoundException("Driver not found with id: " + driverId));
            Vehicle vehicle = vehicleRepository.findById(vehicleId)
                    .orElseThrow(() -> new EntityNotFoundException("Vehicle not found with id: " + vehicleId));

            order.setDriver(driver);
            order.setVehicle(vehicle);

            // Cập nhật status sang "Processing"
            Optional<Status> processingStatus = statusService.getStatusByTypeAndName("ORDER", "Processing");
            if (processingStatus.isPresent()) {
                order.setStatus(processingStatus.get());
            } else {
                log.error("Không tìm thấy status 'Processing', giữ nguyên status hiện tại!");
            }

            orderRepository.save(order);

            // Ghi log checklist bằng ChecklistService
            String details = "Dispatcher assigned driver " + driverId + " and vehicle " + vehicleId;
            checklistService.markStepCompleted(dispatcherId, orderId, "DISPATCHER_ASSIGN_DRIVER", details);

            // Trả về cả timeline và log checklist
            OrderTimelineResponse timeline = convertToOrderTimelineResponse(order);
            result.put("timeline", timeline);
            result.put("message", "Gán tài xế và xe thành công!");
            // Trả về log checklist cho order (bao gồm cả bước gán xe/tài xế)
            ChecklistProgressResponse checklistLog = checklistService.getProgressByOrder(orderId);
            result.put("checklistLog", checklistLog);
            return result;
        } catch (Exception e) {
            log.error("ERROR in assignDriverAndVehicle: {}", e.getMessage(), e);
            result.put("error", e.getMessage());
            return result;
        }
    }
}
