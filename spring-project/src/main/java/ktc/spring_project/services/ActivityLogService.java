package ktc.spring_project.services;
import ktc.spring_project.exceptions.HttpException;

import ktc.spring_project.entities.ActivityLog;
import ktc.spring_project.entities.User;
import ktc.spring_project.enums.ActionType;
import ktc.spring_project.enums.ChecklistActionType;
import ktc.spring_project.repositories.ActivityLogRepository;
import ktc.spring_project.repositories.UserRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.List;

@Service
public class ActivityLogService {

    private final ActivityLogRepository activityLogRepository;
    
    @Autowired
    private UserService userService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ktc.spring_project.repositories.StatusRepository statusRepository;

    @Autowired
    public ActivityLogService(ActivityLogRepository activityLogRepository) {
        this.activityLogRepository = activityLogRepository;
    }
    public ActivityLog createActivityLog(ActivityLog activityLog) {
        return activityLogRepository.save(activityLog);
    }

    public ActivityLog getActivityLogById(Long id) {
        return activityLogRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("ActivityLog not found with id: " + id));
    }

    public void logUserActivity(Long userId, String action, String description) {
        try {
            // Chuyển đổi String action thành ActionType
            ActionType actionType = ActionType.valueOf(action);
            ActivityLog log = new ActivityLog(userId, actionType );
            // Lấy user từ userId để lấy role
            var user = userService.getUserById(userId);
            log.setRole(user.getRole());
                // Set default status if user status is null
                if (user.getStatus() != null) {
                    log.setStatus(user.getStatus());
                } else {
                    // Lấy status id=1 từ DB
                    ktc.spring_project.entities.Status status = statusRepository.findById((short)1)
                        .orElseThrow(() -> new RuntimeException("Status id=1 not found"));
                    log.setStatus(status);
                }
            // Có thể đặt thêm các trường nếu cần
            // log.setTableName("users");
            // log.setRecordId(userId);
            activityLogRepository.save(log);
        } catch (IllegalArgumentException e) {
            // Xử lý trường hợp action không hợp lệ
            throw new HttpException("Invalid action type: " + action, org.springframework.http.HttpStatus.BAD_REQUEST);
        }
    }

    public List<ActivityLog> getAllActivityLogs() {
        return activityLogRepository.findAll();
    }

    public ActivityLog updateActivityLog(Long id, ActivityLog activityLogDetails) {
        ActivityLog activityLog = getActivityLogById(id);
        activityLog.setActorId(activityLogDetails.getActorId());
        activityLog.setActor(activityLogDetails.getActor());
        activityLog.setRole(activityLogDetails.getRole());
        activityLog.setStatus(activityLogDetails.getStatus());
        activityLog.setActionType(activityLogDetails.getActionType());
        activityLog.setTableName(activityLogDetails.getTableName());
        activityLog.setRecordId(activityLogDetails.getRecordId());
        activityLog.setActionTimestamp(activityLogDetails.getActionTimestamp());
        activityLog.setMetadata(activityLogDetails.getMetadata());
        return activityLogRepository.save(activityLog);
    }

    // Utility methods cho relationships
    public List<ActivityLog> getActivityLogsByActor(Long actorId) {
        return activityLogRepository.findByActorId(actorId);
    }

    public List<ActivityLog> getActivityLogsByRole(Long roleId) {
        return activityLogRepository.findByRoleId(roleId);
    }

    public List<ActivityLog> getSystemActivityLogs() {
        return activityLogRepository.findByActorIdIsNull();
    }

    public void deleteActivityLog(Long id) {
        ActivityLog activityLog = getActivityLogById(id);
        activityLogRepository.delete(activityLog);
    }

    // ============ CHECKLIST LOGGING METHODS ============

    /**
     * Log một hành động checklist
     */
    public void logChecklistAction(Long userId, ChecklistActionType checklistAction, 
                                 String tableName, Long recordId, String details) {
        try {
            User user = userRepository.findById(userId).orElse(null);
            if (user == null) {
                return;
            }

            ActivityLog log = new ActivityLog();
            log.setActor(user);
            log.setRole(user.getRole());
            // Sử dụng CREATE action type cho checklist actions
            log.setActionType(ActionType.CREATE);
            log.setTableName(tableName);
            log.setRecordId(recordId);
            log.setMetadata(details != null ? details : checklistAction.getDescription());
            log.setActionTimestamp(new Timestamp(System.currentTimeMillis()));
            
            // Set status mặc định
            if (user.getStatus() != null) {
                log.setStatus(user.getStatus());
            } else {
                ktc.spring_project.entities.Status status = statusRepository.findById((short)1)
                    .orElseThrow(() -> new RuntimeException("Status id=1 not found"));
                log.setStatus(status);
            }

            activityLogRepository.save(log);
        } catch (Exception e) {
            // Log error but don't break the main flow
            System.err.println("Error logging checklist action: " + e.getMessage());
        }
    }

    /**
     * Log customer actions
     */
    public void logCustomerRegister(Long customerId) {
        logChecklistAction(customerId, ChecklistActionType.CUSTOMER_ACCOUNT_CREATED, "users", customerId, 
                         "Khách hàng đăng ký tài khoản thành công");
    }

    public void logCustomerFirstOrder(Long customerId, Long orderId) {
        logChecklistAction(customerId, ChecklistActionType.CUSTOMER_FIRST_ORDER_CREATED, "orders", orderId,
                         "Khách hàng tạo đơn hàng đầu tiên");
    }

    public void logCustomerPayment(Long customerId, Long orderId, Long paymentId) {
        logChecklistAction(customerId, ChecklistActionType.CUSTOMER_PAYMENT_COMPLETED, "payments", paymentId,
                         "Khách hàng hoàn thành thanh toán cho đơn hàng #" + orderId);
    }

    public void logCustomerReceiveOrder(Long customerId, Long orderId) {
        logChecklistAction(customerId, ChecklistActionType.CUSTOMER_ORDER_RECEIVED, "orders", orderId,
                         "Khách hàng nhận hàng thành công");
    }

    public void logCustomerReview(Long customerId, Long orderId) {
        logChecklistAction(customerId, ChecklistActionType.CUSTOMER_SERVICE_REVIEWED, "orders", orderId,
                         "Khách hàng đánh giá dịch vụ");
    }

    /**
     * Log dispatcher actions
     */
    public void logDispatcherLogin(Long dispatcherId) {
        logChecklistAction(dispatcherId, ChecklistActionType.DISPATCHER_SYSTEM_LOGIN, "users", dispatcherId,
                         "Dispatcher đăng nhập hệ thống");
    }

    public void logDispatcherProcessOrder(Long dispatcherId, Long orderId) {
        logChecklistAction(dispatcherId, ChecklistActionType.DISPATCHER_FIRST_ORDER_PROCESSED, "orders", orderId,
                         "Dispatcher xử lý đơn hàng");
    }

    public void logDispatcherAssignDriver(Long dispatcherId, Long deliveryId, Long driverId, Long vehicleId) {
        logChecklistAction(dispatcherId, ChecklistActionType.DISPATCHER_DRIVER_ASSIGNED, "deliveries", deliveryId,
                         "Dispatcher phân công tài xế #" + driverId + " và gán xe #" + vehicleId);
    }

    // Overload method cho backward compatibility
    public void logDispatcherAssignDriver(Long dispatcherId, Long deliveryId, Long driverId) {
        logChecklistAction(dispatcherId, ChecklistActionType.DISPATCHER_DRIVER_ASSIGNED, "deliveries", deliveryId,
                         "Dispatcher phân công tài xế #" + driverId);
    }

    public void logDispatcherOptimizeRoute(Long dispatcherId, Long routeId) {
        logChecklistAction(dispatcherId, ChecklistActionType.DISPATCHER_ROUTE_OPTIMIZED, "routes", routeId,
                         "Dispatcher tối ưu hóa tuyến đường");
    }

    public void logDispatcherTrackDelivery(Long dispatcherId, Long deliveryId) {
        logChecklistAction(dispatcherId, ChecklistActionType.DISPATCHER_DELIVERY_TRACKED, "deliveries", deliveryId,
                         "Dispatcher theo dõi giao hàng");
    }

    /**
     * Log driver actions
     */
    public void logDriverLogin(Long driverId) {
        logChecklistAction(driverId, ChecklistActionType.DRIVER_MOBILE_LOGIN, "users", driverId,
                         "Driver đăng nhập ứng dụng mobile");
    }

    public void logDriverReceiveDelivery(Long driverId, Long deliveryId) {
        logChecklistAction(driverId, ChecklistActionType.DRIVER_FIRST_DELIVERY_RECEIVED, "deliveries", deliveryId,
                         "Driver nhận đơn giao hàng");
    }

    public void logDriverUpdateLocation(Long driverId, Long trackingId) {
        logChecklistAction(driverId, ChecklistActionType.DRIVER_LOCATION_UPDATED, "delivery_tracking", trackingId,
                         "Driver cập nhật vị trí GPS");
    }

    public void logDriverCompleteDelivery(Long driverId, Long deliveryId) {
        logChecklistAction(driverId, ChecklistActionType.DRIVER_DELIVERY_COMPLETED, "deliveries", deliveryId,
                         "Driver hoàn thành giao hàng");
    }

    public void logDriverUploadProof(Long driverId, Long proofId) {
        logChecklistAction(driverId, ChecklistActionType.DRIVER_PROOF_UPLOADED, "delivery_proofs", proofId,
                         "Driver tải ảnh chứng minh giao hàng");
    }

    /**
     * Check if user has completed a specific checklist action
     */
    public boolean hasUserCompletedAction(Long userId, ChecklistActionType actionType) {
        try {
            return activityLogRepository.existsByActorIdAndActionContaining(userId, actionType.getActionCode());
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Get completion count for a specific action type
     */
    public long getActionCompletionCount(Long userId, ChecklistActionType actionType) {
        try {
            return activityLogRepository.countByActorId(userId);
        } catch (Exception e) {
            return 0;
        }
    }
}