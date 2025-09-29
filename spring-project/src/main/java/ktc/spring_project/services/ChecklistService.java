package ktc.spring_project.services;

import ktc.spring_project.enums.ChecklistActionType;
import ktc.spring_project.dtos.ChecklistProgressResponse;
import ktc.spring_project.dtos.ChecklistStepResponse;
import ktc.spring_project.dtos.timeline.*;
import ktc.spring_project.entities.*;
import ktc.spring_project.repositories.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Comparator;
import java.util.stream.Collectors;
@Service
public class ChecklistService {
    /**
     * Lấy tất cả các bước checklist chuẩn (không phân biệt role)
     * Dành cho FE hiển thị timeline với đầy đủ các bước
     */
    public List<ChecklistStepResponse> getAllChecklistSteps() {
        List<ChecklistStep> steps = checklistStepRepository.findAll();
        List<ChecklistStepResponse> result = new ArrayList<>();
        for (ChecklistStep step : steps) {
            ChecklistStepResponse resp = new ChecklistStepResponse();
            resp.setStepCode(step.getStepCode());
            resp.setStepName(step.getStepName());
            resp.setDescription(step.getDescription());
            resp.setStepOrder(step.getStepOrder());
            result.add(resp);
        }
        result.sort(Comparator.comparing(ChecklistStepResponse::getStepOrder));
        return result;
    }
// ...existing code...
    // Trả về tiến trình checklist cho user và order cụ thể
    public ChecklistProgressResponse getChecklistProgress(Long userId, Long orderId) {
        Order order = orderRepository.findById(orderId).orElse(null);
        if (order == null) {
            ChecklistProgressResponse response = new ChecklistProgressResponse();
            response.setStatus("error");
            response.setMessage("Order not found with ID: " + orderId);
            return response;
        }
        User user = userRepository.findById(userId).orElse(null);
        if (user == null) {
            ChecklistProgressResponse response = new ChecklistProgressResponse();
            response.setStatus("error");
            response.setMessage("User not found with ID: " + userId);
            return response;
        }
        List<ChecklistProgress> progresses = checklistProgressRepository.findByOrder(order);
        List<ChecklistStepResponse> steps = new ArrayList<>();
        for (ChecklistProgress progress : progresses) {
            ChecklistStep step = progress.getStep();
            ChecklistStepResponse stepResp = new ChecklistStepResponse();
            stepResp.setStepCode(step.getStepCode());
            stepResp.setStepName(step.getStepName());
            stepResp.setDescription(step.getDescription());
            stepResp.setStepOrder(step.getStepOrder());
            stepResp.setCompleted(progress.getCompleted() != null ? progress.getCompleted() : false);
            stepResp.setStepStatus(progress.getCompleted() ? "Completed" : "Pending");
            stepResp.setCompletionDetails(progress.getDetails());
            stepResp.setRequired(true);
            steps.add(stepResp);
        }
        ChecklistProgressResponse response = new ChecklistProgressResponse();
        response.setUserId(userId);
        response.setUserRole(user.getRole().getRoleName());
        response.setUserName(user.getFullName());
        response.setSteps(steps);
        response.setTotalSteps(steps.size());
        response.setCompletedSteps((int) steps.stream().filter(ChecklistStepResponse::isCompleted).count());
        response.calculateCompletionPercentage();
        response.setStatus("success");
        response.setMessage("Lấy tiến trình checklist thành công!");
        return response;
    }
    // Logger for writing checklist logs to a file
    private static final org.slf4j.Logger checklistLogger = org.slf4j.LoggerFactory.getLogger("ChecklistLog");
    /**
     * Tìm ChecklistActionType theo stepCode (enum lookup)
     */
    private ChecklistActionType findActionTypeByStepCode(String stepCode) {
        if (stepCode == null) return null;
        for (ChecklistActionType type : ChecklistActionType.values()) {
            if (type.name().equalsIgnoreCase(stepCode) || type.getActionCode().equalsIgnoreCase(stepCode)) {
                return type;
            }
        }
        return null;
    }
    @Autowired
    private ChecklistStepRepository checklistStepRepository;

    @Autowired
    private ChecklistProgressRepository checklistProgressRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private DeliveryRepository deliveryRepository;

    @Autowired
    private PaymentRepository paymentRepository;

    @Autowired
    private ActivityLogRepository activityLogRepository;

    @Autowired
    private DeliveryTrackingRepository deliveryTrackingRepository;

    @Autowired
    private ActivityLogService activityLogService;

    @Autowired
    private StatusService statusService;


    /**
     * Lấy checklist cho tất cả đơn hàng đang hoạt động của customer
     * Đơn hàng đang hoạt động: chưa giao thành công hoặc chưa thanh toán thành công
     */
    public List<Map<String, Object>> getActiveOrdersChecklist(Long customerId) {
        List<Map<String, Object>> result = new ArrayList<>();
        if (customerId == null) return result;

        // Lấy các status hợp lệ
        List<String> validOrderStatuses = statusService.getStatusesByType("ORDER").stream().map(Status::getName).collect(Collectors.toList());
        List<String> validPaymentStatuses = statusService.getStatusesByType("PAYMENT").stream().map(Status::getName).collect(Collectors.toList());

        // Lấy tất cả đơn hàng của customer
        List<Order> orders = orderRepository.findByCreatedBy_Id(customerId);
        for (Order order : orders) {
            String orderStatus = (order.getStatus() != null && order.getStatus().getName() != null) ? order.getStatus().getName() : "";
            // Kiểm tra đã giao thành công chưa
            boolean delivered = "Delivered".equalsIgnoreCase(orderStatus);

            // Kiểm tra đã thanh toán thành công chưa
            List<Payment> payments = paymentRepository.findByOrderId(order.getId());
            boolean paid = false;
            String paymentStatus = "Not paid yet";
            if (!payments.isEmpty()) {
                payments.sort((p1, p2) -> p2.getCreatedAt().compareTo(p1.getCreatedAt()));
                Payment latestPayment = payments.get(0);
                if (latestPayment.getStatus() != null && latestPayment.getStatus().getName() != null && validPaymentStatuses.contains(latestPayment.getStatus().getName())) {
                    paymentStatus = latestPayment.getStatus().getName();
                    paid = "Paid".equalsIgnoreCase(paymentStatus) || "Completed".equalsIgnoreCase(paymentStatus);
                }
            }

            // Nếu chưa giao thành công hoặc chưa thanh toán thành công thì là đơn đang hoạt động
            if (!delivered || !paid) {
                Map<String, Object> orderChecklist = new HashMap<>();
                orderChecklist.put("orderId", order.getId());
                orderChecklist.put("orderCode", order.getOrderCode());
                orderChecklist.put("createdAt", order.getCreatedAt());
                orderChecklist.put("orderStatus", delivered ? orderStatus : "Not received yet");
                orderChecklist.put("paymentStatus", paid ? paymentStatus : "Not paid yet");
                orderChecklist.put("orderStepStatus", validOrderStatuses.contains(orderStatus) ? orderStatus : "Not ordered yet");
                result.add(orderChecklist);
            }
        }
        return result;
    }

    /**
     * Lấy tiến trình checklist cho khách hàng
     */
    public ChecklistProgressResponse getCustomerProgress(Long customerId) {
        User user = userRepository.findById(customerId).orElse(null);
        if (user == null) {
            ChecklistProgressResponse response = new ChecklistProgressResponse();
            response.setStatus("error");
            response.setMessage("Customer not found with ID: " + customerId);
            return response;
        }

    ChecklistProgressResponse response = new ChecklistProgressResponse(customerId, "CUSTOMER", user.getFullName());
    List<ChecklistStepResponse> steps = getCustomerSteps(customerId);
    response.setSteps(steps);
    response.setTotalSteps(steps.size());
    response.setCompletedSteps((int) steps.stream().filter(ChecklistStepResponse::isCompleted).count());
    response.calculateCompletionPercentage();
    response.setStatus("success");
    // Thêm danh sách đơn còn hoạt động
    response.setActiveOrders(getActiveOrdersChecklist(customerId));
    return response;
    }

    /**
     * Lấy tiến trình checklist cho dispatcher
     */
    public ChecklistProgressResponse getDispatcherProgress(Long dispatcherId) {
        User user = userRepository.findById(dispatcherId).orElse(null);
        if (user == null) {
            ChecklistProgressResponse response = new ChecklistProgressResponse();
            response.setStatus("error");
            response.setMessage("Dispatcher not found with ID: " + dispatcherId);
            return response;
        }

        ChecklistProgressResponse response = new ChecklistProgressResponse(dispatcherId, "DISPATCHER", user.getFullName());
        List<ChecklistStepResponse> steps = getDispatcherSteps(dispatcherId);
        
        response.setSteps(steps);
        response.setTotalSteps(steps.size());
        response.setCompletedSteps((int) steps.stream().filter(ChecklistStepResponse::isCompleted).count());
        response.calculateCompletionPercentage();
        response.setStatus("success");
        
        return response;
    }

    /**
     * Lấy tiến trình checklist cho tài xế
     */
    public ChecklistProgressResponse getDriverProgress(Long driverId) {
        User user = userRepository.findById(driverId).orElse(null);
        if (user == null) {
            ChecklistProgressResponse response = new ChecklistProgressResponse();
            response.setStatus("error");
            response.setMessage("Driver not found with ID: " + driverId);
            return response;
        }

        ChecklistProgressResponse response = new ChecklistProgressResponse(driverId, "DRIVER", user.getFullName());
        List<ChecklistStepResponse> steps = getDriverSteps(driverId);
        
        response.setSteps(steps);
        response.setTotalSteps(steps.size());
        response.setCompletedSteps((int) steps.stream().filter(ChecklistStepResponse::isCompleted).count());
        response.calculateCompletionPercentage();
        response.setStatus("success");
        
        return response;
    }

    /**
     * Lấy tiến trình cho user tự động theo role
     */
    public ChecklistProgressResponse getUserProgress(Long userId) {
        User user = userRepository.findById(userId).orElse(null);
        if (user == null) {
            ChecklistProgressResponse response = new ChecklistProgressResponse();
            response.setStatus("error");
            response.setMessage("User not found with ID: " + userId);
            return response;
        }

        String roleName = user.getRole().getRoleName();
        switch (roleName) {
            case "CUSTOMER":
                return getCustomerProgress(userId);
            case "DISPATCHER":
                return getDispatcherProgress(userId);
            case "DRIVER":
                return getDriverProgress(userId);
            default:
                ChecklistProgressResponse response = new ChecklistProgressResponse();
                response.setStatus("error");
                response.setMessage("Role not supported: " + roleName);
                return response;
        }
    }

    /**
     * Lấy danh sách các bước checklist theo role
     */
    public List<ChecklistStepResponse> getChecklistStepsByRole(String role) {
    // Lấy tất cả các bước checklist theo role từ DB
    List<ChecklistStep> steps = checklistStepRepository.findAll()
        .stream().filter(s -> s.getRole().equalsIgnoreCase(role)).collect(Collectors.toList());
    List<ChecklistStepResponse> result = new ArrayList<>();
    for (ChecklistStep step : steps) {
        ChecklistStepResponse resp = new ChecklistStepResponse();
        resp.setStepCode(step.getStepCode());
        resp.setStepName(step.getStepName());
        resp.setDescription(step.getDescription());
        resp.setStepOrder(step.getStepOrder());
        // Chưa có trạng thái completed vì chưa truyền userId/orderId
        result.add(resp);
    }
    return result;
    }

    /**
     * Đánh dấu một bước đã hoàn thành
     */
    public void markStepCompleted(Long userId, Long orderId, String stepCode, String details) {
        checklistLogger.info("[CHECKLIST] markStepCompleted called | userId={} | orderId={} | stepCode={} | details={}", userId, orderId, stepCode, details);
        checklistLogger.info("[CHECKLIST] DEBUG | userId={} | orderId={} | stepCode={} | details={} | Thread={}", userId, orderId, stepCode, details, Thread.currentThread().getName());
        
        // CRITICAL DEBUG: Log stack trace để xem ai gọi method này
        checklistLogger.info("[CHECKLIST] STACK TRACE for stepCode={} orderId={}:", stepCode, orderId);
        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        for (int i = 0; i < Math.min(10, stackTrace.length); i++) {
            checklistLogger.info("[CHECKLIST] STACK[{}]: {}", i, stackTrace[i]);
        }
        try {
            // Log input types
            checklistLogger.info("[CHECKLIST] Input types | userId:{}({}), orderId:{}({}), stepCode:{}({}), details:{}({})",
                userId, userId != null ? userId.getClass().getSimpleName() : "null",
                orderId, orderId != null ? orderId.getClass().getSimpleName() : "null",
                stepCode, stepCode != null ? stepCode.getClass().getSimpleName() : "null",
                details, details != null ? details.getClass().getSimpleName() : "null");

            User user = userRepository.findById(userId).orElse(null);
            Order order = orderRepository.findById(orderId).orElse(null);
            List<ChecklistStep> allSteps = checklistStepRepository.findAll();
            checklistLogger.info("[CHECKLIST] DEBUG | allSteps.size={} | allStepCodes={}", allSteps.size(), allSteps.stream().map(ChecklistStep::getStepCode).toList());
            ChecklistStep step = allSteps.stream().filter(s -> s.getStepCode().equals(stepCode)).findFirst().orElse(null);
            checklistLogger.info("[CHECKLIST] DEBUG | step found={} | stepCode={} | stepName={}", step != null, step != null ? step.getStepCode() : "NULL", step != null ? step.getStepName() : "NULL");

            checklistLogger.info("[CHECKLIST] Entity state | user:{} | order:{} | step:{}", user != null ? user.getId() : "NULL", order != null ? order.getId() : "NULL", step != null ? step.getStepCode() : "NULL");

            if (user == null) checklistLogger.error("[CHECKLIST] User not found | userId={}", userId);
            if (order == null) checklistLogger.error("[CHECKLIST] Order not found | orderId={}", orderId);
            if (step == null) checklistLogger.error("[CHECKLIST] Step not found | stepCode={}", stepCode);

            if (user == null || order == null || step == null) {
                checklistLogger.error("[CHECKLIST] ERROR: User, Order, or Step not found | userId={} | orderId={} | stepCode={}", userId, orderId, stepCode);
                if (step == null) {
                    checklistLogger.error("[CHECKLIST] Step with stepCode '{}' does not exist in DB", stepCode);
                    checklistLogger.error("[CHECKLIST] DEBUG | allStepCodes={}", allSteps.stream().map(ChecklistStep::getStepCode).toList());
                }
                return;
            }

            // Tìm log trùng orderId và stepCode (bất kể user)
            List<ChecklistProgress> progresses = checklistProgressRepository.findByOrder(order);
            checklistLogger.info("[CHECKLIST] DEBUG | progresses.size={} | progressesStepCodes={}", progresses.size(), progresses.stream().map(p -> p.getStep().getStepCode()).toList());
            checklistLogger.info("[CHECKLIST] Existing progresses for orderId={}: {}", orderId, progresses.stream().map(p -> p.getStep().getStepCode()).toList());
            ChecklistProgress progress = progresses.stream().filter(p -> p.getStep().equals(step)).findFirst().orElse(null);
            checklistLogger.info("[CHECKLIST] DEBUG | progress found={} | progressId={}", progress != null, progress != null ? progress.getId() : "NULL");
            if (progress == null) {
                progress = new ChecklistProgress();
                checklistLogger.info("[CHECKLIST] Created new ChecklistProgress entity for orderId={} stepCode={}", orderId, stepCode);
            } else {
                checklistLogger.info("[CHECKLIST] Overwriting ChecklistProgress entity for orderId={} stepCode={}", orderId, stepCode);
            }
            // Ghi đè user, details, completedAt
            progress.setUser(user);
            progress.setOrder(order);
            progress.setStep(step);
            progress.setCompleted(true);
            progress.setCompletedAt(java.time.LocalDateTime.now());
            progress.setDetails(details);
            ChecklistProgress saved = checklistProgressRepository.save(progress);
            checklistLogger.info("[CHECKLIST] DEBUG | saved progressId={} | saved stepCode={}", saved.getId(), saved.getStep().getStepCode());
            checklistLogger.info("[CHECKLIST] Saved ChecklistProgress with ID: {} for orderId={} stepCode={}", saved.getId(), orderId, stepCode);
            checklistLogger.info("[CHECKLIST] === markStepCompleted COMPLETED ===");
        } catch (Exception e) {
            checklistLogger.error("[CHECKLIST] ERROR in markStepCompleted: {}", e.getMessage(), e);
        }
    }

    /**
     * Lấy danh sách user chưa hoàn thành checklist
     */
    public List<Long> getIncompleteUsers(String role) {
        List<User> users = userRepository.findByRole_RoleName(role.toUpperCase());
        List<Long> incompleteUserIds = new ArrayList<>();
        
        for (User user : users) {
            ChecklistProgressResponse progress = getUserProgress(user.getId());
            if (progress.getCompletionPercentage() < 100.0) {
                incompleteUserIds.add(user.getId());
            }
        }
        
        return incompleteUserIds;
    }

    /**
     * Lấy thống kê checklist theo role
     */
    public Map<String, Object> getChecklistStatsByRole(String role) {
        List<User> users = userRepository.findByRole_RoleName(role.toUpperCase());
        Map<String, Object> stats = new HashMap<>();
        
        int totalUsers = users.size();
        int completedUsers = 0;
        double averageProgress = 0.0;
        
        for (User user : users) {
            ChecklistProgressResponse progress = getUserProgress(user.getId());
            if (progress.getCompletionPercentage() >= 100.0) {
                completedUsers++;
            }
            averageProgress += progress.getCompletionPercentage();
        }
        
        if (totalUsers > 0) {
            averageProgress = averageProgress / totalUsers;
        }
        
        stats.put("totalUsers", totalUsers);
        stats.put("completedUsers", completedUsers);
        stats.put("incompleteUsers", totalUsers - completedUsers);
        stats.put("averageProgress", Math.round(averageProgress * 100.0) / 100.0);
        stats.put("completionRate", totalUsers > 0 ? Math.round((double) completedUsers / totalUsers * 100 * 100.0) / 100.0 : 0.0);
        
        return stats;
    }


    // ============== PRIVATE METHODS ==============

    /**
     * Lấy checklist động từ DB, join trạng thái hoàn thành cho user
     */
    public List<ChecklistStepResponse> getChecklistForUser(Long userId) {
    // Lấy role của user
    User user = userRepository.findById(userId).orElse(null);
    if (user == null) return new ArrayList<>();
    String role = user.getRole().getRoleName();
    List<ChecklistStep> steps = checklistStepRepository.findAll().stream()
        .filter(s -> s.getRole().equalsIgnoreCase(role)).collect(Collectors.toList());
    // Lấy progress của user
    List<ChecklistProgress> progresses = checklistProgressRepository.findAll().stream()
        .filter(p -> p.getUser() != null && p.getUser().getId().equals(userId)).collect(Collectors.toList());
    Map<String, Boolean> completedMap = progresses.stream()
        .collect(Collectors.toMap(p -> p.getStep().getStepCode(), ChecklistProgress::getCompleted));
    List<ChecklistStepResponse> result = new ArrayList<>();
        for (ChecklistStep step : steps) {
            ChecklistStepResponse resp = new ChecklistStepResponse();
            resp.setStepCode(step.getStepCode());
            resp.setStepName(step.getStepName());
            resp.setDescription(step.getDescription());
            resp.setStepOrder(step.getStepOrder());
            resp.setCompleted(completedMap.getOrDefault(step.getStepCode(), false));

            // Nếu là bước tạo đơn hàng, thêm substeps là danh sách đơn hàng của user
            if ("CUSTOMER_CREATE_ORDER".equalsIgnoreCase(step.getStepCode())) {
                List<Order> orders = orderRepository.findByCreatedBy_Id(userId);
                List<java.util.Map<String, Object>> substeps = new ArrayList<>();
                for (Order order : orders) {
                    java.util.Map<String, Object> sub = new java.util.HashMap<>();
                    sub.put("orderId", order.getId());
                    sub.put("orderCode", order.getOrderCode());
                    String orderStatus = (order.getStatus() != null && order.getStatus().getName() != null) ? order.getStatus().getName() : "";
                    sub.put("orderStatus", orderStatus.isEmpty() ? "Not received yet" : orderStatus);
                    // Lấy paymentStatus
                    List<Payment> payments = paymentRepository.findByOrderId(order.getId());
                    String paymentStatus = "Not paid yet";
                    if (!payments.isEmpty()) {
                        payments.sort((p1, p2) -> p2.getCreatedAt().compareTo(p1.getCreatedAt()));
                        Payment latestPayment = payments.get(0);
                        if (latestPayment.getStatus() != null && latestPayment.getStatus().getName() != null) {
                            paymentStatus = latestPayment.getStatus().getName();
                        }
                    }
                    sub.put("paymentStatus", paymentStatus);
                    substeps.add(sub);
                }
                resp.setSubsteps(substeps);
            }
            result.add(resp);
        }
        return result;
    }

    /**
     * Lấy các bước checklist cho customer
     */
    /**
     * Lấy checklist cho customer - sử dụng unified logic
     */
    private List<ChecklistStepResponse> getCustomerSteps(Long customerId) {
    return getChecklistStepsByRole(customerId, "CUSTOMER");
    }

    /**
     * Lấy checklist cho dispatcher - sử dụng unified logic
     */
    private List<ChecklistStepResponse> getDispatcherSteps(Long dispatcherId) {
        return getChecklistStepsByRole(dispatcherId, "DISPATCHER");
    }

    // Removed duplicate findActionTypeByStepCode method

    /**
     * Lấy các bước checklist cho driver
     */
    private List<ChecklistStepResponse> getDriverSteps(Long driverId) {
    return getChecklistStepsByRole(driverId, "DRIVER");
    }

    /**
     * UNIFIED METHOD: Lấy checklist steps theo role, kết hợp 3 bảng:
     * - checklist_step: định nghĩa các bước
     * - checklist_progress: log thao tác đã thực hiện  
     * - status: trạng thái business data
     */
    private List<ChecklistStepResponse> getChecklistStepsByRole(Long userId, String role) {
        if (userId == null || role == null) {
            return new ArrayList<>();
        }

        // ✅ 1. Lấy định nghĩa bước từ checklist_step
        List<ChecklistStep> steps = checklistStepRepository.findAll().stream()
            .filter(s -> s.getRole().equalsIgnoreCase(role))
            .sorted((s1, s2) -> Integer.compare(s1.getStepOrder(), s2.getStepOrder()))
            .collect(Collectors.toList());

        // ✅ 2. Lấy log thao tác từ checklist_progress
        List<ChecklistProgress> progresses = checklistProgressRepository.findAll().stream()
            .filter(p -> p.getUser() != null && p.getUser().getId().equals(userId))
            .collect(Collectors.toList());
        Map<String, ChecklistProgress> progressMap = progresses.stream()
            .collect(Collectors.toMap(p -> p.getStep().getStepCode(), p -> p));

        // ✅ 3. Kết hợp data và set business status
        return steps.stream().map(step -> {
            ChecklistStepResponse response = new ChecklistStepResponse();
            response.setStepCode(step.getStepCode());
            response.setStepName(step.getStepName());
            response.setDescription(step.getDescription());
            response.setStepOrder(step.getStepOrder());

            // ✅ Set completed từ checklist_progress (SINGLE SOURCE OF TRUTH)
            ChecklistProgress progress = progressMap.get(step.getStepCode());
            response.setCompleted(progress != null && progress.getCompleted());
            response.setCompletionDetails(progress != null ? progress.getDetails() : null);

            // ✅ Set stepStatus từ business data (orders, payments, deliveries)
            setStepStatusFromBusinessData(response, userId, role);

            return response;
        }).collect(Collectors.toList());
    }

    /**
     * Set stepStatus dựa trên business data từ các bảng orders, payments, deliveries
     * Sử dụng bảng status để lấy trạng thái chuẩn
     */
    private void setStepStatusFromBusinessData(ChecklistStepResponse response, Long userId, String role) {
        String stepCode = response.getStepCode();
        
        try {
            // Lấy các status hợp lệ từ DB
            List<String> validOrderStatuses = statusService.getStatusesByType("ORDER").stream().map(Status::getName).collect(Collectors.toList());
            List<String> validPaymentStatuses = statusService.getStatusesByType("PAYMENT").stream().map(Status::getName).collect(Collectors.toList());
            List<String> validDeliveryStatuses = statusService.getStatusesByType("DELIVERY").stream().map(Status::getName).collect(Collectors.toList());

            switch (stepCode) {
                case "CUSTOMER_CREATE_ORDER":
                    setCustomerOrderStatus(response, userId, validOrderStatuses);
                    break;
                case "CUSTOMER_PAYMENT":
                    setCustomerPaymentStatus(response, userId, validPaymentStatuses);
                    break;
                case "DISPATCHER_RECEIVE_ORDER":
                case "DISPATCHER_ASSIGN_DRIVER":
                case "DISPATCHER_RECEIVE_RESULT":
                    setDispatcherStatus(response, userId, stepCode);
                    break;
                case "DRIVER_RECEIVE_ORDER":
                case "DRIVER_START_DELIVERY":
                case "DRIVER_COMPLETE_DELIVERY":
                    setDriverStatus(response, userId, stepCode);
                    break;
                default:
                    response.setStepStatus("Ready");
                    break;
            }
        } catch (Exception e) {
            // Fallback if business data lookup fails
            response.setStepStatus("Unknown");
        }
    }

    private void setCustomerOrderStatus(ChecklistStepResponse response, Long userId, List<String> validOrderStatuses) {
        List<Order> orders = orderRepository.findByCreatedBy_Id(userId);
        if (orders.isEmpty()) {
            response.setStepStatus("Not ordered yet");
            return;
        }
        
        orders.sort((o1, o2) -> o2.getCreatedAt().compareTo(o1.getCreatedAt()));
        Order latestOrder = orders.get(0);
        
        if (latestOrder.getStatus() != null && latestOrder.getStatus().getName() != null 
            && validOrderStatuses.contains(latestOrder.getStatus().getName())) {
            response.setStepStatus(latestOrder.getStatus().getName());
            
            // Add substeps with active orders
            List<Map<String, Object>> activeOrders = getActiveOrdersChecklist(userId);
            List<Map<String, Object>> substeps = new ArrayList<>();
            for (Map<String, Object> order : activeOrders) {
                Map<String, Object> substep = new HashMap<>(order);
                substep.remove("completed");
                substeps.add(substep);
            }
            response.setSubsteps(substeps);
        } else {
            response.setStepStatus("Not ordered yet");
        }
    }

    private void setCustomerReceiveStatus(ChecklistStepResponse response, Long userId, List<String> validDeliveryStatuses) {
        List<Order> orders = orderRepository.findByCreatedBy_Id(userId);
        if (orders.isEmpty()) {
            response.setStepStatus("Not received yet");
            return;
        }
        
        orders.sort((o1, o2) -> o2.getCreatedAt().compareTo(o1.getCreatedAt()));
        Order latestOrder = orders.get(0);
        
        if (latestOrder.getStatus() != null && latestOrder.getStatus().getName() != null 
            && validDeliveryStatuses.contains(latestOrder.getStatus().getName())) {
            response.setStepStatus(latestOrder.getStatus().getName());
        } else {
            response.setStepStatus("Not received yet");
        }
    }

    private void setCustomerPaymentStatus(ChecklistStepResponse response, Long userId, List<String> validPaymentStatuses) {
        List<Order> orders = orderRepository.findByCreatedBy_Id(userId);
        if (orders.isEmpty()) {
            response.setStepStatus("Not paid yet");
            return;
        }
        
        orders.sort((o1, o2) -> o2.getCreatedAt().compareTo(o1.getCreatedAt()));
        Order latestOrder = orders.get(0);
        List<Payment> payments = paymentRepository.findByOrderId(latestOrder.getId());
        
        if (payments.isEmpty()) {
            response.setStepStatus("Not paid yet");
            return;
        }
        
        payments.sort((p1, p2) -> p2.getCreatedAt().compareTo(p1.getCreatedAt()));
        Payment latestPayment = payments.get(0);
        
        if (latestPayment.getStatus() != null && latestPayment.getStatus().getName() != null 
            && validPaymentStatuses.contains(latestPayment.getStatus().getName())) {
            response.setStepStatus(latestPayment.getStatus().getName());
        } else {
            response.setStepStatus("Not paid yet");
        }
    }

    private void setDispatcherStatus(ChecklistStepResponse response, Long userId, String stepCode) {
        // For dispatcher, check activity logs or specific business logic
        boolean hasActivity = activityLogRepository.existsByActorIdAndActionContaining(userId, stepCode);
        response.setStepStatus(hasActivity ? "Completed" : "Pending");
    }

    private void setDriverStatus(ChecklistStepResponse response, Long userId, String stepCode) {
        // For driver, check delivery status and activity logs
        boolean hasActivity = activityLogRepository.existsByActorIdAndActionContaining(userId, stepCode);
        response.setStepStatus(hasActivity ? "Completed" : "Pending");
    }

    /**
     * ============== ORDER TIMELINE API ==============
     * Lấy timeline hoàn chỉnh cho một đơn hàng cụ thể
     */
    public OrderTimelineResponse getOrderTimeline(Long orderId) {
        // Lấy thông tin đơn hàng
        Order order = orderRepository.findById(orderId).orElse(null);
        if (order == null) {
            // Có thể trả về response rỗng hoặc null, hoặc response với thông báo lỗi
            return null;
        }

        // Build response
        OrderTimelineResponse response = OrderTimelineResponse.builder()
            .orderId(order.getId())
            .orderCode(order.getOrderCode())
            .createdAt(order.getCreatedAt() != null ? order.getCreatedAt().toLocalDateTime() : null)
            .updatedAt(order.getUpdatedAt() != null ? order.getUpdatedAt().toLocalDateTime() : null)
            .build();

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

        // Build timeline steps
        List<TimelineStepDto> timeline = buildTimelineSteps(orderId);
        response.setTimeline(timeline);

        return response;
    }

    /**
     * Xây dựng danh sách các bước timeline cho một đơn hàng
     */

    public List<TimelineStepDto> buildTimelineSteps(Long orderId) {
        Order order = orderRepository.findById(orderId).orElse(null);
        if (order == null) return new ArrayList<>();

        // Lấy tất cả các bước checklist_progress liên quan đến orderId (không filter role)
        List<ChecklistProgress> progresses = checklistProgressRepository.findAll().stream()
            .filter(p -> p.getOrder() != null && p.getOrder().getId().equals(orderId))
            .collect(Collectors.toList());
        List<TimelineStepDto> timelineSteps = new ArrayList<>();
        for (ChecklistProgress progress : progresses) {
            ChecklistStep step = progress.getStep();
            // Build timeline cho cả CUSTOMER, DISPATCHER, DRIVER
            TimelineStepDto dto = new TimelineStepDto();
            dto.setStepOrder(step.getStepOrder());
            dto.setStepCode(step.getStepCode());
            dto.setStepName(step.getStepName());
            dto.setDescription(step.getDescription());
            dto.setCompleted(progress.getCompleted());
            dto.setCompletedAt(progress.getCompletedAt());
            if (progress.getUser() != null) {
                dto.setActor(new ActorDto(progress.getUser().getId(), progress.getUser().getFullName(), progress.getUser().getRole().getRoleName(), progress.getUser().getPhone()));
            }
            dto.setDetails(progress.getDetails());
            dto.setStatus(progress.getCompleted() ? "Completed" : "Pending");
            timelineSteps.add(dto);
        }
        // Sắp xếp theo stepOrder
        timelineSteps.sort(Comparator.comparingInt(s -> s.getStepOrder() != null ? s.getStepOrder() : 0));
        return timelineSteps;
    }

    /**
     * Lấy tiến trình checklist (timeline) cho một đơn hàng
     */
    public ChecklistProgressResponse getProgressByOrder(Long orderId) {
        Order order = orderRepository.findById(orderId).orElse(null);
        if (order == null) {
            ChecklistProgressResponse response = new ChecklistProgressResponse();
            response.setStatus("error");
            response.setMessage("Order not found with ID: " + orderId);
            return response;
        }
        List<ChecklistProgress> progresses = checklistProgressRepository.findByOrder(order);
        List<ChecklistStepResponse> steps = new ArrayList<>();
        for (ChecklistProgress progress : progresses) {
            ChecklistStep step = progress.getStep();
            ChecklistStepResponse stepResp = new ChecklistStepResponse();
            stepResp.setStepCode(step.getStepCode());
            stepResp.setStepName(step.getStepName());
            stepResp.setDescription(step.getDescription());
            stepResp.setStepOrder(step.getStepOrder());
            stepResp.setCompleted(progress.getCompleted() != null ? progress.getCompleted() : false);
            stepResp.setStepStatus(progress.getCompleted() ? "Completed" : "Pending");
            stepResp.setCompletionDetails(progress.getDetails());
            stepResp.setRequired(true);
            steps.add(stepResp);
        }
        ChecklistProgressResponse response = new ChecklistProgressResponse();
        response.setUserId(order.getDriver() != null ? order.getDriver().getId() : null);
        response.setUserRole(order.getDriver() != null ? "DRIVER" : "DISPATCHER");
        response.setUserName(order.getDriver() != null ? order.getDriver().getFullName() : "");
        response.setSteps(steps);
        response.setTotalSteps(steps.size());
        response.setCompletedSteps((int) steps.stream().filter(ChecklistStepResponse::isCompleted).count());
        response.calculateCompletionPercentage();
        response.setStatus("success");
        response.setMessage("Lấy tiến trình checklist thành công!");
        return response;
    }

}       

