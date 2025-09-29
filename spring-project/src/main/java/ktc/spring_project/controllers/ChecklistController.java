package ktc.spring_project.controllers;

import ktc.spring_project.dtos.ChecklistProgressResponse;
import ktc.spring_project.dtos.ChecklistStepResponse;
import ktc.spring_project.dtos.timeline.OrderTimelineResponse;
import ktc.spring_project.services.ChecklistService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * Controller chính thức cho Checklist API
 * Cung cấp các endpoint để theo dõi tiến trình hoàn thành các bước trong hệ thống
 */
@RestController
@RequestMapping("/api/checklist")
@CrossOrigin(originPatterns = {"http://localhost:*", "https://localhost:*"}, allowCredentials = "true")
public class ChecklistController {

    /**
     * API lấy checklist động cho user (trả về trạng thái từng bước)
     */
    @GetMapping("/user/{userId}/steps")
    public ResponseEntity<List<ChecklistStepResponse>> getChecklistForUser(@PathVariable Long userId) {
        List<ChecklistStepResponse> steps = checklistService.getChecklistForUser(userId);
        return ResponseEntity.ok(steps);
    }

    /**
     * API đánh dấu hoàn thành bước checklist cho user
     */
    @PostMapping("/user/{userId}/complete/{stepCode}")
    public ResponseEntity<String> completeStepForUser(@PathVariable Long userId, @PathVariable String stepCode, @RequestParam Long orderId, @RequestParam(required = false) String details) {
        checklistService.markStepCompleted(userId, orderId, stepCode, details);
        return ResponseEntity.ok("Đã cập nhật trạng thái hoàn thành bước " + stepCode);
    }

    @Autowired
    private ChecklistService checklistService;
    
    @Autowired
    private UserService userService;

    /**
     * Lấy tiến trình checklist cho khách hàng
     * @param customerId ID của khách hàng
     * @return Tiến trình hoàn thành các bước của khách hàng
     */
    @GetMapping("/customer/{customerId}")
    // @PreAuthorize("hasRole('ADMIN') or hasRole('DISPATCHER')") // Tạm thời bỏ để test
    public ResponseEntity<ChecklistProgressResponse> getCustomerProgress(@PathVariable Long customerId) {
        ChecklistProgressResponse progress = checklistService.getCustomerProgress(customerId);
        return ResponseEntity.ok(progress);
    }

    /**
     * Lấy tiến trình checklist cho dispatcher
     * @param dispatcherId ID của dispatcher
     * @return Tiến trình hoàn thành các bước của dispatcher
     */
    @GetMapping("/dispatcher/{dispatcherId}")
    // @PreAuthorize("hasRole('ADMIN')") // Tạm thời bỏ để test
    public ResponseEntity<ChecklistProgressResponse> getDispatcherProgress(@PathVariable Long dispatcherId) {
        ChecklistProgressResponse progress = checklistService.getDispatcherProgress(dispatcherId);
        return ResponseEntity.ok(progress);
    }

    /**
     * Lấy tiến trình checklist cho tài xế
     * @param driverId ID của tài xế
     * @return Tiến trình hoàn thành các bước của tài xế
     */
    @GetMapping("/driver/{driverId}")
    // @PreAuthorize("hasRole('ADMIN') or hasRole('DISPATCHER')") // Tạm thời bỏ để test
    public ResponseEntity<ChecklistProgressResponse> getDriverProgress(@PathVariable Long driverId) {
        ChecklistProgressResponse progress = checklistService.getDriverProgress(driverId);
        return ResponseEntity.ok(progress);
    }

    /**
     * Lấy checklist cho user hiện tại
     */
    @GetMapping("/my-checklist")
    // @PreAuthorize("hasRole('CUSTOMER') or hasRole('DISPATCHER') or hasRole('DRIVER')") // Tạm thời bỏ để test
    public ResponseEntity<ChecklistProgressResponse> getMyChecklist() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Long userId = extractUserIdFromAuthentication(authentication);
        
        if (userId == null) {
            return ResponseEntity.badRequest().build();
        }

        ChecklistProgressResponse progress = checklistService.getUserProgress(userId);
        return ResponseEntity.ok(progress);
    }

    /**
     * Lấy tất cả các bước checklist chuẩn (không phân biệt role)
     * Dành cho FE hiển thị timeline với đầy đủ các bước
     * @return Danh sách tất cả các bước trong checklist step
     */
    @GetMapping("/steps")
    public ResponseEntity<List<ChecklistStepResponse>> getAllChecklistSteps() {
        List<ChecklistStepResponse> steps = checklistService.getAllChecklistSteps();
        return ResponseEntity.ok(steps);
    }

    /**
     * Lấy tất cả các bước checklist cho một vai trò
     * @param role Vai trò (customer, dispatcher, driver)
     * @return Danh sách tất cả các bước trong checklist
     */
    @GetMapping("/steps/{role}")
    public ResponseEntity<List<ChecklistStepResponse>> getChecklistSteps(@PathVariable String role) {
        List<ChecklistStepResponse> steps = checklistService.getChecklistStepsByRole(role);
        return ResponseEntity.ok(steps);
    }

    /**
     * Đánh dấu một bước checklist đã hoàn thành
     * @param userId ID của người dùng
     * @param stepCode Mã của bước cần đánh dấu hoàn thành
     * @param orderId ID của đơn hàng
     * @param details Chi tiết bổ sung (tuỳ chọn)
     * @return Kết quả thành công
     */
    @PostMapping("/complete/{userId}/{stepCode}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<String> markStepCompleted(@PathVariable Long userId, 
                                                   @PathVariable String stepCode,
                                                   @RequestParam Long orderId,
                                                   @RequestParam(required = false) String details) {
        checklistService.markStepCompleted(userId, orderId, stepCode, details);
        return ResponseEntity.ok("Bước checklist đã được đánh dấu hoàn thành");
    }

    /**
     * Lấy danh sách người dùng chưa hoàn thành checklist
     * @param role Vai trò cần kiểm tra
     * @return Danh sách ID người dùng chưa hoàn thành
     */
    @GetMapping("/incomplete/{role}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<List<Long>> getIncompleteUsers(@PathVariable String role) {
        List<Long> incompleteUserIds = checklistService.getIncompleteUsers(role);
        return ResponseEntity.ok(incompleteUserIds);
    }

    /**
     * Lấy thống kê tổng quan checklist theo vai trò
     * @param role Vai trò cần thống kê
     * @return Thống kê tổng quan
     */
    @GetMapping("/stats/{role}")
    @PreAuthorize("hasRole('ADMIN') or hasRole('DISPATCHER')")
    public ResponseEntity<Map<String, Object>> getChecklistStats(@PathVariable String role) {
        Map<String, Object> stats = checklistService.getChecklistStatsByRole(role);
        return ResponseEntity.ok(stats);
    }

    /**
     * Utility method để extract user ID từ authentication
     */
    private Long extractUserIdFromAuthentication(Authentication authentication) {
        try {
            String authName = authentication.getName();
            
            // Thử parse trực tiếp nếu là user ID
            try {
                return Long.parseLong(authName);
            } catch (NumberFormatException e) {
                // Nếu không parse được, tìm user theo username
                var user = userService.findByUsername(authName);
                return user != null ? user.getId() : null;
            }
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * ============== ORDER TIMELINE API ==============
     * Lấy timeline hoàn chỉnh cho một đơn hàng cụ thể
     * @param orderId ID của đơn hàng
     * @return Timeline chi tiết của đơn hàng
     */
    @GetMapping("/orders/{orderId}/timeline")
    public ResponseEntity<OrderTimelineResponse> getOrderTimeline(@PathVariable Long orderId) {
        try {
            OrderTimelineResponse timeline = checklistService.getOrderTimeline(orderId);
            return ResponseEntity.ok(timeline);
        } catch (RuntimeException e) {
            return ResponseEntity.notFound().build();
        } catch (Exception e) {
            return ResponseEntity.status(500).build();
        }
    }
}
