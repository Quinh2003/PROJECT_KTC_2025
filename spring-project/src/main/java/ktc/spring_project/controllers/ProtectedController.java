package ktc.spring_project.controllers;

import ktc.spring_project.entities.User;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * Protected Controller - Controller chứa các endpoints được bảo vệ bởi JWT và RBAC
 *
 * Controller này được tạo để test và demo JWT authentication và Role-Based Access Control
 * Tất cả endpoints trong controller này đều yêu cầu JWT token hợp lệ
 *
 * Mục đích:
 * - Test axios interceptor để tự động thêm JWT token vào requests
 * - Demo RBAC với các role khác nhau (ADMIN, DISPATCHER, DRIVER, CUSTOMER)
 * - Cung cấp endpoints để verify JWT token đang hoạt động đúng
 *
 * Security Features:
 * - Tất cả endpoints yêu cầu authentication (JWT token)
 * - Một số endpoints yêu cầu role cụ thể (@PreAuthorize)
 * - Access control dựa trên role của user
 */
@RestController
@RequestMapping("/api/protected")
public class ProtectedController {

    @Autowired
    private UserService userService;

    /**
     * Get Profile Endpoint
     *
     * Endpoint này accessible bởi tất cả authenticated users (mọi role)
     * Dùng để test basic JWT authentication mà không cần role cụ thể
     *
     * @param authentication Spring Security Authentication object (tự động inject)
     * @return User profile data với timestamp
     */
    @GetMapping("/profile")
    public ResponseEntity<Map<String, Object>> getProfile(Authentication authentication) {
        // Lấy thông tin user hiện tại từ JWT token
        User user = userService.getCurrentUser(authentication);

        return ResponseEntity.ok(Map.of(
            "message", "Profile data accessed successfully",
            "user", user,
            "timestamp", System.currentTimeMillis()
        ));
    }

    /**
     * Admin Users Endpoint
     *
     * Endpoint này CHỈ dành cho ADMIN role
     * Test RBAC - chỉ admin mới có thể xem danh sách tất cả users
     *
     * @return Danh sách tất cả users trong system
     */
    @GetMapping("/admin/users")
    @PreAuthorize("hasRole('ADMIN')")  // Chỉ ADMIN mới access được
    public ResponseEntity<Map<String, Object>> getAllUsers() {
        List<User> users = userService.getAllUsers();

        return ResponseEntity.ok(Map.of(
            "message", "Admin access: All users retrieved",
            "data", users,
            "count", users.size()
        ));
    }

    /**
     * Dispatcher Dashboard Endpoint
     *
     * Endpoint này dành cho DISPATCHER hoặc ADMIN role
     * Test RBAC với multiple roles - cả ADMIN và DISPATCHER đều access được
     *
     * @return Dashboard data cho dispatcher
     */
    @GetMapping("/dispatcher/dashboard")
    @PreAuthorize("hasRole('ADMIN') or hasRole('DISPATCHER')")
    public ResponseEntity<Map<String, Object>> getDispatcherDashboard() {
        return ResponseEntity.ok(Map.of(
            "message", "Dispatcher dashboard data",
            "data", "Dashboard metrics and statistics",
            "accessLevel", "DISPATCHER"
        ));
    }

    /**
     * Driver Routes Endpoint
     *
     * Endpoint này CHỈ dành cho DRIVER role
     * Test RBAC - chỉ driver mới có thể xem routes của mình
     *
     * @param authentication Spring Security Authentication object
     * @return Routes data cho driver hiện tại
     */
    @GetMapping("/driver/routes")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<Map<String, Object>> getDriverRoutes(Authentication authentication) {
        User driver = userService.getCurrentUser(authentication);

        return ResponseEntity.ok(Map.of(
            "message", "Driver routes retrieved",
            "driver", driver.getFullName(),
            "routes", "Active delivery routes for driver"
        ));
    }

    /**
     * Customer Orders Endpoint
     *
     * Endpoint này CHỈ dành cho CUSTOMER role
     * Test RBAC - chỉ customer mới có thể xem orders của mình
     *
     * @param authentication Spring Security Authentication object
     * @return Orders data cho customer hiện tại
     */
    @GetMapping("/customer/orders")
    @PreAuthorize("hasRole('CUSTOMER')")
    public ResponseEntity<Map<String, Object>> getCustomerOrders(Authentication authentication) {
        User customer = userService.getCurrentUser(authentication);

        return ResponseEntity.ok(Map.of(
            "message", "Customer orders retrieved",
            "customer", customer.getFullName(),
            "orders", "List of customer orders"
        ));
    }

    /**
     * Test JWT Endpoint
     *
     * Endpoint này dùng để test JWT validation với POST request
     * Nhận request body và echo lại cùng với thông tin user
     *
     * Mục đích:
     * - Test axios interceptor với POST requests
     * - Verify JWT token được gửi đúng cách
     * - Test request/response data flow
     *
     * @param requestData Data từ frontend
     * @param authentication Spring Security Authentication object
     * @return Echo data cùng với user info và timestamp
     */
    @PostMapping("/test-jwt")
    public ResponseEntity<Map<String, Object>> testJwt(
            @RequestBody Map<String, Object> requestData,
            Authentication authentication) {

        User user = userService.getCurrentUser(authentication);

        return ResponseEntity.ok(Map.of(
            "message", "JWT validation successful",
            "user", user.getUsername(),
            "role", user.getRole().getRoleName(),
            "requestData", requestData,
            "timestamp", System.currentTimeMillis()
        ));
    }
}
