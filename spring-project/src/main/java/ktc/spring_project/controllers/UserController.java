package ktc.spring_project.controllers;

import ktc.spring_project.entities.User;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.OrderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Controller responsible for managing users
 * Based on user stories:
 * - US-ADMIN-USERS-01: Manage Users and Roles
 * - US-DRIVER-HOME-01: Driver View New Orders
 * - User profile management
 */
@RestController
@RequestMapping("/api/users")
public class UserController {

    @Autowired
    private UserService userService;

    @Autowired
    private OrderService orderService;

    /**
     * Get all users with optional filters
     * US-ADMIN-USERS-01
     */
    @GetMapping
    public ResponseEntity<List<User>> getAllUsers(
            @RequestParam(required = false) String role,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String search) {

        List<User> users = userService.getFilteredUsers(role, status, search);
        return ResponseEntity.ok(users);
    }

    /**
     * Get user by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<User> getUserById(@PathVariable Long id) {
        User user = userService.getUserById(id);
        return ResponseEntity.ok(user);
    }

    /**
     * Create new user
     * US-ADMIN-USERS-01
     */
    @PostMapping
    public ResponseEntity<User> createUser(
            @Valid @RequestBody ktc.spring_project.dtos.user.CreateUserRequestDTO dto,
            Authentication authentication) {

    User user = new User();
    user.setFullName(dto.getFullName());
    user.setEmail(dto.getEmail());
    user.setPassword(dto.getPassword());
    user.setPhone(dto.getPhone());
    user.setUsername(dto.getUsername());
    // user.setAddress(dto.getAddress());
    user.setNotes(dto.getNotes());
    // Map role, status nếu cần (giả sử có các repository/service)

        User createdUser = userService.createUser(user);
        return new ResponseEntity<>(createdUser, HttpStatus.CREATED);
    }

    /**
     * Update user information
     * US-ADMIN-USERS-01
     */
    @PutMapping("/{id}")
    public ResponseEntity<User> updateUser(
            @PathVariable Long id,
            @Valid @RequestBody User user,
            Authentication authentication) {

        User updatedUser = userService.updateUser(id, user);
        return ResponseEntity.ok(updatedUser);
    }

    /**
     * Delete user (soft delete)
     * US-ADMIN-USERS-01
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteUser(
            @PathVariable Long id,
            Authentication authentication) {

        userService.deleteUser(id);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get current user profile
     */
    @GetMapping("/profile")
    public ResponseEntity<User> getCurrentUserProfile(Authentication authentication) {
        User currentUser = userService.getCurrentUser(authentication);
        return ResponseEntity.ok(currentUser);
    }

    /**
     * Update current user profile
     */
    @PutMapping("/profile")
    public ResponseEntity<User> updateCurrentUserProfile(
            @Valid @RequestBody Map<String, Object> profileData,
            Authentication authentication) {

        // Lấy thông tin người dùng hiện tại
        User currentUser = userService.getCurrentUser(authentication);

        // Cập nhật thông tin người dùng (ví dụ: chỉ cập nhật thông tin cơ bản như fullName, phone, ...)
        if (profileData.containsKey("fullName")) {
            currentUser.setFullName((String) profileData.get("fullName"));
        }

        if (profileData.containsKey("phone")) {
            currentUser.setPhone((String) profileData.get("phone"));
        }

        // Lưu thông tin người dùng đã cập nhật
        User updatedUser = userService.updateUser(currentUser.getId(), currentUser);

        return ResponseEntity.ok(updatedUser);
    }

    /**
     * Get all drivers
     * Supporting US-ORDER-ASSIGN-01
     */
    @GetMapping("/drivers")
    public ResponseEntity<List<User>> getAllDrivers(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Boolean available) {

        // Sử dụng phương thức cơ bản getAllUsers() thay vì tạo phương thức mới
        List<User> allUsers = userService.getAllUsers();

        // Lọc người dùng có vai trò là tài xế
        List<User> drivers = allUsers.stream()
                .filter(user -> user.getRole() != null && "DRIVER".equals(user.getRole().getRoleName()))
                .toList();

        return ResponseEntity.ok(drivers);
    }

    /**
     * Get driver's assigned orders
     * US-DRIVER-HOME-01
     */
    @GetMapping("/{driverId}/orders")
    public ResponseEntity<List<Map<String, Object>>> getDriverOrders(
            @PathVariable Long driverId,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        // Sử dụng phương thức cơ bản từ OrderService
        List<Map<String, Object>> orders = new ArrayList<>();

        // Trong thực tế, sẽ lấy danh sách đơn hàng của tài xế từ OrderService
        // Đây chỉ là giải pháp tạm thời cho đến khi service được cập nhật

        return ResponseEntity.ok(orders);
    }

    /**
     * Update user role
     * US-ADMIN-USERS-01
     */
    @PatchMapping("/{id}/role")
    public ResponseEntity<User> updateUserRole(
            @PathVariable Long id,
            @RequestBody Map<String, Long> roleData,
            Authentication authentication) {


    // Lấy thông tin người dùng
    User user = userService.getUserById(id);

        // Cập nhật vai trò
        // Giả sử roleService.getRoleById() trả về Role entity tương ứng với roleId
        // user.setRole(roleService.getRoleById(roleId));

        // Trong trường hợp thực tế, có thể phải thêm role vào user và cập nhật
        // Đây chỉ là giải pháp tạm thời

        // Lưu thông tin người dùng đã cập nhật
        User updatedUser = userService.updateUser(id, user);

        return ResponseEntity.ok(updatedUser);
    }

    /**
     * Update user status
     * US-ADMIN-USERS-01
     */
    @PatchMapping("/{id}/status")
    public ResponseEntity<User> updateUserStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Long> statusData,
            Authentication authentication) {


    // Lấy thông tin người dùng
    User user = userService.getUserById(id);

        // Cập nhật trạng thái
        // Giả sử statusService.getStatusById() trả về Status entity tương ứng với statusId
        // user.setStatus(statusService.getStatusById(statusId));

        // Trong trường hợp thực tế, có thể phải thêm status vào user và cập nhật
        // Đây chỉ là giải pháp tạm thời

        // Lưu thông tin người dùng đã cập nhật
        User updatedUser = userService.updateUser(id, user);

        return ResponseEntity.ok(updatedUser);
    }

    /**
     * Get user activity logs
     * US-ADMIN-USERS-01
     */
    @GetMapping("/{id}/activity-logs")
    public ResponseEntity<List<Map<String, Object>>> getUserActivityLogs(
            @PathVariable Long id,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "50") int size) {

        // Trong thực tế, sẽ lấy danh sách log hoạt động của người dùng từ ActivityLogService
        // Đây chỉ là giải pháp tạm thời cho đến khi service được cập nhật
        List<Map<String, Object>> activityLogs = new ArrayList<>();

        return ResponseEntity.ok(activityLogs);
    }
}
