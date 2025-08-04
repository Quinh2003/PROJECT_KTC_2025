package ktc.spring_project.controllers;

import ktc.spring_project.entities.User;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.DriverService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
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
    private DriverService driverService;

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
            @Valid @RequestBody User user,
            Authentication authentication) {

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

        User updatedUser = userService.updateCurrentUserProfile(profileData, authentication);
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

        List<User> drivers = driverService.getFilteredDrivers(status, available);
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

        List<Map<String, Object>> orders = driverService.getDriverOrders(driverId, status, dateFrom, dateTo);
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

        Long roleId = roleData.get("roleId");
        User updatedUser = userService.updateUserRole(id, roleId);
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

        Long statusId = statusData.get("statusId");
        User updatedUser = userService.updateUserStatus(id, statusId);
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

        List<Map<String, Object>> activityLogs = userService.getUserActivityLogs(id, page, size);
        return ResponseEntity.ok(activityLogs);
    }
}
