package ktc.spring_project.controllers;

import ktc.spring_project.entities.ActivityLog;
import ktc.spring_project.entities.Role;
import ktc.spring_project.entities.Status;
import ktc.spring_project.services.ActivityLogService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Controller responsible for system administration
 * Based on user stories:
 * - US-ADMIN-USERS-01: Manage Users and Roles
 * - US-ADMIN-CONFIG-01: Manage System Configurations
 * - System administration and configuration
 */
@RestController
@RequestMapping("/api/admin")
@PreAuthorize("hasRole('ADMIN')")
public class AdminController {

    @Autowired
    private UserService userService;

    @Autowired
    private ActivityLogService activityLogService;

    /**
     * Get system configurations
     * US-ADMIN-CONFIG-01
     */
    @GetMapping("/config")
    public ResponseEntity<Map<String, Object>> getSystemConfigurations() {
        // TODO: Implement ConfigService and inject here
        // Map<String, Object> configurations = configService.getAllConfigurations();
        return ResponseEntity.ok(Map.of());
    }

    /**
     * Update system configuration
     * US-ADMIN-CONFIG-01
     */
    @PutMapping("/config")
    public ResponseEntity<Map<String, Object>> updateSystemConfiguration(
            @Valid @RequestBody Map<String, Object> configData,
            Authentication authentication) {

        // TODO: Implement ConfigService and inject here
        // Map<String, Object> updatedConfig = configService.updateConfiguration(configData, authentication);
        return ResponseEntity.ok(Map.of());
    }

    /**
     * Get all roles
     * US-ADMIN-USERS-01
     */
    @GetMapping("/roles")
    public ResponseEntity<List<Role>> getAllRoles() {
        // TODO: Implement AdminService and inject here
        // List<Role> roles = adminService.getAllRoles();
        return ResponseEntity.ok(List.of());
    }

    /**
     * Create new role
     * US-ADMIN-USERS-01
     */
    @PostMapping("/roles")
    public ResponseEntity<Role> createRole(
            @Valid @RequestBody Role role,
            Authentication authentication) {

        // TODO: Implement AdminService and inject here
        // Role createdRole = adminService.createRole(role, authentication);
        return new ResponseEntity<>(new Role(), HttpStatus.CREATED);
    }

    /**
     * Update role
     * US-ADMIN-USERS-01
     */
    @PutMapping("/roles/{id}")
    public ResponseEntity<Role> updateRole(
            @PathVariable Long id,
            @Valid @RequestBody Role role,
            Authentication authentication) {

        // TODO: Implement AdminService and inject here
        // Role updatedRole = adminService.updateRole(id, role, authentication);
        return ResponseEntity.ok(new Role());
    }

    /**
     * Delete role
     * US-ADMIN-USERS-01
     */
    @DeleteMapping("/roles/{id}")
    public ResponseEntity<Void> deleteRole(
            @PathVariable Long id,
            Authentication authentication) {

        // TODO: Implement AdminService and inject here
        // adminService.deleteRole(id, authentication);
        return ResponseEntity.noContent().build();
    }

    /**
     * Get all status types
     */
    @GetMapping("/status")
    public ResponseEntity<List<Status>> getAllStatus(
            @RequestParam(required = false) String type) {

        // TODO: Implement AdminService and inject here
        // List<Status> statusList = adminService.getAllStatus(type);
        return ResponseEntity.ok(List.of());
    }

    /**
     * Create new status
     */
    @PostMapping("/status")
    public ResponseEntity<Status> createStatus(
            @Valid @RequestBody Status status,
            Authentication authentication) {

        // TODO: Implement AdminService and inject here
        // Status createdStatus = adminService.createStatus(status, authentication);
        return new ResponseEntity<>(new Status(), HttpStatus.CREATED);
    }

    /**
     * Get system activity logs
     * US-ADMIN-USERS-01
     */
    @GetMapping("/activity-logs")
    public ResponseEntity<List<Map<String, Object>>> getSystemActivityLogs(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String actionType,
            @RequestParam(required = false) Long userId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "50") int size) {

        try {
            List<ActivityLog> logs = activityLogService.getAllActivityLogs();
            
            List<Map<String, Object>> activityLogs = logs.stream().map(log -> {
                Map<String, Object> logMap = new HashMap<>();
                logMap.put("id", log.getId());
                logMap.put("time", log.getActionTimestamp() != null ? log.getActionTimestamp().toString() : null);
                logMap.put("user", log.getActor() != null ? log.getActor().getEmail() : "system");
                logMap.put("action", log.getActionType() != null ? log.getActionType().toString() : "Unknown");
                logMap.put("detail", log.getMetadata() != null ? log.getMetadata() : "No details");
                logMap.put("status", "success"); // Default status, can be enhanced
                logMap.put("role", log.getRole() != null ? log.getRole().getRoleName() : null);
                return logMap;
            }).collect(Collectors.toList());
            
            return ResponseEntity.ok(activityLogs);
        } catch (Exception e) {
            // Return empty list if service not available
            return ResponseEntity.ok(List.of());
        }
    }

    /**
     * Get system statistics
     */
    @GetMapping("/statistics")
    public ResponseEntity<Map<String, Object>> getSystemStatistics() {
        // TODO: Implement AdminService and inject here
        // Map<String, Object> statistics = adminService.getSystemStatistics();
        return ResponseEntity.ok(Map.of());
    }

    /**
     * Backup system data
     */
    @PostMapping("/backup")
    public ResponseEntity<Map<String, String>> backupSystemData(
            @RequestBody(required = false) Map<String, Object> backupOptions,
            Authentication authentication) {

        // TODO: Implement AdminService and inject here
        // String backupId = adminService.createSystemBackup(backupOptions, authentication);
        return ResponseEntity.ok(Map.of("backupId", "temp-id", "message", "Backup initiated"));
    }

    /**
     * Get backup history
     */
    @GetMapping("/backup/history")
    public ResponseEntity<List<Map<String, Object>>> getBackupHistory() {
        // TODO: Implement AdminService and inject here
        // List<Map<String, Object>> backupHistory = adminService.getBackupHistory();
        return ResponseEntity.ok(List.of());
    }

    /**
     * System health check
     */
    @GetMapping("/health")
    public ResponseEntity<Map<String, Object>> getSystemHealth() {
        // TODO: Implement AdminService and inject here
        // Map<String, Object> healthStatus = adminService.getSystemHealth();
        return ResponseEntity.ok(Map.of("status", "healthy"));
    }
}