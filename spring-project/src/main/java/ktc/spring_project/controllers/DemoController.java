package ktc.spring_project.controllers;

import ktc.spring_project.services.interfaces.UserService;
import ktc.spring_project.dtos.user.UserResponseDTO;
import ktc.spring_project.dtos.common.ApiResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * Demo controller to showcase KTC Logistics System
 * Provides endpoints to test the complete system functionality
 */
@RestController
@RequestMapping("/api/demo")
@CrossOrigin(origins = "*")
public class DemoController {
    
    @Autowired
    private UserService userService;
    
    /**
     * System health check endpoint
     */
    @GetMapping("/health")
    public ResponseEntity<ApiResponse<Map<String, Object>>> healthCheck() {
        Map<String, Object> health = Map.of(
            "status", "UP",
            "service", "KTC Logistics System",
            "version", "1.0.0",
            "timestamp", System.currentTimeMillis(),
            "description", "3PL/4PL Logistics Management System with AI Route Optimization"
        );
        
        ApiResponse<Map<String, Object>> response = new ApiResponse<>();
        response.setSuccess(true);
        response.setMessage("System is running successfully");
        response.setData(health);
        response.setTimestamp(java.time.LocalDateTime.now());
        
        return ResponseEntity.ok(response);
    }
    
    /**
     * Get system statistics
     */
    @GetMapping("/stats")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getSystemStats() {
        try {
            // Simulated stats for demo
            Map<String, Object> stats = Map.of(
                "totalUsers", 0L,
                "activeUsers", 0L,
                "inactiveUsers", 0L,
                "customerCount", 0L,
                "driverCount", 0L,
                "managerCount", 0L,
                "adminCount", 0L,
                "message", "Database is empty - ready for data insertion"
            );
            
            ApiResponse<Map<String, Object>> response = new ApiResponse<>();
            response.setSuccess(true);
            response.setMessage("System statistics retrieved successfully");
            response.setData(stats);
            response.setTimestamp(java.time.LocalDateTime.now());
            
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            ApiResponse<Map<String, Object>> response = new ApiResponse<>();
            response.setSuccess(false);
            response.setMessage("Error retrieving system statistics: " + e.getMessage());
            response.setTimestamp(java.time.LocalDateTime.now());
            
            return ResponseEntity.status(500).body(response);
        }
    }
    
    /**
     * Get all drivers for demo
     */
    @GetMapping("/drivers")
    public ResponseEntity<ApiResponse<List<UserResponseDTO>>> getAllDrivers() {
        try {
            List<UserResponseDTO> drivers = List.of(); // Empty for now
            
            ApiResponse<List<UserResponseDTO>> response = new ApiResponse<>();
            response.setSuccess(true);
            response.setMessage("Drivers retrieved successfully (database is empty - demo mode)");
            response.setData(drivers);
            response.setTimestamp(java.time.LocalDateTime.now());
            
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            ApiResponse<List<UserResponseDTO>> response = new ApiResponse<>();
            response.setSuccess(false);
            response.setMessage("Error retrieving drivers: " + e.getMessage());
            response.setTimestamp(java.time.LocalDateTime.now());
            
            return ResponseEntity.status(500).body(response);
        }
    }
    
    /**
     * Get all customers for demo
     */
    @GetMapping("/customers")
    public ResponseEntity<ApiResponse<List<UserResponseDTO>>> getAllCustomers() {
        try {
            List<UserResponseDTO> customers = List.of(); // Empty for now
            
            ApiResponse<List<UserResponseDTO>> response = new ApiResponse<>();
            response.setSuccess(true);
            response.setMessage("Customers retrieved successfully (database is empty - demo mode)");
            response.setData(customers);
            response.setTimestamp(java.time.LocalDateTime.now());
            
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            ApiResponse<List<UserResponseDTO>> response = new ApiResponse<>();
            response.setSuccess(false);
            response.setMessage("Error retrieving customers: " + e.getMessage());
            response.setTimestamp(java.time.LocalDateTime.now());
            
            return ResponseEntity.status(500).body(response);
        }
    }
    
    /**
     * Demo user authentication
     */
    @PostMapping("/auth/test")
    public ResponseEntity<ApiResponse<Map<String, Object>>> testAuth(@RequestBody Map<String, String> credentials) {
        try {
            String email = credentials.get("email");
            String password = credentials.get("password");
            
            // Demo authentication (accepts any credentials for demo)
            Map<String, Object> result = Map.of(
                "authenticated", true,
                "email", email,
                "message", "Demo authentication successful - database is empty, would create user with these credentials",
                "note", "In production, this would verify against stored user credentials"
            );
            
            ApiResponse<Map<String, Object>> response = new ApiResponse<>();
            response.setSuccess(true);
            response.setMessage("Demo authentication completed");
            response.setData(result);
            response.setTimestamp(java.time.LocalDateTime.now());
            
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            ApiResponse<Map<String, Object>> response = new ApiResponse<>();
            response.setSuccess(false);
            response.setMessage("Authentication error: " + e.getMessage());
            response.setTimestamp(java.time.LocalDateTime.now());
            
            return ResponseEntity.status(500).body(response);
        }
    }
    
    /**
     * System demo information
     */
    @GetMapping("/info")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getDemoInfo() {
        Map<String, Object> info = Map.of(
            "system", "KTC Logistics Management System",
            "description", "Complete 3PL/4PL logistics solution with AI-powered route optimization",
            "features", List.of(
                "User Management (Admin, Manager, Driver, Customer)",
                "Product & Warehouse Management",
                "Order Management with Real-time Tracking",
                "Vehicle Fleet Management",
                "Payment Processing & Financial Analytics",
                "AI Route Optimization",
                "3D Map Visualization",
                "Real-time Notifications",
                "Comprehensive Dashboard & Analytics"
            ),
            "sampleCredentials", Map.of(
                "admin", Map.of("email", "admin@ktc.com", "password", "admin123"),
                "manager", Map.of("email", "manager@ktc.com", "password", "manager123"),
                "driver", Map.of("email", "driver1@ktc.com", "password", "driver123"),
                "customer", Map.of("email", "tech@abc.com", "password", "customer123")
            ),
            "endpoints", List.of(
                "GET /api/demo/health - System health check",
                "GET /api/demo/stats - System statistics",
                "GET /api/demo/drivers - All drivers",
                "GET /api/demo/customers - All customers",
                "POST /api/demo/auth/test - Test authentication",
                "GET /api/demo/info - This demo information"
            )
        );
        
        ApiResponse<Map<String, Object>> response = new ApiResponse<>();
        response.setSuccess(true);
        response.setMessage("KTC Logistics System Demo Information");
        response.setData(info);
        response.setTimestamp(java.time.LocalDateTime.now());
        
        return ResponseEntity.ok(response);
    }
}