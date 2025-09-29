package ktc.spring_project.controllers;

import ktc.spring_project.dtos.common.ApiResponse;
import ktc.spring_project.dtos.maintenance.CreateMaintenanceRequestByDriverDTO;
import ktc.spring_project.dtos.maintenance.CreateMonthlyMaintenanceRequestDTO;
import ktc.spring_project.dtos.maintenance.MaintenanceRequestResponseDTO;
import ktc.spring_project.dtos.maintenance.UpdateMaintenanceRequestDTO;
import ktc.spring_project.services.MaintenanceRequestService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.sql.Timestamp;
import java.util.Map;

/**
 * Controller for managing vehicle maintenance requests
 * Supports complete 7-step workflow:
 * Step 1: Driver creates request (statusId=51 MAINTENANCE_PENDING)  
 * Step 2: Fleet manager views pending requests
 * Step 3: Fleet manager schedules maintenance (statusId=19 MAINTENANCE)
 * Step 4: Driver views scheduled maintenance
 * Step 5: Driver takes vehicle to garage (statusId=19 MAINTENANCE) 
 * Step 6: Fleet manager completes maintenance (statusId=17 AVAILABLE)
 * Step 7: Driver picks up vehicle (statusId=18 IN_USE)
 */
@RestController
@RequestMapping("/api")
public class MaintenanceRequestController {

    @Autowired
    private MaintenanceRequestService maintenanceRequestService;

    // =================
    // DRIVER APIs (Steps 1, 4, 5, 7)
    // =================

    /**
     * Step 1: Driver creates maintenance request
     * POST /api/drivers/{driverId}/maintenance-requests
     */
    @PostMapping("/drivers/{driverId}/maintenance-requests/emergency")
    public ResponseEntity<ApiResponse<MaintenanceRequestResponseDTO>> createMaintenanceRequest(
            @PathVariable Long driverId,
            @Valid @RequestBody CreateMaintenanceRequestByDriverDTO createDTO) {
        try {
            MaintenanceRequestResponseDTO response = maintenanceRequestService.createMaintenanceRequestByDriver(createDTO, driverId);
            
            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ApiResponse.success(response, "Maintenance request created successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error("Failed to create maintenance request: " + e.getMessage()));
        }
    }

    /**
     * Step 4 & 5: Driver views their maintenance requests
     * GET /api/drivers/{driverId}/maintenance-requests
     */
    @GetMapping("/drivers/{driverId}/maintenance-requests")
    public ResponseEntity<ApiResponse<Page<MaintenanceRequestResponseDTO>>> getDriverMaintenanceRequests(
            @PathVariable Long driverId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String maintenanceType) {
        try {
            Pageable pageable = PageRequest.of(page, size, Sort.by("createdAt").descending());
            Page<MaintenanceRequestResponseDTO> requests = maintenanceRequestService.getDriverMaintenanceRequests(driverId, pageable);
            
            return ResponseEntity.ok(ApiResponse.success(requests, "Driver maintenance requests retrieved successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error("Failed to retrieve driver maintenance requests: " + e.getMessage()));
        }
    }

    /**
     * Step 4 & 5: Driver views specific maintenance request detail
     * GET /api/drivers/{driverId}/maintenance-requests/{maintenanceId}
     */
    @GetMapping("/drivers/{driverId}/maintenance-requests/{maintenanceId}")
    public ResponseEntity<ApiResponse<MaintenanceRequestResponseDTO>> getDriverMaintenanceRequestDetail(
            @PathVariable Long driverId,
            @PathVariable Long maintenanceId) {
        try {
            MaintenanceRequestResponseDTO response = maintenanceRequestService.getMaintenanceRequestById(maintenanceId);
            
            // Verify the request belongs to the driver
            if (!response.getCreatedBy().getId().equals((long) driverId)) {
                return ResponseEntity.status(HttpStatus.FORBIDDEN)
                        .body(ApiResponse.error("Access denied: This maintenance request does not belong to the specified driver"));
            }
            
            return ResponseEntity.ok(ApiResponse.success(response, "Maintenance request detail retrieved successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error("Maintenance request not found: " + e.getMessage()));
        }
    }

    // =================
    // FLEET MANAGEMENT APIs (Steps 2, 3, 6)
    // =================

    /**
     * Step 2: Fleet manager gets list of vehicles needing maintenance
     * GET /api/fleet/maintenance-requests
     */
    @GetMapping("/fleet/maintenance-requests")
    public ResponseEntity<ApiResponse<Page<MaintenanceRequestResponseDTO>>> getFleetMaintenanceRequests(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String maintenanceType,
            @RequestParam(required = false) Long vehicleId) {
        try {
            Pageable pageable = PageRequest.of(page, size, Sort.by("createdAt").descending());
            Page<MaintenanceRequestResponseDTO> requests = maintenanceRequestService.getAllMaintenanceRequests(pageable);
            
            return ResponseEntity.ok(ApiResponse.success(requests, "Fleet maintenance requests retrieved successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ApiResponse.error("Failed to retrieve fleet maintenance requests: " + e.getMessage()));
        }
    }

    /**
     * Step 2: Fleet manager gets detail of specific maintenance request
     * GET /api/fleet/maintenance-requests/{maintenanceId}
     */
    @GetMapping("/fleet/maintenance-requests/{maintenanceId}")
    public ResponseEntity<ApiResponse<MaintenanceRequestResponseDTO>> getFleetMaintenanceRequestDetail(
            @PathVariable Long maintenanceId) {
        try {
            MaintenanceRequestResponseDTO response = maintenanceRequestService.getMaintenanceRequestById(maintenanceId);
            return ResponseEntity.ok(ApiResponse.success(response, "Fleet maintenance request detail retrieved successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error("Maintenance request not found: " + e.getMessage()));
        }
    }

    /**
     * POST: Fleet manager creates new monthly maintenance request
     * POST /api/fleet/maintenance-requests/monthly
     */
    @PostMapping("/fleet/maintenance-requests/monthly")
    public ResponseEntity<ApiResponse<MaintenanceRequestResponseDTO>> createMonthlyMaintenanceRequest(
            @RequestBody @Valid CreateMonthlyMaintenanceRequestDTO requestDTO) {
        try {
            // Create a new maintenance request for monthly maintenance
            CreateMaintenanceRequestByDriverDTO createDTO = new CreateMaintenanceRequestByDriverDTO();
            createDTO.setDescription("Bảo dưỡng định kỳ hàng tháng");
            createDTO.setMaintenanceType("Bảo dưỡng định kỳ");
            createDTO.setStatusId(19L); // Set to MAINTENANCE status for scheduled monthly maintenance
            
            if (requestDTO.getCost() != null) {
                createDTO.setCost(requestDTO.getCost());
            }
            
            if (requestDTO.getScheduledMaintenanceDate() != null) {
                createDTO.setMaintenanceDate(java.sql.Timestamp.valueOf(requestDTO.getScheduledMaintenanceDate()));
            }
            
            if (requestDTO.getNotes() != null) {
                createDTO.setNotes(requestDTO.getNotes());
            }
            
            MaintenanceRequestResponseDTO response = maintenanceRequestService.createMonthlyMaintenanceRequest(createDTO, requestDTO.getVehicleId());
            
            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ApiResponse.success(response, "Monthly maintenance request created successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error("Failed to create monthly maintenance request: " + e.getMessage()));
        }
    }

    /**
     * Step 3a: Fleet manager schedules emergency maintenance (keeps existing maintenanceType)
     * PUT /api/fleet/maintenance-requests/{maintenanceId}/schedule
     */
    @PutMapping("/fleet/maintenance-requests/{maintenanceId}/schedule")
    public ResponseEntity<ApiResponse<MaintenanceRequestResponseDTO>> scheduleMaintenanceEmergency(
            @PathVariable Long maintenanceId,
            @RequestBody Map<String, Object> requestBody) {
        try {
            UpdateMaintenanceRequestDTO updateDTO = new UpdateMaintenanceRequestDTO();
            updateDTO.setStatusId(19L); // MAINTENANCE status
            
            // Required fields for scheduling
            if (requestBody.containsKey("scheduledMaintenanceDate")) {
                String dateStr = (String) requestBody.get("scheduledMaintenanceDate");
                updateDTO.setMaintenanceDate(Timestamp.valueOf(dateStr.replace("T", " ")));
            }
            
            if (requestBody.containsKey("cost")) {
                updateDTO.setCost(java.math.BigDecimal.valueOf(((Number) requestBody.get("cost")).doubleValue()));
            }
            
            if (requestBody.containsKey("notes")) {
                updateDTO.setNotes((String) requestBody.get("notes"));
            }
            
            // Do NOT update maintenanceType - keep existing from create request
            
            MaintenanceRequestResponseDTO response = maintenanceRequestService.updateMaintenanceRequest(maintenanceId, updateDTO);
            
            return ResponseEntity.ok(ApiResponse.success(response, "Emergency maintenance scheduled successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error("Failed to schedule emergency maintenance: " + e.getMessage()));
        }
    }

    /**
     * Step 3b: Fleet manager schedules monthly maintenance (sets maintenanceType to periodic)
     * PUT /api/fleet/maintenance-requests/{maintenanceId}/schedule-monthly
     */
    @PutMapping("/fleet/maintenance-requests/{maintenanceId}/schedule-monthly")
    public ResponseEntity<ApiResponse<MaintenanceRequestResponseDTO>> scheduleMaintenanceMonthly(
            @PathVariable Long maintenanceId,
            @RequestBody Map<String, Object> requestBody) {
        try {
            UpdateMaintenanceRequestDTO updateDTO = new UpdateMaintenanceRequestDTO();
            updateDTO.setStatusId(19L); // MAINTENANCE status
            updateDTO.setMaintenanceType("Bảo dưỡng định kỳ"); // Always set to periodic maintenance
            
            // Required fields for scheduling
            if (requestBody.containsKey("scheduledMaintenanceDate")) {
                String dateStr = (String) requestBody.get("scheduledMaintenanceDate");
                updateDTO.setMaintenanceDate(Timestamp.valueOf(dateStr.replace("T", " ")));
            }
            
            if (requestBody.containsKey("cost")) {
                updateDTO.setCost(java.math.BigDecimal.valueOf(((Number) requestBody.get("cost")).doubleValue()));
            }
            
            if (requestBody.containsKey("notes")) {
                updateDTO.setNotes((String) requestBody.get("notes"));
            }
            
            MaintenanceRequestResponseDTO response = maintenanceRequestService.updateMaintenanceRequest(maintenanceId, updateDTO);
            
            return ResponseEntity.ok(ApiResponse.success(response, "Monthly maintenance scheduled successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error("Failed to schedule monthly maintenance: " + e.getMessage()));
        }
    }

    /**
     * Unified maintenance request update endpoint (matches Postman collection)
     * Handles Steps 5, 6, 7: Take to Garage, Complete, Pick up Vehicle
     * PUT /api/maintenance-requests/{maintenanceId}
     * 
     * Step 5 (Driver To Garage): statusId=19 
     * Step 6 (Fleet Complete): statusId=17, nextDueDate
     * Step 7 (Driver Pickup): statusId=18
     */
    @PutMapping("/maintenance-requests/{maintenanceId}")
    public ResponseEntity<ApiResponse<MaintenanceRequestResponseDTO>> updateMaintenanceRequest(
            @PathVariable Long maintenanceId,
            @RequestBody Map<String, Object> requestBody) {
        try {
            UpdateMaintenanceRequestDTO updateDTO = new UpdateMaintenanceRequestDTO();
            
            // Required statusId
            if (requestBody.containsKey("statusId")) {
                updateDTO.setStatusId(((Number) requestBody.get("statusId")).longValue());
            }
            
            // Optional fields for Complete Maintenance (Step 6) - nextDueDate only
            if (requestBody.containsKey("nextDueDate")) {
                String dateStr = (String) requestBody.get("nextDueDate");
                updateDTO.setNextDueDate(Timestamp.valueOf(dateStr.replace("T", " ")));
            }
            
            // Support for scheduling fields (scheduledMaintenanceDate, cost, notes)
            if (requestBody.containsKey("scheduledMaintenanceDate")) {
                String dateStr = (String) requestBody.get("scheduledMaintenanceDate");
                updateDTO.setMaintenanceDate(Timestamp.valueOf(dateStr.replace("T", " ")));
            }
            
            if (requestBody.containsKey("cost")) {
                updateDTO.setCost(java.math.BigDecimal.valueOf(((Number) requestBody.get("cost")).doubleValue()));
            }
            
            if (requestBody.containsKey("notes")) {
                updateDTO.setNotes((String) requestBody.get("notes"));
            }
            
            // Support for direct maintenanceDate field as well
            if (requestBody.containsKey("maintenanceDate")) {
                String dateStr = (String) requestBody.get("maintenanceDate");
                updateDTO.setMaintenanceDate(Timestamp.valueOf(dateStr.replace("T", " ")));
            }
            
            MaintenanceRequestResponseDTO response = maintenanceRequestService.updateMaintenanceRequest(maintenanceId, updateDTO);
            
            return ResponseEntity.ok(ApiResponse.success(response, "Maintenance request updated successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error("Failed to update maintenance request: " + e.getMessage()));
        }
    }

    // =================
    // GENERAL APIs
    // =================

    /**
     * Get all maintenance requests (general endpoint with filtering)
     * GET /api/fleet/maintenance-requests
     */
    @GetMapping("/fleet/maintenance-requests/all")
    public ResponseEntity<ApiResponse<Page<MaintenanceRequestResponseDTO>>> getAllMaintenanceRequests(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(required = false) Long statusId,
            @RequestParam(required = false) String maintenanceType,
            @RequestParam(required = false) Long vehicleId,
            @RequestParam(required = false) Long driverId) {
        try {
            Pageable pageable = PageRequest.of(page, size, Sort.by("createdAt").descending());
            Page<MaintenanceRequestResponseDTO> requests = maintenanceRequestService.getAllMaintenanceRequests(pageable);
            
            return ResponseEntity.ok(ApiResponse.success(requests, "All maintenance requests retrieved successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ApiResponse.error("Failed to retrieve maintenance requests: " + e.getMessage()));
        }
    }

    // =================
    // UTILITY APIs
    // =================

    /**
     * Get maintenance requests summary for dashboard
     * GET /api/fleet/maintenance-requests/summary
     */
    @GetMapping("/fleet/maintenance-requests/summary")
    public ResponseEntity<ApiResponse<Object>> getMaintenanceRequestsSummary() {
        try {
            // You can customize this summary based on requirements
            var summary = maintenanceRequestService.getMaintenanceRequestsSummary();
            return ResponseEntity.ok(ApiResponse.success(summary, "Maintenance requests summary retrieved successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ApiResponse.error("Failed to retrieve maintenance summary: " + e.getMessage()));
        }
    }

    /**
     * Search maintenance requests by keyword
     * GET /api/fleet/maintenance-requests/search
     */
    @GetMapping("/fleet/maintenance-requests/search")
    public ResponseEntity<ApiResponse<Page<MaintenanceRequestResponseDTO>>> searchMaintenanceRequests(
            @RequestParam String keyword,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size) {
        try {
            Pageable pageable = PageRequest.of(page, size, Sort.by("createdAt").descending());
            Page<MaintenanceRequestResponseDTO> results = maintenanceRequestService.searchMaintenanceRequests(keyword, pageable);
            
            return ResponseEntity.ok(ApiResponse.success(results, "Search results retrieved successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ApiResponse.error("Failed to search maintenance requests: " + e.getMessage()));
        }
    }

    /**
     * Delete maintenance request (admin only)
     * DELETE /api/fleet/maintenance-requests/{maintenanceId}
     */
    @DeleteMapping("/fleet/maintenance-requests/{maintenanceId}")
    public ResponseEntity<ApiResponse<Void>> deleteMaintenanceRequest(@PathVariable Long maintenanceId) {
        try {
            maintenanceRequestService.deleteMaintenanceRequest(maintenanceId);
            return ResponseEntity.ok(ApiResponse.success(null, "Maintenance request deleted successfully"));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error("Failed to delete maintenance request: " + e.getMessage()));
        }
    }
}