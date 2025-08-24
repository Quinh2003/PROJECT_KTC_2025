package ktc.spring_project.controllers;

import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.services.VehicleService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.dtos.vehicle.PaginatedVehicleResponseDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Controller responsible for managing vehicles
 * Based on user stories:
 * - US-FLEET-LIST-01: View Vehicle List
 * - US-FLEET-MAINTAIN-01: Manage Vehicle Maintenance
 * - US-MAP-FILTER-03: Vehicle Filter & Search on Map
 */
@RestController
@RequestMapping("/api/vehicles")
public class VehicleController {

    @Autowired
    private VehicleService vehicleService;

    @Autowired
    private UserService userService;

    /**
     * Get all vehicles with optional filters and pagination
     * US-FLEET-LIST-01
     */
    @GetMapping
    public ResponseEntity<PaginatedVehicleResponseDto> getAllVehicles(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "5") int size,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String vehicleType,
            @RequestParam(required = false) Long driverId) {
        try {
            // Sử dụng phân trang thật từ database với sắp xếp theo updatedAt descending
            Page<Vehicle> vehiclePage = vehicleService.getVehiclesPaginated(page, size);
            
            // Chuyển đổi Vehicle entities thành DTO để tránh circular reference
            List<Map<String, Object>> vehicleDTOs = vehiclePage.getContent().stream().map(vehicle -> {
                Map<String, Object> vehicleDTO = new HashMap<>();
                vehicleDTO.put("id", vehicle.getId());
                vehicleDTO.put("licensePlate", vehicle.getLicensePlate());
                vehicleDTO.put("vehicleType", vehicle.getVehicleType());
                vehicleDTO.put("capacityWeightKg", vehicle.getCapacityWeightKg());
                vehicleDTO.put("capacityVolumeM3", vehicle.getCapacityVolumeM3());
                vehicleDTO.put("notes", vehicle.getNotes());
                vehicleDTO.put("createdAt", vehicle.getCreatedAt());
                vehicleDTO.put("updatedAt", vehicle.getUpdatedAt());
                
                // Status info
                if (vehicle.getStatus() != null) {
                    Map<String, Object> statusDTO = new HashMap<>();
                    statusDTO.put("id", vehicle.getStatus().getId());
                    statusDTO.put("name", vehicle.getStatus().getName());
                    statusDTO.put("statusType", vehicle.getStatus().getStatusType());
                    statusDTO.put("description", vehicle.getStatus().getDescription());
                    vehicleDTO.put("status", statusDTO);
                }
                
                // Current driver info
                if (vehicle.getCurrentDriver() != null) {
                    Map<String, Object> driverDTO = new HashMap<>();
                    driverDTO.put("id", vehicle.getCurrentDriver().getId());
                    driverDTO.put("fullName", vehicle.getCurrentDriver().getFullName());
                    driverDTO.put("username", vehicle.getCurrentDriver().getUsername());
                    driverDTO.put("email", vehicle.getCurrentDriver().getEmail());
                    vehicleDTO.put("currentDriver", driverDTO);
                }
                
                return vehicleDTO;
            }).collect(Collectors.toList());

            PaginatedVehicleResponseDto response = PaginatedVehicleResponseDto.builder()
                    .data(vehicleDTOs)
                    .pageNumber(page)
                    .pageSize(size)
                    .totalRecords(vehiclePage.getTotalElements())
                    .totalPages(vehiclePage.getTotalPages())
                    .hasNext(vehiclePage.hasNext())
                    .hasPrevious(vehiclePage.hasPrevious())
                    .build();
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            System.err.println("Error in getAllVehicles: " + e.getMessage());
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }
    }

    /**
     * Get vehicle by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Vehicle> getVehicleById(@PathVariable Long id) {
        Vehicle vehicle = vehicleService.getVehicleById(id);
        return ResponseEntity.ok(vehicle);
    }

    /**
     * Create a new vehicle
     */
    @PostMapping
    public ResponseEntity<Vehicle> createVehicle(
            @Valid @RequestBody Vehicle vehicle,
            Authentication authentication) {

        Vehicle createdVehicle = vehicleService.createVehicle(vehicle);
        return new ResponseEntity<>(createdVehicle, HttpStatus.CREATED);
    }

    /**
     * Update vehicle information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Vehicle> updateVehicle(
            @PathVariable Long id,
            @Valid @RequestBody Vehicle vehicle,
            Authentication authentication) {

        Vehicle updatedVehicle = vehicleService.updateVehicle(id, vehicle);
        return ResponseEntity.ok(updatedVehicle);
    }

    /**
     * Assign driver to vehicle
     */
    @PatchMapping("/{id}/assign-driver")
    public ResponseEntity<Vehicle> assignDriverToVehicle(
            @PathVariable Long id,
            @RequestBody Map<String, Long> driverData,
            Authentication authentication) {

        Long driverId = driverData.get("driverId");
        Vehicle vehicle = vehicleService.getVehicleById(id);

        if (driverId == null) {
            // Nếu driverId null, bỏ gán tài xế
            vehicle.setCurrentDriver(null);
        } else {
            // Gán tài xế mới
            User driver = userService.getUserById(driverId);
            vehicle.setCurrentDriver(driver);
        }

        Vehicle updatedVehicle = vehicleService.updateVehicle(id, vehicle);
        return ResponseEntity.ok(updatedVehicle);
    }

    /**
     * Schedule vehicle maintenance
     * US-FLEET-MAINTAIN-01
     */
    @PostMapping("/{id}/maintenance")
    public ResponseEntity<Map<String, Object>> scheduleVehicleMaintenance(
            @PathVariable Long id,
            @Valid @RequestBody Map<String, Object> maintenanceData,
            Authentication authentication) {

        // Extract maintenance data and convert to appropriate types
        String maintenanceType = (String) maintenanceData.get("maintenanceType");
        String scheduledDate = (String) maintenanceData.get("scheduledDate");
        String description = (String) maintenanceData.get("description");

        // Tạm thời trả về thông báo thành công
        // Đây là giải pháp tạm thời cho đến khi có MaintenanceService
        Map<String, Object> response = Map.of(
            "status", "scheduled",
            "vehicleId", id,
            "maintenanceType", maintenanceType,
            "scheduledDate", scheduledDate,
            "message", "Maintenance scheduled successfully"
        );

        return new ResponseEntity<>(response, HttpStatus.CREATED);
    }

    /**
     * Get maintenance history for a vehicle
     * US-FLEET-MAINTAIN-01
     */
    @GetMapping("/{id}/maintenance-history")
    public ResponseEntity<List<Map<String, Object>>> getVehicleMaintenanceHistory(@PathVariable Long id) {
        // Trả về danh sách trống tạm thời
        // Đây là giải pháp tạm thời cho đến khi có MaintenanceService
        return ResponseEntity.ok(List.of());
    }

    /**
     * Get current location of a vehicle
     * Supporting US-MAP-REALTIME-01 and US-ORDER-TRACK-01
     */
    @GetMapping("/{id}/location")
    public ResponseEntity<Map<String, Object>> getVehicleLocation(@PathVariable Long id) {
        // Lấy thông tin phương tiện
        Vehicle vehicle = vehicleService.getVehicleById(id);

        // Tạo dữ liệu giả về vị trí
        // Đây là giải pháp tạm thời cho đến khi có phương thức thích hợp trong VehicleService
        Map<String, Object> locationData = Map.of(
            "vehicleId", id,
            "licensePlate", vehicle.getLicensePlate(),
            "vehicleType", vehicle.getVehicleType(),
            "latitude", 10.762622, // Giá trị mẫu
            "longitude", 106.660172, // Giá trị mẫu
            "lastUpdated", System.currentTimeMillis()
        );

        return ResponseEntity.ok(locationData);
    }

    // ...existing code...

@DeleteMapping("/{id}")
public ResponseEntity<Void> deleteVehicle(@PathVariable Long id, Authentication authentication) {
    vehicleService.deleteVehicle(id);
    return ResponseEntity.noContent().build();
}

// ...existing code...

    /**
     * Get active vehicle locations for map display
     * US-MAP-REALTIME-01
     * TO-DO: Future integration with AI service for real-time vehicle tracking and prediction
     */
    @GetMapping("/active-locations")
    public ResponseEntity<List<Map<String, Object>>> getActiveVehicleLocations(
            @RequestParam(required = false) String vehicleType,
            @RequestParam(required = false) String status) {

        // TO-DO: This is a temporary implementation.
        // When AI model is deployed to separate server,
        // this endpoint will call the AI service to get accurate real-time locations
        // and predictive path analysis for vehicles on routes

        // Lấy tất cả phương tiện
        List<Vehicle> vehicles = vehicleService.getAllVehicles();

        // Tạo danh sách vị trí giả
        List<Map<String, Object>> activeLocations = new ArrayList<>();

        // Tạo dữ liệu mẫu cho từng phương tiện
        for (Vehicle vehicle : vehicles) {
            // Chỉ thêm phương tiện thỏa mãn điều kiện lọc
            if ((vehicleType == null || vehicleType.equals(vehicle.getVehicleType())) &&
                (status == null || (vehicle.getStatus() != null && status.equals(vehicle.getStatus().getName())))) {

                Map<String, Object> locationData = Map.of(
                    "vehicleId", vehicle.getId(),
                    "licensePlate", vehicle.getLicensePlate(),
                    "vehicleType", vehicle.getVehicleType(),
                    "latitude", 10.762622 + (Math.random() * 0.01), // Vị trí ngẫu nhiên quanh TP.HCM
                    "longitude", 106.660172 + (Math.random() * 0.01), // Vị trí ngẫu nhiên quanh TP.HCM
                    "lastUpdated", System.currentTimeMillis()
                );

                activeLocations.add(locationData);
            }
        }

        return ResponseEntity.ok(activeLocations);
    }
}
