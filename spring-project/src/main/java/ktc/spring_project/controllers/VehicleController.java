package ktc.spring_project.controllers;

import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.services.VehicleService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.MaintenanceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

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

    @Autowired
    private MaintenanceService maintenanceService;

    /**
     * Get all vehicles with optional filters
     * US-FLEET-LIST-01
     */
    @GetMapping
    public ResponseEntity<List<Vehicle>> getAllVehicles(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String vehicleType,
            @RequestParam(required = false) Long driverId) {

        List<Vehicle> vehicles = vehicleService.getFilteredVehicles(status, vehicleType, driverId);
        return ResponseEntity.ok(vehicles);
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

        Vehicle updatedVehicle = vehicleService.assignDriverToVehicle(id, driverId);
        return ResponseEntity.ok(updatedVehicle);
    }

    /**
     * Schedule vehicle maintenance
     * US-FLEET-MAINTAIN-01
     */
    @PostMapping("/{id}/maintenance")
    public ResponseEntity<?> scheduleVehicleMaintenance(
            @PathVariable Long id,
            @Valid @RequestBody Map<String, Object> maintenanceData,
            Authentication authentication) {

        // Extract maintenance data and convert to appropriate types
        String maintenanceType = (String) maintenanceData.get("maintenanceType");
        String scheduledDate = (String) maintenanceData.get("scheduledDate");
        String description = (String) maintenanceData.get("description");

        // Create maintenance record
        Object maintenanceRecord = maintenanceService.scheduleVehicleMaintenance(
                id, maintenanceType, scheduledDate, description);

        return new ResponseEntity<>(maintenanceRecord, HttpStatus.CREATED);
    }

    /**
     * Get maintenance history for a vehicle
     * US-FLEET-MAINTAIN-01
     */
    @GetMapping("/{id}/maintenance-history")
    public ResponseEntity<List<?>> getVehicleMaintenanceHistory(@PathVariable Long id) {
        List<?> maintenanceHistory = maintenanceService.getVehicleMaintenanceHistory(id);
        return ResponseEntity.ok(maintenanceHistory);
    }

    /**
     * Get current location of a vehicle
     * Supporting US-MAP-REALTIME-01 and US-ORDER-TRACK-01
     */
    @GetMapping("/{id}/location")
    public ResponseEntity<Map<String, Object>> getVehicleLocation(@PathVariable Long id) {
        Map<String, Object> locationData = vehicleService.getVehicleLocation(id);
        return ResponseEntity.ok(locationData);
    }

    /**
     * Get active vehicle locations for map display
     * US-MAP-REALTIME-01
     */
    @GetMapping("/active-locations")
    public ResponseEntity<List<Map<String, Object>>> getActiveVehicleLocations(
            @RequestParam(required = false) String vehicleType,
            @RequestParam(required = false) String status) {

        List<Map<String, Object>> activeLocations = vehicleService.getActiveVehicleLocations(vehicleType, status);
        return ResponseEntity.ok(activeLocations);
    }
}
