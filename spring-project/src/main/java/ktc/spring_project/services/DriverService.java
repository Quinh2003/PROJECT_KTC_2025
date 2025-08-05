package ktc.spring_project.services;

import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.entities.Order;
import ktc.spring_project.repositories.UserRepository;
import ktc.spring_project.repositories.VehicleRepository;
import ktc.spring_project.repositories.OrderRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

/**
 * Service for handling driver-specific operations
 */
@Service
public class DriverService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private VehicleRepository vehicleRepository;

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private UserService userService;

    /**
     * Get current vehicle assigned to driver
     *
     * @param driverId Driver user ID
     * @return Currently assigned vehicle or null
     */
    public Vehicle getCurrentVehicle(Long driverId) {
        return vehicleRepository.findByCurrentDriverId(driverId);
    }

    /**
     * Get driver summary including assigned vehicle and current orders
     *
     * @param authentication Current authenticated user
     * @return Map with driver summary information
     */
    public Map<String, Object> getDriverSummary(Authentication authentication) {
        User driver = userService.getCurrentUser(authentication);

        // Get assigned vehicle
        Vehicle vehicle = getCurrentVehicle(driver.getId());

        // Get active orders
        List<Order> activeOrders = orderRepository.findActiveOrdersByDriverId(driver.getId());

        // Get delivered orders count today
        int deliveredToday = orderRepository.countDeliveredOrdersByDriverIdToday(driver.getId());

        // Assemble response
        Map<String, Object> summary = new HashMap<>();
        summary.put("driver", driver);
        summary.put("vehicle", vehicle);
        summary.put("activeOrdersCount", activeOrders.size());
        summary.put("activeOrders", activeOrders);
        summary.put("deliveredToday", deliveredToday);

        return summary;
    }

    /**
     * Get available orders for a driver to pick up
     *
     * @param authentication Current authenticated user
     * @return List of available orders
     */
    public List<Order> getAvailableOrders(Authentication authentication) {
        User driver = userService.getCurrentUser(authentication);

        // Get driver's vehicle to check capacity
        Vehicle vehicle = getCurrentVehicle(driver.getId());

        if (vehicle == null) {
            // No vehicle assigned, return empty list
            return List.of();
        }

        // Get orders that match the vehicle capacity and are in the same area
        return orderRepository.findAvailableOrdersForVehicle(
            vehicle.getCapacityWeightKg(),
            vehicle.getCapacityVolumeM3(),
            driver.getId()
        );
    }

    /**
     * Assign a driver to a vehicle
     *
     * @param driverId ID of the driver user
     * @param vehicleId ID of the vehicle to assign
     * @return Updated vehicle
     */
    public Vehicle assignVehicle(Long driverId, Long vehicleId) {
        Vehicle vehicle = vehicleRepository.findById(vehicleId)
            .orElseThrow(() -> new RuntimeException("Vehicle not found"));

        // Check if vehicle is already assigned
        if (vehicle.getCurrentDriverId() != null && !vehicle.getCurrentDriverId().equals(driverId)) {
            throw new RuntimeException("Vehicle already assigned to another driver");
        }

        // Unassign from previous vehicle if any
        vehicleRepository.clearDriverAssignment(driverId);

        // Assign to new vehicle
        vehicle.setCurrentDriverId(driverId);
        return vehicleRepository.save(vehicle);
    }
}
