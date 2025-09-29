package ktc.spring_project.dtos.delivery;

import ktc.spring_project.dtos.order.OrderSimpleDTO;
import ktc.spring_project.dtos.user.UserResponseDTO;
import ktc.spring_project.dtos.vehicle.VehicleSimpleDTO;
import ktc.spring_project.dtos.store.StoreResponseDTO;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;
import lombok.Data;

/**
 * DTO for detailed delivery information including orders
 */
@Data
public class DeliveryDetailResponseDTO {
    private Long id;
    private String deliveryCode;
    private String status;
    private Short statusId;
    private Timestamp scheduledTime;
    private Timestamp actualDeliveryTime;
    private BigDecimal deliveryFee;
    private String notes;
    
    // Pickup information
    private String pickupAddress;
    private BigDecimal pickupLatitude;
    private BigDecimal pickupLongitude;
    
    // Route information
    private String routeName;
    private Long routeId;
    private BigDecimal estimatedDistance;
    private Integer estimatedDuration; // In minutes
    
    // Related entities
    private UserResponseDTO driver;
    private VehicleSimpleDTO vehicle;
    private List<OrderSimpleDTO> orders; // Simple order information
    private StoreResponseDTO store; // Added store information
    
    // Timestamps
    private Timestamp createdAt;
    private Timestamp updatedAt;
}
