package ktc.spring_project.services;

import ktc.spring_project.exceptions.HttpException;
import ktc.spring_project.exceptions.EntityNotFoundException;
import ktc.spring_project.exceptions.EntityDuplicateException;
import ktc.spring_project.dtos.DeliveryFeeBreakdown;
import ktc.spring_project.dtos.delivery.CreateDeliveryRequestDTO;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.Order;
import ktc.spring_project.repositories.DeliveryRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;

@Service
@Slf4j
public class DeliveryService {

    @Autowired
    private DeliveryRepository deliveryRepository;

    @Autowired
    private DeliveryFeeCalculationService deliveryFeeCalculationService;

    @Autowired
    private OrderService orderService;
    
    @Autowired
    private VehicleService vehicleService;
    
    @Autowired
    private UserService userService;
    
    @Autowired
    private RouteService routeService;

    public Delivery createDelivery(Delivery delivery) {
        if (delivery.getDeliveryAttempts() != null && delivery.getDeliveryAttempts() < 0) {
            throw new HttpException("Delivery attempts cannot be negative", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        if (delivery.getDeliveryFee() != null && delivery.getDeliveryFee().compareTo(BigDecimal.ZERO) < 0) {
            throw new HttpException("Delivery fee cannot be negative", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        return deliveryRepository.save(delivery);
    }

    /**
     * Tạo Delivery với tự động tính deliveryFee
     * Sử dụng method này thay cho createDelivery() để có deliveryFee tự động
     */
    public Delivery createDeliveryWithFeeCalculation(Delivery delivery) {
        log.info("Creating delivery with automatic fee calculation for Order ID: {}", 
                delivery.getOrder().getId());
        
        try {
            // 1. Validate input
            if (delivery.getDeliveryAttempts() != null && delivery.getDeliveryAttempts() < 0) {
                throw new HttpException("Delivery attempts cannot be negative", org.springframework.http.HttpStatus.BAD_REQUEST);
            }
            
            // 2. Tính deliveryFee tự động
            DeliveryFeeBreakdown feeBreakdown = deliveryFeeCalculationService.calculateDeliveryFee(
                delivery.getOrder(), 
                delivery.getServiceType()
            );
            
            // 3. Set deliveryFee vào entity
            delivery.setDeliveryFee(feeBreakdown.getTotalDeliveryFee());
            
            // 4. Log chi tiết tính toán
            log.info("Delivery fee calculation completed:");
            log.info("- Order ID: {}", feeBreakdown.getOrderId());
            log.info("- Service Type: {}", feeBreakdown.getServiceType());
            log.info("- Total Shipping Fee: {}", feeBreakdown.getTotalShippingFee());
            log.info("- Distance Fee: {}", feeBreakdown.getDistanceFee());
            log.info("- Base Delivery Fee: {}", feeBreakdown.getBaseDeliveryFee());
            log.info("- Service Multiplier: {}", feeBreakdown.getServiceMultiplier());
            log.info("- TOTAL DELIVERY FEE: {}", feeBreakdown.getTotalDeliveryFee());
            
            // 5. Save delivery
            Delivery savedDelivery = deliveryRepository.save(delivery);
            log.info("Delivery created successfully with ID: {} and fee: {}", 
                    savedDelivery.getId(), savedDelivery.getDeliveryFee());
            
            return savedDelivery;
            
        } catch (Exception e) {
            log.error("Error creating delivery with fee calculation: {}", e.getMessage(), e);
            throw e;
        }
    }

    public Delivery getDeliveryById(Long id) {
        return deliveryRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Delivery not found with id: " + id));
    }

    public List<Delivery> getAllDeliveries() {
        return deliveryRepository.findAll();
    }

    public Delivery updateDelivery(Long id, Delivery deliveryDetails) {
        Delivery delivery = getDeliveryById(id);
        if (delivery.getDeliveryAttempts() != null && delivery.getDeliveryAttempts() < 0) {
            throw new HttpException("Delivery attempts cannot be negative", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        if (delivery.getDeliveryFee() != null && delivery.getDeliveryFee().compareTo(BigDecimal.ZERO) < 0) {
            throw new HttpException("Delivery fee cannot be negative", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        delivery.setOrder(deliveryDetails.getOrder());
        delivery.setDeliveryFee(deliveryDetails.getDeliveryFee());
        delivery.setTransportMode(deliveryDetails.getTransportMode());
        delivery.setServiceType(deliveryDetails.getServiceType());
        delivery.setOrderDate(deliveryDetails.getOrderDate());
        delivery.setPickupDate(deliveryDetails.getPickupDate());
        delivery.setScheduleDeliveryTime(deliveryDetails.getScheduleDeliveryTime());
        delivery.setActualDeliveryTime(deliveryDetails.getActualDeliveryTime());
        delivery.setLateDeliveryRisk(deliveryDetails.getLateDeliveryRisk());
        delivery.setVehicle(deliveryDetails.getVehicle());
        delivery.setDriver(deliveryDetails.getDriver());
delivery.setTrackingPoints(deliveryDetails.getTrackingPoints());
        delivery.setRoute(deliveryDetails.getRoute());
        delivery.setDeliveryAttempts(deliveryDetails.getDeliveryAttempts());
        delivery.setDeliveryNotes(deliveryDetails.getDeliveryNotes());
        return deliveryRepository.save(delivery);
    }

    public void deleteDelivery(Long id) {
        Delivery delivery = getDeliveryById(id);
        deliveryRepository.delete(delivery);
    }

    /**
     * Duplicate check for delivery (orderId, vehicleId, driverId, routeId)
     */
    public boolean isDuplicateDelivery(Long orderId, Long vehicleId, Long driverId, Long routeId) {
        List<Delivery> deliveries = deliveryRepository.findByOrderId(orderId);
        for (Delivery d : deliveries) {
            if ((vehicleId == null || (d.getVehicle() != null && vehicleId.equals(d.getVehicle().getId()))) &&
                (driverId == null || (d.getDriver() != null && driverId.equals(d.getDriver().getId()))) &&
                (routeId == null || (d.getRoute() != null && routeId.equals(d.getRoute().getId())))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Kiểm tra order có tồn tại delivery với orderId
     */
    public boolean existsByOrderId(Long orderId) {
        return deliveryRepository.existsByOrderId(orderId);
    }

    /**
     * Tạo delivery từ DTO với validation và exception handling đầy đủ
     */
    public Delivery createDeliveryFromDTO(CreateDeliveryRequestDTO dto) {
        log.info("Creating delivery from DTO for Order ID: {}", dto.getOrderId());
        
        try {
            // 1. Kiểm tra trùng lặp
            if (existsByOrderId(dto.getOrderId())) {
                throw new EntityDuplicateException(
                    "Delivery for this order already exists: orderId=" + dto.getOrderId());
            }
            
            // 2. Tạo entity từ DTO với validation
            Delivery delivery = new Delivery();
            
            // Validate và set Order
            try {
                Order order = orderService.getOrderById(dto.getOrderId());
                delivery.setOrder(order);
                
                // Kiểm tra order có address không
                if (order.getAddress() == null) {
                    log.warn("Order {} does not have delivery address, will skip fee calculation", dto.getOrderId());
                }
            } catch (Exception e) {
                throw new EntityNotFoundException("Order not found with id: " + dto.getOrderId());
            }
            
            // Validate và set Vehicle
            try {
                delivery.setVehicle(vehicleService.getVehicleById(dto.getVehicleId()));
            } catch (Exception e) {
                throw new EntityNotFoundException("Vehicle not found with id: " + dto.getVehicleId());
            }
            
            // Set optional Driver
            if (dto.getDriverId() != null) {
                try {
                    delivery.setDriver(userService.getUserById(dto.getDriverId()));
                } catch (Exception e) {
                    throw new EntityNotFoundException("Driver not found with id: " + dto.getDriverId());
                }
            }
            
            // Set optional Route
            if (dto.getRouteId() != null) {
                try {
                    delivery.setRoute(routeService.getRouteById(dto.getRouteId()));
                } catch (Exception e) {
                    throw new EntityNotFoundException("Route not found with id: " + dto.getRouteId());
                }
            }
            
            // Set các thuộc tính khác
            delivery.setTransportMode(dto.getTransportMode());
            delivery.setServiceType(dto.getServiceType());
            delivery.setPickupDate(dto.getPickupDate());
            delivery.setScheduleDeliveryTime(dto.getScheduleDeliveryTime());
            delivery.setLateDeliveryRisk(dto.getLateDeliveryRisk() != null && dto.getLateDeliveryRisk() ? 1 : 0);
            delivery.setDeliveryNotes(dto.getDeliveryNotes());
            delivery.setOrderDate(dto.getOrderDate());
            
            // 3. Tạo delivery với hoặc không có fee calculation
            if (delivery.getOrder() != null && delivery.getOrder().getAddress() != null) {
                return createDeliveryWithFeeCalculation(delivery);
            } else {
                return createDelivery(delivery);
            }
            
        } catch (EntityNotFoundException | EntityDuplicateException e) {
            log.error("Validation error creating delivery: {}", e.getMessage());
            throw e;
        } catch (Exception e) {
            log.error("Unexpected error creating delivery: {}", e.getMessage(), e);
            throw new HttpException(
                "Failed to create delivery: " + e.getMessage(), 
                org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Convert CreateDeliveryRequestDTO to Delivery entity
     */
    public Delivery convertFromDTO(
        CreateDeliveryRequestDTO dto,
        OrderService orderService,
        VehicleService vehicleService,
        UserService userService,
        RouteService routeService
    ) {
        Delivery delivery = new Delivery();
        if (dto.getOrderId() != null) {
            delivery.setOrder(orderService.getOrderById(dto.getOrderId()));
        }
    delivery.setDeliveryFee(dto.getDeliveryFee());
    delivery.setTransportMode(dto.getTransportMode());
    delivery.setServiceType(dto.getServiceType());
    delivery.setPickupDate(dto.getPickupDate());
    delivery.setScheduleDeliveryTime(dto.getScheduleDeliveryTime());
    // Chuyển đổi Boolean sang Integer: true -> 1, false/null -> 0
    delivery.setLateDeliveryRisk(dto.getLateDeliveryRisk() != null && dto.getLateDeliveryRisk() ? 1 : 0);
    delivery.setDeliveryNotes(dto.getDeliveryNotes());
    delivery.setOrderDate(dto.getOrderDate());
        if (dto.getVehicleId() != null) {
            delivery.setVehicle(vehicleService.getVehicleById(dto.getVehicleId()));
        }
        if (dto.getDriverId() != null) {
            delivery.setDriver(userService.getUserById(dto.getDriverId()));
        }
        if (dto.getRouteId() != null) {
            delivery.setRoute(routeService.getRouteById(dto.getRouteId()));
        }
        return delivery;
    }
}