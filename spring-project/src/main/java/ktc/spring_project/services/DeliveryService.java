package ktc.spring_project.services;

import ktc.spring_project.exceptions.HttpException;
import ktc.spring_project.exceptions.EntityNotFoundException;
import ktc.spring_project.exceptions.EntityDuplicateException;
import ktc.spring_project.dtos.DeliveryFeeBreakdown;
import ktc.spring_project.dtos.address.AddressResponseDTO;
import ktc.spring_project.dtos.delivery.DeliveryDetailResponseDTO;
import ktc.spring_project.dtos.delivery.DeliveryResponseDTO;
import ktc.spring_project.dtos.order.DeliveryOrderResponseDTO;
import ktc.spring_project.dtos.order.DriverOrderSimpleResponseDTO;
import ktc.spring_project.dtos.order.OrderDetailResponseDTO;
import ktc.spring_project.dtos.order.OrderSimpleDTO;
import ktc.spring_project.dtos.orderitem.OrderItemResponseDTO;
import ktc.spring_project.dtos.store.StoreResponseDTO;
import ktc.spring_project.dtos.user.UserResponseDTO;
import ktc.spring_project.dtos.vehicle.VehicleSimpleDTO;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.Payment;
import ktc.spring_project.entities.Store;
import ktc.spring_project.repositories.DeliveryRepository;
import ktc.spring_project.repositories.OrderItemRepository;
import ktc.spring_project.services.OrderService;
import ktc.spring_project.services.VehicleService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.services.RouteService;
import ktc.spring_project.repositories.PaymentRepository;
// import jakarta.persistence.EntityNotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.math.BigDecimal;

@Slf4j
@Service
public class DeliveryService {

    @Autowired
    private PaymentRepository paymentRepository;

    @Autowired
    private DeliveryRepository deliveryRepository;

    @Autowired
    private DeliveryFeeCalculationService deliveryFeeCalculationService;

    @Autowired
    private OrderItemRepository orderItemRepository;
    
    @Autowired
    private OrderService orderService;
    
    @Autowired
    private VehicleService vehicleService;
    
    @Autowired
    private UserService userService;
    
    @Autowired
    private RouteService routeService;
    
    @Autowired
    private ChecklistService checklistService;

    /**
     * Tìm delivery theo orderId
     */
    public List<Delivery> findByOrderId(Long orderId) {
        return deliveryRepository.findByOrderId(orderId);
    }

    /**
     * Lưu delivery
     */
    public Delivery save(Delivery delivery) {
        return deliveryRepository.save(delivery);
    }

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
                .orElseThrow(() -> new ktc.spring_project.exceptions.EntityNotFoundException("Delivery not found with id: " + id));
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

    public List<DriverOrderSimpleResponseDTO> getOrdersByDriverId(Long driverId) {
        List<Delivery> deliveries = deliveryRepository.findByDriverId(driverId);
        return deliveries.stream()
                .map(this::toDriverOrderSimpleResponseDTO)
                .collect(Collectors.toList());
    }

    private DriverOrderSimpleResponseDTO toDriverOrderSimpleResponseDTO(Delivery delivery) {
        DriverOrderSimpleResponseDTO dto = new DriverOrderSimpleResponseDTO();
        if (delivery == null || delivery.getOrder() == null) return dto;
        var order = delivery.getOrder();
        dto.setId(order.getId());
        dto.setOrderCode(null); // Nếu có trường orderCode thì lấy, chưa có thì để null
        dto.setStatus(order.getStatus() != null ? order.getStatus().getName() : null);
        // Lấy thông tin khách hàng từ address (nếu có)
        if (order.getAddress() != null) {
            dto.setCustomerName(order.getAddress().getContactName());
            dto.setCustomerPhone(order.getAddress().getContactPhone());
            dto.setDeliveryAddress(order.getAddress().getAddress());
        }
        // Lấy nơi lấy hàng từ store (nếu có)
        if (order.getStore() != null) {
            dto.setPickupAddress(order.getStore().getAddress());
        }
        dto.setScheduledTime(delivery.getScheduleDeliveryTime());
        dto.setDeliveryFee(delivery.getDeliveryFee());
        // Lấy phương thức thanh toán từ payment nếu có (tìm payment đầu tiên liên quan đến order)
        // Lấy phương thức thanh toán từ payment đầu tiên theo orderId
        String paymentMethodDisplay = null;
        List<Payment> payments = paymentRepository.findByOrderId(order.getId());
        if (payments != null && !payments.isEmpty()) {
            Payment payment = payments.get(0);
            if (payment != null && payment.getPaymentMethod() != null) {
                paymentMethodDisplay = payment.getPaymentMethod().getDisplayName();
            }
        }
        dto.setPaymentMethod(paymentMethodDisplay);
        dto.setPriority(order.getNotes()); // Nếu có trường priority riêng thì lấy, tạm thời lấy notes
        return dto;
    }

    /**
     * Map Order + Delivery sang DeliveryOrderResponseDTO (tối giản, có thể mở rộng thêm fields nếu cần)
     */
    private DeliveryOrderResponseDTO toDeliveryOrderResponseDTO(Order order, Delivery delivery) {
        DeliveryOrderResponseDTO dto = new DeliveryOrderResponseDTO();
        if (order == null) return dto;
        dto.setId(order.getId());
        // dto.setOrderCode(null); // Nếu có trường orderCode thì lấy, chưa có thì để null
        dto.setDeliveryAddress(order.getAddress() != null ? order.getAddress().getAddress() : null);
        dto.setRecipientName(null); // Nếu có trường recipient thì lấy, chưa có thì để null
        dto.setScheduledTime(delivery != null ? delivery.getScheduleDeliveryTime() : null);
        dto.setTotalAmount(order.getTotalAmount());
        dto.setNotes(order.getNotes());
        dto.setStatusId(order.getStatus() != null ? order.getStatus().getId() : null);
        dto.setStatusDescription(order.getStatus() != null ? order.getStatus().getName() : null);
        dto.setCreatedAt(order.getCreatedAt());
        dto.setUpdatedAt(order.getUpdatedAt());
        // Driver info
        if (delivery != null && delivery.getDriver() != null) {
            UserResponseDTO driverDto = new UserResponseDTO();
            driverDto.setId(delivery.getDriver().getId());
            driverDto.setFullName(delivery.getDriver().getFullName());
            driverDto.setEmail(delivery.getDriver().getEmail());
            driverDto.setPhone(delivery.getDriver().getPhone());
            dto.setDriver(driverDto);
        }
        // Vehicle info
        if (order.getVehicle() != null) {
            dto.setVehicleId(order.getVehicle().getId());
            dto.setVehicleLicensePlate(order.getVehicle().getLicensePlate());
            dto.setVehicleType(order.getVehicle().getVehicleType() != null ? order.getVehicle().getVehicleType().name() : null);
        }
        return dto;
        }

    public OrderDetailResponseDTO getOrderDetailById(Long orderId) {
    List<Delivery> deliveries = deliveryRepository.findByOrderId(orderId);
    Delivery delivery = deliveries != null && !deliveries.isEmpty() ? deliveries.get(0) : null;
    if (delivery == null) return null;

    Order order = delivery.getOrder();
    OrderDetailResponseDTO dto = new OrderDetailResponseDTO();
    // Chỉ set các trường cần thiết
    dto.setId(order.getId());
    dto.setStatus(order.getStatus() != null ? order.getStatus().getName() : null);
    dto.setDescription(order.getDescription());
    dto.setNotes(order.getNotes());

    // Map address: id, address, latitude, longitude, contactName, contactPhone
    if (order.getAddress() != null) {
        AddressResponseDTO addressDTO = new AddressResponseDTO();
        addressDTO.setId(order.getAddress().getId());
        addressDTO.setAddress(order.getAddress().getAddress());
        addressDTO.setLatitude(order.getAddress().getLatitude());
        addressDTO.setLongitude(order.getAddress().getLongitude());
        addressDTO.setContactName(order.getAddress().getContactName());
        addressDTO.setContactPhone(order.getAddress().getContactPhone());
        dto.setAddress(addressDTO);
    }

    // Map store: id, storeName, phone, address, latitude, longitude
    if (order.getStore() != null) {
        StoreResponseDTO storeDTO = new StoreResponseDTO();
        storeDTO.setId(order.getStore().getId());
        storeDTO.setStoreName(order.getStore().getStoreName());
        storeDTO.setPhone(order.getStore().getPhone());
        storeDTO.setAddress(order.getStore().getAddress());
        if (order.getStore().getLatitude() != null) {
            storeDTO.setLatitude(order.getStore().getLatitude().doubleValue());
        }
        if (order.getStore().getLongitude() != null) {
            storeDTO.setLongitude(order.getStore().getLongitude().doubleValue());
        }
        dto.setStore(storeDTO);
    }

    // Map delivery: id, orderId, deliveryFee, orderDate
    if (delivery != null) {
        DeliveryResponseDTO deliveryDTO = new DeliveryResponseDTO();
        deliveryDTO.setId(delivery.getId());
        deliveryDTO.setOrderId(order.getId());
        deliveryDTO.setDeliveryFee(delivery.getDeliveryFee());
        deliveryDTO.setOrderDate(delivery.getOrderDate());
        dto.setDelivery(deliveryDTO);
    }

    // Map orderItems: id, productName, quantity, volume, weight, shippingFee
    List<ktc.spring_project.entities.OrderItem> orderItems = orderItemRepository.findByOrderId(order.getId());
    if (orderItems != null && !orderItems.isEmpty()) {
        List<OrderItemResponseDTO> itemDTOs = orderItems.stream().map(item -> {
            OrderItemResponseDTO itemDTO = new OrderItemResponseDTO();
            itemDTO.setId(item.getId());
            itemDTO.setProductName(item.getProduct() != null ? item.getProduct().getName() : null);
            itemDTO.setQuantity(item.getQuantity());
            itemDTO.setVolume(item.getProduct() != null ? item.getProduct().getVolume() : null);
            itemDTO.setWeight(item.getProduct() != null ? item.getProduct().getWeight() : null);
            itemDTO.setShippingFee(item.getShippingFee());
            return itemDTO;
        }).collect(Collectors.toList());
        dto.setOrderItems(itemDTOs);
    }
    return dto;
}

/**
 * Get all deliveries for a driver
 */
public List<DeliveryResponseDTO> getDeliveriesByDriverId(Long driverId) {
    List<Delivery> deliveries = deliveryRepository.findByDriverId(driverId);
    return deliveries.stream()
            .map(this::mapToDeliveryResponseDTO)
            .collect(Collectors.toList());
}

/**
 * Get detailed information about a specific delivery
 */
public DeliveryDetailResponseDTO getDeliveryDetailById(Long deliveryId) {
    Delivery delivery = deliveryRepository.findById(deliveryId)
            .orElseThrow(() -> new RuntimeException("Delivery not found with id: " + deliveryId));
    
    return mapToDeliveryDetailResponseDTO(delivery);
}

/**
 * Map Delivery entity to DeliveryResponseDTO
 */
private DeliveryResponseDTO mapToDeliveryResponseDTO(Delivery delivery) {
    DeliveryResponseDTO dto = new DeliveryResponseDTO();
    dto.setId(delivery.getId());
    
    // Map other fields from delivery entity
    if (delivery.getStatus() != null) {
        dto.setStatusId(delivery.getStatus().getId().longValue());
        dto.setDeliveryStatus(delivery.getStatus().getName());
    }
    
    dto.setScheduleDeliveryTime(delivery.getScheduleDeliveryTime());
    // Thêm scheduledTime cho backward compatibility
    dto.setScheduledTime(delivery.getScheduleDeliveryTime());
    dto.setActualDeliveryTime(delivery.getActualDeliveryTime());
    dto.setDeliveryFee(delivery.getDeliveryFee());
    
    // Map order info if available
    if (delivery.getOrder() != null) {
        Order order = delivery.getOrder();
        dto.setOrderId(order.getId());
        
        if (order.getAddress() != null) {
            dto.setDeliveryAddress(order.getAddress().getAddress());
        }
    }
    
    // Map vehicle info if available
    if (delivery.getVehicle() != null) {
        dto.setVehicleId(delivery.getVehicle().getId());
        dto.setLicensePlate(delivery.getVehicle().getLicensePlate());
    }
    
    return dto;
}

/**
 * Map Delivery entity to DeliveryDetailResponseDTO
 */
private DeliveryDetailResponseDTO mapToDeliveryDetailResponseDTO(Delivery delivery) {
    DeliveryDetailResponseDTO dto = new DeliveryDetailResponseDTO();
    dto.setId(delivery.getId());
    
    // Map status info
    if (delivery.getStatus() != null) {
        dto.setStatusId(delivery.getStatus().getId());
        dto.setStatus(delivery.getStatus().getName());
    }
    
    // Map delivery info
    dto.setScheduledTime(delivery.getScheduleDeliveryTime());
    dto.setActualDeliveryTime(delivery.getActualDeliveryTime());
    dto.setDeliveryFee(delivery.getDeliveryFee());
    dto.setNotes(delivery.getNotes());
    dto.setCreatedAt(delivery.getCreatedAt());
    dto.setUpdatedAt(delivery.getUpdatedAt());
    
    // Map route info if available
    if (delivery.getRoute() != null) {
        dto.setRouteId(delivery.getRoute().getId());
        dto.setRouteName(delivery.getRoute().getName());
        dto.setEstimatedDistance(delivery.getRoute().getEstimatedDistance());
        dto.setEstimatedDuration(delivery.getRoute().getEstimatedDuration());
    }
    
    // Map driver info if available
    if (delivery.getDriver() != null) {
        UserResponseDTO driverDto = new UserResponseDTO();
        driverDto.setId(delivery.getDriver().getId());
        driverDto.setFullName(delivery.getDriver().getFullName());
        driverDto.setEmail(delivery.getDriver().getEmail());
        driverDto.setPhone(delivery.getDriver().getPhone());
        dto.setDriver(driverDto);
    }
    
    // Map vehicle info if available
    if (delivery.getVehicle() != null) {
        VehicleSimpleDTO vehicleDto = new VehicleSimpleDTO();
        vehicleDto.setId(delivery.getVehicle().getId());
        vehicleDto.setLicensePlate(delivery.getVehicle().getLicensePlate());
        vehicleDto.setVehicleType(delivery.getVehicle().getVehicleType() != null ? 
                delivery.getVehicle().getVehicleType().name() : null);
        vehicleDto.setModel(delivery.getVehicle().getModel());
        dto.setVehicle(vehicleDto);
    }
    
    // Map orders if available
    if (delivery.getOrder() != null) {
        Order order = delivery.getOrder();
        OrderSimpleDTO orderDto = new OrderSimpleDTO();
        orderDto.setId(order.getId());
        
        if (order.getStatus() != null) {
            orderDto.setStatusId(order.getStatus().getId());
            orderDto.setStatus(order.getStatus().getName());
        }
        
        if (order.getAddress() != null) {
            orderDto.setDeliveryAddress(order.getAddress().getAddress());
            orderDto.setRecipientName(order.getAddress().getContactName());
            orderDto.setRecipientPhone(order.getAddress().getContactPhone());
        }
        
        orderDto.setTotalAmount(order.getTotalAmount());
        orderDto.setCreatedAt(order.getCreatedAt());
        
        dto.setOrders(List.of(orderDto));
        
        // Map store information if available
        if (order.getStore() != null) {
            StoreResponseDTO storeDto = new StoreResponseDTO();
            storeDto.setId(order.getStore().getId());
            storeDto.setStoreName(order.getStore().getStoreName());
            storeDto.setAddress(order.getStore().getAddress());
            storeDto.setPhone(order.getStore().getPhone());
            storeDto.setEmail(order.getStore().getEmail());
            
            // Convert BigDecimal to Double for latitude and longitude
            if (order.getStore().getLatitude() != null) {
                storeDto.setLatitude(order.getStore().getLatitude().doubleValue());
            }
            if (order.getStore().getLongitude() != null) {
                storeDto.setLongitude(order.getStore().getLongitude().doubleValue());
            }
            
            storeDto.setNotes(order.getStore().getNotes());
            storeDto.setIsActive(order.getStore().getIsActive());
            storeDto.setCreatedAt(order.getStore().getCreatedAt());
            storeDto.setUpdatedAt(order.getStore().getUpdatedAt());
            
            dto.setStore(storeDto);
        }
    }
    
    return dto;
}

    // --- Các method nâng cao từ bản mới ---

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
    public Delivery createDeliveryFromDTO(ktc.spring_project.dtos.delivery.CreateDeliveryRequestDTO dto) {
        log.info("Creating delivery from DTO for Order ID: {}", dto.getOrderId());
        try {
            if (existsByOrderId(dto.getOrderId())) {
                throw new EntityDuplicateException(
                    "Delivery for this order already exists: orderId=" + dto.getOrderId());
            }
            Delivery delivery = new Delivery();
            try {
                Order order = orderService.getOrderById(dto.getOrderId());
                delivery.setOrder(order);
                if (order.getAddress() == null) {
                    log.warn("Order {} does not have delivery address, will skip fee calculation", dto.getOrderId());
                }
            } catch (Exception e) {
                throw new ktc.spring_project.exceptions.EntityNotFoundException("Order not found with id: " + dto.getOrderId());
            }
            if (dto.getVehicleId() != null) {
                try {
                    delivery.setVehicle(vehicleService.getVehicleById(dto.getVehicleId()));
                } catch (Exception e) {
                    throw new ktc.spring_project.exceptions.EntityNotFoundException("Vehicle not found with id: " + dto.getVehicleId());
                }
            }
            if (dto.getDriverId() != null) {
                try {
                    delivery.setDriver(userService.getUserById(dto.getDriverId()));
                } catch (Exception e) {
                    throw new ktc.spring_project.exceptions.EntityNotFoundException("Driver not found with id: " + dto.getDriverId());
                }
            }
            if (dto.getRouteId() != null) {
                try {
                    delivery.setRoute(routeService.getRouteById(dto.getRouteId()));
                } catch (Exception e) {
                    throw new ktc.spring_project.exceptions.EntityNotFoundException("Route not found with id: " + dto.getRouteId());
                }
            }
            delivery.setTransportMode(dto.getTransportMode());
            delivery.setServiceType(dto.getServiceType());
            delivery.setPickupDate(dto.getPickupDate() != null ? java.sql.Timestamp.valueOf(dto.getPickupDate().atStartOfDay()) : null);
            delivery.setScheduleDeliveryTime(dto.getScheduleDeliveryTime());
            delivery.setLateDeliveryRisk(dto.getLateDeliveryRisk() != null && dto.getLateDeliveryRisk() ? 1 : 0);
            delivery.setDeliveryNotes(dto.getDeliveryNotes());
            delivery.setOrderDate(dto.getOrderDate());
            
            // SỬ DỤNG DELIVERY FEE TỪ FRONTEND THAY VÌ TÍNH LẠI
            if (dto.getDeliveryFee() != null) {
                log.info("Using delivery fee from frontend: {}", dto.getDeliveryFee());
                delivery.setDeliveryFee(dto.getDeliveryFee());
                return createDelivery(delivery);
            } else if (delivery.getOrder() != null && delivery.getOrder().getAddress() != null) {
                log.info("Creating delivery with automatic fee calculation for Order ID: {}", dto.getOrderId());
                return createDeliveryWithFeeCalculation(delivery);
            } else {
                return createDelivery(delivery);
            }
    } catch (ktc.spring_project.exceptions.EntityNotFoundException | EntityDuplicateException e) {
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
        ktc.spring_project.dtos.delivery.CreateDeliveryRequestDTO dto,
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
    delivery.setPickupDate(dto.getPickupDate() != null ? java.sql.Timestamp.valueOf(dto.getPickupDate().atStartOfDay()) : null);
    delivery.setScheduleDeliveryTime(dto.getScheduleDeliveryTime());
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
    
    // ================ CHECKLIST INTEGRATION METHODS ================
    
    /**
     * Driver nhận delivery order từ dispatcher
     */
    public void driverReceiveDeliveryOrder(Long driverId, Long deliveryId) {
        try {
            Delivery delivery = deliveryRepository.findById(deliveryId)
                .orElseThrow(() -> new ktc.spring_project.exceptions.EntityNotFoundException("Delivery not found with id: " + deliveryId));
            
            // Log checklist step
            checklistService.markStepCompleted(
                driverId, 
                delivery.getOrder().getId(),
                "DRIVER_RECEIVE_DELIVERY_ORDER", 
                "Received delivery order: " + deliveryId + " for Order: " + delivery.getOrder().getId()
            );
            
            log.info("Driver {} received delivery order {}", driverId, deliveryId);
        } catch (Exception e) {
            log.error("Failed to process driver receive delivery order: {}", e.getMessage());
            throw new HttpException("Failed to process driver receive delivery order", org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
    
    /**
     * Driver kiểm tra hàng hóa
     */
    public void driverCheckGoods(Long driverId, Long deliveryId, String checkNotes) {
        try {
            Delivery delivery = deliveryRepository.findById(deliveryId)
                .orElseThrow(() -> new HttpException("Delivery not found", org.springframework.http.HttpStatus.NOT_FOUND));
            
            checklistService.markStepCompleted(
                driverId, 
                delivery.getOrder().getId(),
                "DRIVER_CHECK_GOODS", 
                "Goods checked for delivery: " + deliveryId + (checkNotes != null ? " - Notes: " + checkNotes : "")
            );
            
            log.info("Driver {} checked goods for delivery {}", driverId, deliveryId);
        } catch (Exception e) {
            log.error("Failed to process driver check goods: {}", e.getMessage());
            throw new HttpException("Failed to process driver check goods", org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
    
    /**
     * Driver bắt đầu chuyến đi
     */
    public void driverStartTrip(Long driverId, Long deliveryId) {
        try {
            Delivery delivery = deliveryRepository.findById(deliveryId)
                .orElseThrow(() -> new HttpException("Delivery not found", org.springframework.http.HttpStatus.NOT_FOUND));
            
            checklistService.markStepCompleted(
                driverId, 
                delivery.getOrder().getId(),
                "DRIVER_START_TRIP", 
                "Started trip for delivery: " + deliveryId
            );
            
            log.info("Driver {} started trip for delivery {}", driverId, deliveryId);
        } catch (Exception e) {
            log.error("Failed to process driver start trip: {}", e.getMessage());
            throw new HttpException("Failed to process driver start trip", org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
    
    /**
     * Driver cập nhật status "On the way"
     */
    public void driverUpdateStatusOnRoute(Long driverId, Long deliveryId) {
        try {
            Delivery delivery = deliveryRepository.findById(deliveryId)
                .orElseThrow(() -> new RuntimeException("Delivery not found"));
                
            checklistService.markStepCompleted(
                driverId, 
                delivery.getOrder().getId(),
                "DRIVER_UPDATE_STATUS_ON_ROUTE", 
                "Updated status to 'On Route' for delivery: " + deliveryId
            );
            
            log.info("Driver {} updated status to 'On Route' for delivery {}", driverId, deliveryId);
        } catch (Exception e) {
            log.error("Failed to process driver status update: {}", e.getMessage());
            throw new HttpException("Failed to process driver status update", org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
    
    /**
     * Driver xác nhận kết quả giao hàng
     */
    public void driverConfirmDeliveryResult(Long driverId, Long deliveryId, boolean isSuccess, String resultNotes) {
        try {
            Delivery delivery = deliveryRepository.findById(deliveryId)
                .orElseThrow(() -> new RuntimeException("Delivery not found"));
                
            String result = isSuccess ? "Success" : "Failed";
            checklistService.markStepCompleted(
                driverId, 
                delivery.getOrder().getId(),
                "DRIVER_CONFIRM_DELIVERY_RESULT", 
                "Delivery result confirmed: " + result + " for delivery: " + deliveryId + 
                (resultNotes != null ? " - Notes: " + resultNotes : "")
            );
            
            log.info("Driver {} confirmed delivery result {} for delivery {}", driverId, result, deliveryId);
        } catch (Exception e) {
            log.error("Failed to process driver delivery result confirmation: {}", e.getMessage());
            throw new HttpException("Failed to process driver delivery result confirmation", org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
    
    /**
     * Driver hoàn thành chuyến đi
     * (Tạm thời KHÔNG cập nhật trạng thái delivered/checklist cho customer)
     */
    public void driverCompleteTrip(Long driverId, Long deliveryId) {
        try {
            Delivery delivery = deliveryRepository.findById(deliveryId)
                .orElseThrow(() -> new ktc.spring_project.exceptions.EntityNotFoundException("Delivery not found"));
            checklistService.markStepCompleted(
                driverId,
                delivery.getOrder().getId(),
                "DRIVER_COMPLETE_TRIP",
                "Completed trip for delivery: " + deliveryId
            );
            // KHÔNG cập nhật trạng thái delivered cho customer nữa
            log.info("Driver {} completed trip for delivery {}", driverId, deliveryId);
        } catch (Exception e) {
            log.error("Failed to process driver complete trip: {}", e.getMessage());
            throw new HttpException("Failed to process driver complete trip", org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Tính tổng doanh thu cho ngày cụ thể từ các delivery đã hoàn thành
     * @param date Ngày theo định dạng YYYY-MM-DD
     * @return Tổng doanh thu (VND)
     */
    public Long calculateRevenueByDate(String date) {
        try {
            return deliveryRepository.findCompletedDeliveriesByDate(date)
                .stream()
                .mapToLong(delivery -> delivery.getDeliveryFee() != null ? delivery.getDeliveryFee().longValue() : 0L)
                .sum();
        } catch (Exception e) {
            log.error("Error calculating revenue for date {}: {}", date, e.getMessage());
            return 0L;
        }
    }

    /**
     * Tính performance statistics tối ưu chỉ trả về kết quả cuối cùng
     */
    public Map<String, Object> calculatePerformanceStats() {
        try {
            // Tính ngày bắt đầu và kết thúc cho 2 tuần
            LocalDate today = LocalDate.now();
            LocalDate thisWeekStart = today.minusDays(7);
            LocalDate lastWeekStart = today.minusDays(14);
            LocalDate lastWeekEnd = today.minusDays(7);

            // Đếm số lượng thay vì lấy toàn bộ records
            long thisWeekTotal = deliveryRepository.countDeliveriesByDateRange(
                thisWeekStart.toString(), today.toString());
            long thisWeekCompleted = deliveryRepository.countCompletedDeliveriesByDateRange(
                thisWeekStart.toString(), today.toString());

            long lastWeekTotal = deliveryRepository.countDeliveriesByDateRange(
                lastWeekStart.toString(), lastWeekEnd.toString());
            long lastWeekCompleted = deliveryRepository.countCompletedDeliveriesByDateRange(
                lastWeekStart.toString(), lastWeekEnd.toString());

            // Tính completion rate
            int thisWeekPercentage = thisWeekTotal > 0 ? (int) Math.round((double) thisWeekCompleted / thisWeekTotal * 100) : 0;
            int lastWeekPercentage = lastWeekTotal > 0 ? (int) Math.round((double) lastWeekCompleted / lastWeekTotal * 100) : 0;

            // Tính phần trăm thay đổi
            double changePercent = 0.0;
            String trend = "stable";

            if (lastWeekPercentage > 0) {
                changePercent = ((double) (thisWeekPercentage - lastWeekPercentage) / lastWeekPercentage) * 100;
            } else if (thisWeekPercentage > 0) {
                changePercent = 100.0; // 100% increase
            }

            if (changePercent > 0) {
                trend = "increase";
            } else if (changePercent < 0) {
                trend = "decrease";
            }

            Map<String, Object> result = new HashMap<>();
            result.put("percentage", thisWeekPercentage);
            result.put("changePercent", Math.abs(changePercent));
            result.put("trend", trend);

            return result;

        } catch (Exception e) {
            log.error("Error calculating performance stats: {}", e.getMessage());
            Map<String, Object> fallback = new HashMap<>();
            fallback.put("percentage", 0);
            fallback.put("changePercent", 0.0);
            fallback.put("trend", "stable");
            return fallback;
        }
    }

    /**
     * Tính doanh thu theo tháng trong 12 tháng gần nhất
     */
    public Map<String, Object> calculateMonthlyRevenue() {
        try {
            Map<String, Object> result = new HashMap<>();
            
            // Lấy doanh thu 12 tháng gần nhất
            List<Map<String, Object>> monthlyRevenue = deliveryRepository.getMonthlyRevenueLast12Months();
            
            // Tính tổng doanh thu
            long totalRevenue = monthlyRevenue.stream()
                .mapToLong(m -> ((Number) m.getOrDefault("revenue", 0)).longValue())
                .sum();
            
            // Tính trung bình: chỉ lấy doanh thu của năm hiện tại (2025) để chia cho tháng hiện tại
            int currentMonth = java.time.LocalDate.now().getMonthValue();
            int currentYear = java.time.LocalDate.now().getYear();
            
            long currentYearRevenue = monthlyRevenue.stream()
                .filter(m -> ((Number) m.get("year")).intValue() == currentYear)
                .mapToLong(m -> ((Number) m.getOrDefault("revenue", 0)).longValue())
                .sum();
            
            long averageRevenue = currentMonth > 0 ? currentYearRevenue / currentMonth : 0;
            
            // Tính tỷ lệ tăng trưởng (so sánh tháng hiện tại với tháng trước trong năm hiện tại)
            double growthPercent = 0.0;
            
            // Tìm doanh thu tháng hiện tại và tháng trước trong năm hiện tại
            long currentMonthRevenue = monthlyRevenue.stream()
                .filter(m -> ((Number) m.get("year")).intValue() == currentYear && 
                           ((Number) m.get("month")).intValue() == currentMonth)
                .mapToLong(m -> ((Number) m.getOrDefault("revenue", 0)).longValue())
                .findFirst().orElse(0L);
                
            long previousMonthRevenue = 0L;
            if (currentMonth > 1) {
                // Tháng trước trong cùng năm
                previousMonthRevenue = monthlyRevenue.stream()
                    .filter(m -> ((Number) m.get("year")).intValue() == currentYear && 
                               ((Number) m.get("month")).intValue() == (currentMonth - 1))
                    .mapToLong(m -> ((Number) m.getOrDefault("revenue", 0)).longValue())
                    .findFirst().orElse(0L);
            } else {
                // Nếu là tháng 1, so sánh với tháng 12 năm trước
                previousMonthRevenue = monthlyRevenue.stream()
                    .filter(m -> ((Number) m.get("year")).intValue() == (currentYear - 1) && 
                               ((Number) m.get("month")).intValue() == 12)
                    .mapToLong(m -> ((Number) m.getOrDefault("revenue", 0)).longValue())
                    .findFirst().orElse(0L);
            }
            
            if (previousMonthRevenue > 0) {
                growthPercent = ((double) (currentMonthRevenue - previousMonthRevenue) / previousMonthRevenue) * 100;
            }
            
            result.put("monthlyRevenue", monthlyRevenue);
            result.put("totalRevenue", totalRevenue);
            result.put("averageRevenue", averageRevenue);
            result.put("growthPercent", growthPercent);
            
            return result;
            
        } catch (Exception e) {
            log.error("Error calculating monthly revenue: {}", e.getMessage());
            Map<String, Object> fallback = new HashMap<>();
            fallback.put("monthlyRevenue", new ArrayList<>());
            fallback.put("totalRevenue", 0L);
            fallback.put("averageRevenue", 0L);
            fallback.put("growthPercent", 0.0);
            return fallback;
        }
    }
}