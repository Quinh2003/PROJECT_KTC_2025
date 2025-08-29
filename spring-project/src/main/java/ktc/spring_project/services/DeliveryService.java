package ktc.spring_project.services;

import ktc.spring_project.dtos.DeliveryFeeBreakdown;
import ktc.spring_project.dtos.order.DeliveryOrderResponseDTO;
import ktc.spring_project.dtos.order.OrderDetailResponseDTO;
import ktc.spring_project.dtos.orderitem.OrderItemResponseDTO;
import ktc.spring_project.dtos.delivery.DeliveryResponseDTO;
import ktc.spring_project.dtos.address.AddressResponseDTO;
import ktc.spring_project.dtos.store.StoreResponseDTO;
import ktc.spring_project.dtos.order.DriverOrderSimpleResponseDTO;
import ktc.spring_project.dtos.user.UserResponseDTO;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.Payment;


import ktc.spring_project.dtos.DeliveryFeeBreakdown;
import ktc.spring_project.dtos.order.DeliveryOrderResponseDTO;
import ktc.spring_project.dtos.order.DriverOrderSimpleResponseDTO;
import ktc.spring_project.dtos.user.UserResponseDTO;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.Payment;
import ktc.spring_project.repositories.DeliveryRepository;
import ktc.spring_project.repositories.PaymentRepository;
import ktc.spring_project.repositories.OrderItemRepository;
import jakarta.persistence.EntityNotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;
import java.math.BigDecimal;
import java.util.List;

import ktc.spring_project.entities.Order;
import java.util.stream.Collectors;

@Service
@Slf4j
public class DeliveryService {

    @Autowired
    private PaymentRepository paymentRepository;

    @Autowired
    private DeliveryRepository deliveryRepository;

    @Autowired
    private DeliveryFeeCalculationService deliveryFeeCalculationService;

    @Autowired
    private OrderItemRepository orderItemRepository;

    public Delivery createDelivery(Delivery delivery) {
        if (delivery.getDeliveryAttempts() != null && delivery.getDeliveryAttempts() < 0) {
            throw new IllegalArgumentException("Delivery attempts cannot be negative");
        }
        if (delivery.getDeliveryFee() != null && delivery.getDeliveryFee().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Delivery fee cannot be negative");
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
                throw new IllegalArgumentException("Delivery attempts cannot be negative");
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
            throw new IllegalArgumentException("Delivery attempts cannot be negative");
        }
        if (delivery.getDeliveryFee() != null && delivery.getDeliveryFee().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Delivery fee cannot be negative");
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

}