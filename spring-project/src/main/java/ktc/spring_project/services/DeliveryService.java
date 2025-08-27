package ktc.spring_project.services;

import ktc.spring_project.dtos.DeliveryFeeBreakdown;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.repositories.DeliveryRepository;
import jakarta.persistence.EntityNotFoundException;
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
}