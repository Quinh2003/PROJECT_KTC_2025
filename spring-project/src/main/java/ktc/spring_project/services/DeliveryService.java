package ktc.spring_project.services;

import ktc.spring_project.entities.Delivery;
import ktc.spring_project.repositories.DeliveryRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;

@Service
public class DeliveryService {

    @Autowired
    private DeliveryRepository deliveryRepository;

    public Delivery createDelivery(Delivery delivery) {
        if (delivery.getDeliveryAttempts() != null && delivery.getDeliveryAttempts() < 0) {
            throw new IllegalArgumentException("Delivery attempts cannot be negative");
        }
        if (delivery.getDeliveryFee() != null && delivery.getDeliveryFee().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Delivery fee cannot be negative");
        }
        return deliveryRepository.save(delivery);
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