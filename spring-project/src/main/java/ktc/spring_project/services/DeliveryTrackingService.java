

package ktc.spring_project.services;

import ktc.spring_project.dtos.tracking.LocationUpdateDTO;
import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.DeliveryTracking;
import ktc.spring_project.entities.Status;
import ktc.spring_project.repositories.DeliveryRepository;
import ktc.spring_project.repositories.DeliveryTrackingRepository;
import ktc.spring_project.repositories.StatusRepository;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;

@Service
public class DeliveryTrackingService {
    @Autowired
    private DeliveryTrackingRepository deliveryTrackingRepository;
    
    @Autowired
    private DeliveryRepository deliveryRepository;
    
    @Autowired
    private StatusRepository statusRepository;

    public Optional<DeliveryTracking> findLatestByVehicleId(Long vehicleId) {
        return deliveryTrackingRepository.findLatestByVehicleId(vehicleId);
    }

    public List<DeliveryTracking> findAll() {
        return deliveryTrackingRepository.findAll();
    }

    public Optional<DeliveryTracking> findById(Long id) {
        return deliveryTrackingRepository.findById(id);
    }

    public DeliveryTracking save(DeliveryTracking entities) {
        return deliveryTrackingRepository.save(entities);
    }

    public void delete(Long id) {
        deliveryTrackingRepository.deleteById(id);
    }
    
    public List<DeliveryTracking> findByVehicleIdAndDeliveryId(Long vehicleId, Long deliveryId) {
        return deliveryTrackingRepository.findByVehicleIdAndDeliveryId(vehicleId, deliveryId);
    }
    
    public List<DeliveryTracking> findByDeliveryId(Long deliveryId) {
        return deliveryTrackingRepository.findByDeliveryId(deliveryId);
    }
    
    /**
     * Update the location and optionally the status of a delivery
     */
    public void updateDeliveryLocation(Long driverId, Long deliveryId, LocationUpdateDTO locationUpdate) {
        // Find the delivery
        Delivery delivery = deliveryRepository.findById(deliveryId)
                .orElseThrow(() -> new RuntimeException("Delivery not found with id: " + deliveryId));
        
        // Validate that the delivery belongs to the driver
        if (!driverId.equals(delivery.getDriver().getId())) {
            throw new RuntimeException("Delivery does not belong to driver with id: " + driverId);
        }
        
        // Create a new tracking record
        DeliveryTracking tracking = new DeliveryTracking();
        tracking.setDelivery(delivery);
        tracking.setLatitude(locationUpdate.getLatitude());
        tracking.setLongitude(locationUpdate.getLongitude());
        tracking.setLocation(locationUpdate.getLocation());
        tracking.setNotes(locationUpdate.getNotes());
        tracking.setTimestamp(Timestamp.from(Instant.now()));
        
        // Add vehicle if available
        if (delivery.getVehicle() != null) {
            tracking.setVehicle(delivery.getVehicle());
        }
        
        // Update status if provided
        if (locationUpdate.getStatusId() != null) {
            Status status = statusRepository.findById(locationUpdate.getStatusId())
                    .orElseThrow(() -> new RuntimeException("Status not found with id: " + locationUpdate.getStatusId()));
            
            tracking.setStatus(status);
            
            // Also update the delivery status
            delivery.setStatus(status);
            deliveryRepository.save(delivery);
        }
        
        // Save the tracking record
        deliveryTrackingRepository.save(tracking);
    }
}
