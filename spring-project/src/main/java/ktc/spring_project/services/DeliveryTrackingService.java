

package ktc.spring_project.services;

import ktc.spring_project.entities.DeliveryTracking;
import ktc.spring_project.repositories.DeliveryTrackingRepository;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;

@Service
public class DeliveryTrackingService {
    @Autowired
    private DeliveryTrackingRepository deliveryTrackingRepository;

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
    
}
