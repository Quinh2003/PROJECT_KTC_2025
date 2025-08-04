package ktc.spring_project.services;

import ktc.spring_project.entities.DeliveryTracking;
import ktc.spring_project.repository.DeliveryTrackingRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class DeliveryTrackingService {
    private final DeliveryTrackingRepository deliveryTrackingRepository;

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
