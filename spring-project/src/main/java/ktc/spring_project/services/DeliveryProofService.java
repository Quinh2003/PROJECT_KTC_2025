package ktc.spring_project.services;

import ktc.spring_project.entities.DeliveryProof;
import ktc.spring_project.repositories.DeliveryProofRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class DeliveryProofService {
    private DeliveryProofRepository deliveryProofRepository;

    public List<DeliveryProof> findAll() {
        return deliveryProofRepository.findAll();
    }

    public Optional<DeliveryProof> findById(Long id) {
        return deliveryProofRepository.findById(id);
    }

    public DeliveryProof save(DeliveryProof entities) {
        return deliveryProofRepository.save(entities);
    }

    public void delete(Long id) {
        deliveryProofRepository.deleteById(id);
    }
}
