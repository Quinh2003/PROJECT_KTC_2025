package ktc.spring_project.services;

import ktc.spring_project.entities.DeliveryProof;
import ktc.spring_project.repositories.DeliveryProofRepository;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.security.core.Authentication;
import org.springframework.web.multipart.MultipartFile;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class DeliveryProofService {
    private final DeliveryProofRepository deliveryProofRepository;

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
public DeliveryProof createProof(
        Long orderId,
        String proofTypeStr,
        MultipartFile file,
        String recipientName,
        String recipientSignature,
        String notes,
        Authentication authentication) {
    // TODO: Thực hiện logic tạo mới DeliveryProof ở đây
    throw new UnsupportedOperationException("Not implemented yet");
}

public DeliveryProof updateProof(
        Long id,
        Map<String, Object> updateData,
        Authentication authentication) {
    // TODO: Implement the update logic
    throw new UnsupportedOperationException("Not implemented yet");
        }
    public void deleteById(Long id, Authentication authentication) {
    // TODO: Implement the logic to delete a DeliveryProof by id, possibly checking authentication
    throw new UnsupportedOperationException("Not implemented yet");
}
}
