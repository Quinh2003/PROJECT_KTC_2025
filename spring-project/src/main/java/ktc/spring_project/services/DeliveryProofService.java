package com.ktc.logistics.service;

import com.ktc.logistics.entity.DeliveryProof;
import com.ktc.logistics.repository.DeliveryProofRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
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

    public DeliveryProof save(DeliveryProof entity) {
        return deliveryProofRepository.save(entity);
    }

    public void delete(Long id) {
        deliveryProofRepository.deleteById(id);
    }
}
