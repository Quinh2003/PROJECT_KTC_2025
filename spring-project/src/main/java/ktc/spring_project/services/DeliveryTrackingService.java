package com.ktc.logistics.service;

import com.ktc.logistics.entity.DeliveryTracking;
import com.ktc.logistics.repository.DeliveryTrackingRepository;
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

    public DeliveryTracking save(DeliveryTracking entity) {
        return deliveryTrackingRepository.save(entity);
    }

    public void delete(Long id) {
        deliveryTrackingRepository.deleteById(id);
    }
}
