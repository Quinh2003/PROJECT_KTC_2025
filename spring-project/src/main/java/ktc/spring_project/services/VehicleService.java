package com.ktc.logistics.service;

import com.ktc.logistics.entity.Vehicle;
import com.ktc.logistics.repository.VehicleRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class VehicleService {
    private final VehicleRepository vehicleRepository;

    public List<Vehicle> findAll() {
        return vehicleRepository.findAll();
    }

    public Optional<Vehicle> findById(Long id) {
        return vehicleRepository.findById(id);
    }

    public Vehicle save(Vehicle entity) {
        return vehicleRepository.save(entity);
    }

    public void delete(Long id) {
        vehicleRepository.deleteById(id);
    }
}
