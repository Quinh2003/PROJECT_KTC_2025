package com.ktc.logistics.service;

import com.ktc.logistics.entity.Route;
import com.ktc.logistics.repository.RouteRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class RouteService {
    private final RouteRepository routeRepository;

    public List<Route> findAll() {
        return routeRepository.findAll();
    }

    public Optional<Route> findById(Long id) {
        return routeRepository.findById(id);
    }

    public Route save(Route entity) {
        return routeRepository.save(entity);
    }

    public void delete(Long id) {
        routeRepository.deleteById(id);
    }
}
