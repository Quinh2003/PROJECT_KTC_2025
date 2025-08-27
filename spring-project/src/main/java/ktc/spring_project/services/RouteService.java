package ktc.spring_project.services;

import ktc.spring_project.entities.Route;
import ktc.spring_project.repositories.RouteRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class RouteService {

    @Autowired
    private RouteRepository routeRepository;

    public List<Route> findAll() {
        return routeRepository.findAll();
    }

    public Optional<Route> findById(Long id) {
        return routeRepository.findById(id);
    }

    public Route save(Route entities) {
        return routeRepository.save(entities);
    }

    public void delete(Long id) {
        routeRepository.deleteById(id);
    }

    // ...existing code...

public Route getRouteById(Long routeId) {
    // Replace with your actual data access logic
    return routeRepository.findById(routeId)
        .orElseThrow(() -> new RuntimeException("Route not found with id: " + routeId));
}

// ...existing code...
}
