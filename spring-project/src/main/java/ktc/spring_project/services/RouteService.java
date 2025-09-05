package ktc.spring_project.services;

import ktc.spring_project.entities.Delivery;
import ktc.spring_project.entities.Route;
import ktc.spring_project.dtos.route.RouteResponseDTO;
import ktc.spring_project.repositories.DeliveryRepository;
import ktc.spring_project.repositories.RouteRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class RouteService {

    @Autowired
    private RouteRepository routeRepository;
    
    @Autowired
    private DeliveryRepository deliveryRepository;

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

/**
 * Get route information for a specific delivery
 */
public RouteResponseDTO getRouteForDelivery(Long deliveryId) {
    Delivery delivery = deliveryRepository.findById(deliveryId)
            .orElseThrow(() -> new RuntimeException("Delivery not found with id: " + deliveryId));
    
    if (delivery.getRoute() == null) {
        throw new RuntimeException("No route found for delivery with id: " + deliveryId);
    }
    
    return mapToRouteResponseDTO(delivery.getRoute());
}

/**
 * Map Route entity to RouteResponseDTO
 */
private RouteResponseDTO mapToRouteResponseDTO(Route route) {
    RouteResponseDTO dto = new RouteResponseDTO();
    dto.setId(route.getId());
    dto.setName(route.getName());
    dto.setEstimatedDistance(route.getEstimatedDistance());
    dto.setEstimatedDuration(route.getEstimatedDuration());
    dto.setPolyline(route.getPolyline());
    dto.setStartLatitude(route.getStartLatitude());
    dto.setStartLongitude(route.getStartLongitude());
    dto.setEndLatitude(route.getEndLatitude());
    dto.setEndLongitude(route.getEndLongitude());
    
    return dto;
}

// ...existing code...
}
