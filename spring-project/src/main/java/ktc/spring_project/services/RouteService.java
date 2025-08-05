package ktc.spring_project.services;

import ktc.spring_project.entities.Route;
import ktc.spring_project.repositories.RouteRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class RouteService {
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
}
