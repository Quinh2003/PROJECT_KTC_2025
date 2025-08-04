package ktc.spring_project.services;

import ktc.spring_project.entities.Order;
import ktc.spring_project.repository.OrderRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class OrderService {
    private final OrderRepository orderRepository;

    public List<Order> findAll() {
        return orderRepository.findAll();
    }

    public Optional<Order> findById(Long id) {
        return orderRepository.findById(id);
    }

    public Order save(Order entities) {
        return orderRepository.save(entities);
    }

    public void delete(Long id) {
        orderRepository.deleteById(id);
    }
}
