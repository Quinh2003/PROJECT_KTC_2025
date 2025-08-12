package ktc.spring_project.services;

import ktc.spring_project.entities.OrderItem;
import ktc.spring_project.repositories.OrderItemRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class OrderItemService {
    @Autowired
    private OrderItemRepository orderItemRepository;

    public List<OrderItem> findAll() {
        return orderItemRepository.findAll();
    }

    public Optional<OrderItem> findById(Long id) {
        return orderItemRepository.findById(id);
    }

    public OrderItem save(OrderItem entities) {
        return orderItemRepository.save(entities);
    }

    public void delete(Long id) {
        orderItemRepository.deleteById(id);
    }
}