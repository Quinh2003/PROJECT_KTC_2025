package com.ktc.logistics.service;

import com.ktc.logistics.entity.Order;
import com.ktc.logistics.repository.OrderRepository;
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

    public Order save(Order entity) {
        return orderRepository.save(entity);
    }

    public void delete(Long id) {
        orderRepository.deleteById(id);
    }
}
