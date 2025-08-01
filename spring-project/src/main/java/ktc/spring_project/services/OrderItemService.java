package com.ktc.logistics.service;

import com.ktc.logistics.entity.OrderItem;
import com.ktc.logistics.repository.OrderItemRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class OrderItemService {
    private final OrderItemRepository orderItemRepository;

    public List<OrderItem> findAll() {
        return orderItemRepository.findAll();
    }

    public Optional<OrderItem> findById(Long id) {
        return orderItemRepository.findById(id);
    }

    public OrderItem save(OrderItem entity) {
        return orderItemRepository.save(entity);
    }

    public void delete(Long id) {
        orderItemRepository.deleteById(id);
    }
}
