package ktc.spring_project.services;

import ktc.spring_project.entities.Order;
import ktc.spring_project.repositories.OrderRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;

@Service
public class OrderService {

    @Autowired
    private OrderRepository orderRepository;

    public Order createOrder(Order order) {
        // Kiểm tra ràng buộc CHECK
        if (order.getTotalAmount() != null && order.getTotalAmount().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Total amount cannot be negative");
        }
        if (order.getBenefitPerOrder() != null && order.getBenefitPerOrder().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Benefit per order cannot be negative");
        }
        if (order.getOrderProfitPerOrder() != null && order.getOrderProfitPerOrder().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Order profit per order cannot be negative");
        }
        return orderRepository.save(order);
    }

    public Order getOrderById(Long id) {
        return orderRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Order not found with id: " + id));
    }

    public List<Order> getAllOrders() {
        return orderRepository.findAll();
    }

    public Order updateOrder(Long id, Order orderDetails) {
        Order order = getOrderById(id);
        // Kiểm tra ràng buộc CHECK
        if (orderDetails.getTotalAmount() != null && orderDetails.getTotalAmount().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Total amount cannot be negative");
        }
        if (orderDetails.getBenefitPerOrder() != null && orderDetails.getBenefitPerOrder().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Benefit per order cannot be negative");
        }
        if (orderDetails.getOrderProfitPerOrder() != null && orderDetails.getOrderProfitPerOrder().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Order profit per order cannot be negative");
        }
        order.setStatus(orderDetails.getStatus());
        order.setStore(orderDetails.getStore());
        order.setDescription(orderDetails.getDescription());
        order.setTotalAmount(orderDetails.getTotalAmount());
        order.setBenefitPerOrder(orderDetails.getBenefitPerOrder());
        order.setOrderProfitPerOrder(orderDetails.getOrderProfitPerOrder());
        order.setDescription(orderDetails.getDescription());
        order.setCreatedBy(orderDetails.getCreatedBy());
        return orderRepository.save(order);
    }

    public void deleteOrder(Long id) {
        Order order = getOrderById(id);
        orderRepository.delete(order);
    }
}