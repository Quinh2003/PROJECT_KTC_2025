package ktc.spring_project.services;

import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.Address;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.repositories.OrderRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        // Có thể kiểm tra address nếu cần
        return orderRepository.save(order);
    }

    public Order getOrderById(Long id) {
        return orderRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Order not found with id: " + id));
    }

    public List<Order> getAllOrders() {
        return orderRepository.findAll(Sort.by("createdAt").descending());
    }

    public List<Order> getAllOrdersSorted() {
        return orderRepository.findAll(Sort.by("createdAt").descending());
    }

    public Page<Order> getOrdersPaginated(int page, int size) {
        Pageable pageable = PageRequest.of(page - 1, size, Sort.by("createdAt").descending());
        return orderRepository.findAll(pageable);
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
        order.setNotes(orderDetails.getNotes());
        order.setCreatedBy(orderDetails.getCreatedBy());
        order.setAddress(orderDetails.getAddress());
        // Chỉ cập nhật vehicle nếu có truyền vào (không ghi đè null nếu không truyền)
        if (orderDetails.getVehicle() != null) {
            if (orderDetails.getVehicle().getId() != null) {
                Vehicle vehicle = new Vehicle();
                vehicle.setId(orderDetails.getVehicle().getId());
                order.setVehicle(vehicle);
            } else {
                order.setVehicle(null);
            }
        }
        return orderRepository.save(order);
    }

    public void deleteOrder(Long id) {
        Order order = getOrderById(id);
        orderRepository.delete(order);
    }

    public Object getOrderTrackingInfo(Long orderId) {
        Order order = getOrderById(orderId); // Lấy order từ DB

        Map<String, Object> tracking = new HashMap<>();
        tracking.put("orderId", order.getId());
        // Lấy trạng thái từ entity Status liên kết
        tracking.put("status", order.getStatus() != null ? order.getStatus().getName() : null);
        // Lấy địa chỉ giao hàng nếu có
        tracking.put("address", order.getAddress() != null ? order.getAddress().getAddress() : null);
        // Lấy vị trí hiện tại (giả sử có trường currentLocation hoặc tracking entity)
        tracking.put("currentLocation", order.getNotes()); // hoặc order.getCurrentLocation() nếu có
        // Thời gian cập nhật gần nhất
        tracking.put("updatedAt", order.getUpdatedAt() != null ? order.getUpdatedAt() : LocalDateTime.now());

        return tracking;
    }
}