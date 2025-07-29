package ktc.spring_project.services;

import com.ktc.springbootproject.entities.User;
import com.ktc.springbootproject.entities.DeliveryOrder;
import com.ktc.springbootproject.repositories.UserRepository;
import com.ktc.springbootproject.repositories.DeliveryOrderRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;


// Đơn hàng tài xế nhận

@Service
public class DriverService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DeliveryOrderRepository orderRepository;

    // Lấy tất cả tài xế trong hệ thống
    public List<User> getAllDrivers() {
        return userRepository.findByRole("DRIVER");
    }

    // Lấy danh sách đơn hàng theo ID tài xế
    public List<DeliveryOrder> getOrdersByDriver(Long driverId) {
        return orderRepository.findByDriverId(driverId);
    }

    // Cập nhật trạng thái đơn hàng mà tài xế được phân
    public DeliveryOrder updateOrderStatus(Long orderId, String status) {
        Optional<DeliveryOrder> optionalOrder = orderRepository.findById(orderId);
        if (optionalOrder.isPresent()) {
            DeliveryOrder order = optionalOrder.get();
            order.setStatus(status);
            return orderRepository.save(order);
        } else {
            throw new RuntimeException("Order not found");
        }
    }
}