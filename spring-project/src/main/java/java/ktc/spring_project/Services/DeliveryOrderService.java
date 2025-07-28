import ktc.spring_project.entities.DeliveryOrder;
import ktc.spring_project.repositories.DeliveryOrderRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

// Quản lý đơn hàng

@Service
public class DeliveryOrderService {

    @Autowired
    private DeliveryOrderRepository deliveryOrderRepository;

    public DeliveryOrder createOrder(DeliveryOrder order) {
        return deliveryOrderRepository.save(order);
    }

    public Optional<DeliveryOrder> getOrderById(Long id) {
        return deliveryOrderRepository.findById(id);
    }

    public List<DeliveryOrder> getAllOrders() {
        return deliveryOrderRepository.findAll();
    }

    public DeliveryOrder updateOrder(DeliveryOrder order) {
        return deliveryOrderRepository.save(order);
    }

    public void deleteOrder(Long id) {
        deliveryOrderRepository.deleteById(id);
    }
}