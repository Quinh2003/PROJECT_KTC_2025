package ktc.spring_project.services;

import ktc.spring_project.repositories.DeliveryOrderRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

// 	Tính toán tỷ lệ giao đúng hạn

@Service
public class KPIService {

    @Autowired
    private DeliveryOrderRepository orderRepository;

    public long countDeliveredOrders() {
        return orderRepository.countByStatus("Delivered");
    }

    public long countLateDeliveries() {
        return orderRepository.countByStatus("Late");
    }

    public double calculateOnTimeRate() {
        long total = orderRepository.count();
        long onTime = orderRepository.countByStatus("Delivered");
        return total == 0 ? 0 : ((double) onTime / total) * 100;
    }
}