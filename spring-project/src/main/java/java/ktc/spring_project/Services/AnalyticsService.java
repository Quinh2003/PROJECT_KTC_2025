import ktc.spring_project.entities.DeliveryOrder;
import ktc.spring_project.repositories.DeliveryOrderRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.stream.Collectors;

//	Thống kê theo tuyến, tài xế
@Service
public class AnalyticsService {

    @Autowired
    private DeliveryOrderRepository orderRepository;

    public Map<String, Long> getOrderCountByRoute() {
        return orderRepository.findAll().stream()
                .collect(Collectors.groupingBy(DeliveryOrder::getRouteName, Collectors.counting()));
    }

    public Map<Long, Long> getOrderCountByDriver() {
        return orderRepository.findAll().stream()
                .collect(Collectors.groupingBy(DeliveryOrder::getDriverId, Collectors.counting()));
    }
}