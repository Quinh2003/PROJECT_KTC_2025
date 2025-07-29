package ktc.spring_project.services;

import ktc.spring_project.entities.OrderTracking;
import ktc.spring_project.repositories.OrderTrackingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

// 	Theo dõi vị trí đơn hàng

@Service
public class TrackingService {

    @Autowired
    private OrderTrackingRepository trackingRepository;

    public OrderTracking saveTracking(OrderTracking tracking) {
        return trackingRepository.save(tracking);
    }

    public List<OrderTracking> getTrackingByOrderId(Long orderId) {
        return trackingRepository.findByOrderIdOrderByTimestampAsc(orderId);
    }
}