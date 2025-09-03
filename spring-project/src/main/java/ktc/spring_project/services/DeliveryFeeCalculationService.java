package ktc.spring_project.services;
import ktc.spring_project.exceptions.HttpException;

import ktc.spring_project.dtos.DeliveryFeeBreakdown;
import ktc.spring_project.entities.*;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.repositories.OrderItemRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

@Service
@Slf4j
public class DeliveryFeeCalculationService {

    @Autowired
    private OrderItemRepository orderItemRepository;
    
    @Autowired
    private DistanceCalculationService distanceCalculationService;

    // Phí cơ bản theo vùng cho DELIVERY
    private static final BigDecimal INNER_CITY_BASE_FEE = new BigDecimal("15000"); // 15,000 VNĐ
    private static final BigDecimal SUBURBAN_BASE_FEE = new BigDecimal("25000"); // 25,000 VNĐ  
    private static final BigDecimal INTER_PROVINCIAL_BASE_FEE = new BigDecimal("40000"); // 40,000 VNĐ

    // Đơn giá theo km cho DELIVERY
    private static final BigDecimal INNER_CITY_RATE_PER_KM = new BigDecimal("1800"); // 1,800 VNĐ/km
    private static final BigDecimal SUBURBAN_RATE_PER_KM = new BigDecimal("1500"); // 1,500 VNĐ/km
    private static final BigDecimal INTER_PROVINCIAL_RATE_PER_KM = new BigDecimal("500"); // 500 VNĐ/km

    /**
     * Tính delivery fee cho một Order
     * CÔNG THỨC: DELIVERY_FEE = (TỔNG SHIPPING_FEE + PHÍ KHOẢNG CÁCH) × HỆ SỐ SERVICE_TYPE
     * 
     * @param order Order cần tính delivery fee
     * @param serviceType Loại dịch vụ delivery
     * @return DeliveryFeeBreakdown chứa chi tiết tính phí
     */
    public DeliveryFeeBreakdown calculateDeliveryFee(Order order, ServiceType serviceType) {
        log.info("Calculating delivery fee for Order ID: {} with ServiceType: {}", order.getId(), serviceType);
        
        // 1. Tính tổng shipping fee của tất cả OrderItems
        BigDecimal totalShippingFee = calculateTotalShippingFeeOfOrderItems(order);
        
        // 2. Tính phí khoảng cách từ Store đến địa chỉ giao hàng
        BigDecimal distanceFee = calculateDistanceFee(order);
        BigDecimal distance = calculateDistance(order);
        
        // 3. Tính phí cơ bản delivery = tổng shipping fee + phí khoảng cách
        BigDecimal baseDeliveryFee = totalShippingFee.add(distanceFee);
        
        // 4. Áp dụng hệ số ServiceType
        BigDecimal serviceMultiplier = getServiceTypeMultiplier(serviceType);
        BigDecimal totalDeliveryFee = baseDeliveryFee.multiply(serviceMultiplier);
        
        // 5. Tạo chi tiết tính toán
        String calculationDetails = buildDeliveryCalculationDetails(
            order, totalShippingFee, distanceFee, distance, serviceType, 
            serviceMultiplier, baseDeliveryFee, totalDeliveryFee
        );
        
        return DeliveryFeeBreakdown.builder()
                .orderId(order.getId())
                .totalShippingFee(totalShippingFee)
                .distanceFee(distanceFee)
                .distance(distance)
                .serviceType(serviceType)
                .serviceMultiplier(serviceMultiplier)
                .baseDeliveryFee(baseDeliveryFee)
                .totalDeliveryFee(totalDeliveryFee)
                .calculationDetails(calculationDetails)
                .build();
    }

    /**
     * Tính tổng shipping fee của tất cả OrderItems trong Order
     */
    private BigDecimal calculateTotalShippingFeeOfOrderItems(Order order) {
        List<OrderItem> orderItems = orderItemRepository.findByOrderId(order.getId());
        
        BigDecimal totalShippingFee = BigDecimal.ZERO;
        int itemCount = 0;
        
        for (OrderItem item : orderItems) {
            if (item.getShippingFee() != null) {
                totalShippingFee = totalShippingFee.add(item.getShippingFee());
                itemCount++;
            }
        }
        
        log.info("Order {} has {} items with total shipping fee: {}", 
                order.getId(), itemCount, totalShippingFee);
        
        return totalShippingFee;
    }

    /**
     * Tính khoảng cách từ Store đến địa chỉ giao hàng
     */
    private BigDecimal calculateDistance(Order order) {
        Store store = order.getStore();
        Address deliveryAddress = order.getAddress();
        
        if (store.getLatitude() == null || store.getLongitude() == null) {
            throw new HttpException("Store không có tọa độ: " + store.getStoreName(), org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        
        if (deliveryAddress.getLatitude() == null || deliveryAddress.getLongitude() == null) {
            throw new HttpException("Địa chỉ giao hàng không có tọa độ: " + deliveryAddress.getAddress(), org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        
        return distanceCalculationService.calculateDistance(
            store.getLatitude(), 
            store.getLongitude(),
            deliveryAddress.getLatitude(), 
            deliveryAddress.getLongitude()
        );
    }

    /**
     * Tính phí khoảng cách cho delivery
     */
    private BigDecimal calculateDistanceFee(Order order) {
        BigDecimal distance = calculateDistance(order);
        
        BigDecimal baseFee;
        BigDecimal ratePerKm;

        if (distance.compareTo(new BigDecimal("15")) <= 0) {
            // Nội thành: 0-15km
            baseFee = INNER_CITY_BASE_FEE;
            ratePerKm = INNER_CITY_RATE_PER_KM;
        } else if (distance.compareTo(new BigDecimal("50")) <= 0) {
            // Ngoại thành: 15-50km
            baseFee = SUBURBAN_BASE_FEE;
            ratePerKm = SUBURBAN_RATE_PER_KM;
        } else {
            // Liên tỉnh: >50km
            baseFee = INTER_PROVINCIAL_BASE_FEE;
            ratePerKm = INTER_PROVINCIAL_RATE_PER_KM;
        }

        // Công thức: Phí cơ bản + (Khoảng cách × Đơn giá/km)
        return baseFee.add(distance.multiply(ratePerKm));
    }

    /**
     * Lấy hệ số service type
     */
    private BigDecimal getServiceTypeMultiplier(ServiceType serviceType) {
        return BigDecimal.valueOf(serviceType.getMultiplier());
    }

    /**
     * Tạo chi tiết tính toán delivery fee
     */
    private String buildDeliveryCalculationDetails(Order order, BigDecimal totalShippingFee, 
                                                 BigDecimal distanceFee, BigDecimal distance,
                                                 ServiceType serviceType, BigDecimal serviceMultiplier,
                                                 BigDecimal baseDeliveryFee, BigDecimal totalDeliveryFee) {
        StringBuilder details = new StringBuilder();
        
        details.append("CHI TIẾT TÍNH DELIVERY FEE:\n");
        details.append("=======================================\n");
        
        // Thông tin cơ bản
        details.append("📦 THÔNG TIN ORDER:\n");
        details.append(String.format("- Order ID: %d\n", order.getId()));
        details.append(String.format("- Store: %s\n", order.getStore().getStoreName()));
        details.append(String.format("- Địa chỉ giao hàng: %s\n", order.getAddress().getAddress()));
        details.append(String.format("- Khoảng cách: %.2f km\n", distance));
        details.append(String.format("- Service Type: %s\n\n", serviceType.getDisplayName()));
        
        // Tính phí
        details.append("💰 TÍNH DELIVERY FEE:\n");
        details.append(String.format("- Tổng shipping fee OrderItems: %,.0f VNĐ\n", totalShippingFee));
        
        // Chi tiết phí khoảng cách
        String zoneType;
        if (distance.compareTo(new BigDecimal("15")) <= 0) {
            zoneType = "Nội thành";
        } else if (distance.compareTo(new BigDecimal("50")) <= 0) {
            zoneType = "Ngoại thành";
        } else {
            zoneType = "Liên tỉnh";
        }
        
        details.append(String.format("- Phí khoảng cách (%s): %,.0f VNĐ\n", zoneType, distanceFee));
        details.append(String.format("- Phí cơ bản delivery: %,.0f + %,.0f = %,.0f VNĐ\n", 
                totalShippingFee, distanceFee, baseDeliveryFee));
        details.append(String.format("- Hệ số %s: %.1f\n", serviceType.getDisplayName(), serviceMultiplier));
        details.append(String.format("- TỔNG DELIVERY FEE: %,.0f × %.1f = %,.0f VNĐ", 
                baseDeliveryFee, serviceMultiplier, totalDeliveryFee));
        
        return details.toString();
    }
}
