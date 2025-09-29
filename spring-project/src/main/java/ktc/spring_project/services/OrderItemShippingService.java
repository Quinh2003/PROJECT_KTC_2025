package ktc.spring_project.services;
import jakarta.persistence.EntityNotFoundException;
import ktc.spring_project.exceptions.HttpException;

import ktc.spring_project.dtos.DistanceCalculationRequest;
import ktc.spring_project.dtos.ShippingCalculationRequest;
import ktc.spring_project.dtos.ShippingFeeBreakdown;
import ktc.spring_project.entities.*;
import ktc.spring_project.enums.ServiceType;
import ktc.spring_project.repositories.OrderItemRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;

@Service
@Slf4j
@Transactional
public class OrderItemShippingService {

    @Autowired
    private DistanceCalculationService distanceCalculationService;

    @Autowired
    private ShippingCalculationService shippingCalculationService;

    @Autowired
    private OrderItemRepository orderItemRepository;

    /**
     * Tính và cập nhật shipping fee cho một OrderItem
     * Dựa trên tọa độ từ Store đến Address của Order, cân nặng và thể tích Product
     * 
     * @param orderItem OrderItem cần tính phí
     * @param serviceType Loại dịch vụ giao hàng  
     * @return ShippingFeeBreakdown chứa chi tiết tính phí
     */
    public ShippingFeeBreakdown calculateAndUpdateShippingFee(OrderItem orderItem, ServiceType serviceType) {
        // 1. Lấy thông tin cần thiết
        Order order = orderItem.getOrder();
        Product product = orderItem.getProduct();
        Store store = order.getStore();
        Address deliveryAddress = order.getAddress();

        // 2. Validation
        validateOrderItemForShipping(orderItem, store, deliveryAddress, product);

        // 3. Tính khoảng cách từ Store đến địa chỉ giao hàng
        BigDecimal distance = calculateDistanceBetweenStoreAndDelivery(store, deliveryAddress);

        // 4. Tạo request để tính shipping fee
        ShippingCalculationRequest shippingRequest = buildShippingRequest(
            orderItem, product, distance, serviceType
        );

        // 5. Tính shipping fee
        ShippingFeeBreakdown breakdown = shippingCalculationService.calculateShippingFee(shippingRequest);

        // 6. Cập nhật shipping fee vào OrderItem
        orderItem.setShippingFee(breakdown.getTotalFee());
        orderItemRepository.save(orderItem);

        return breakdown;
    }

    /**
     * Tính shipping fee mà không cập nhật vào database
     */
    public ShippingFeeBreakdown calculateShippingFeeOnly(OrderItem orderItem, ServiceType serviceType) {
        Order order = orderItem.getOrder();
        Product product = orderItem.getProduct();
        Store store = order.getStore();
        Address deliveryAddress = order.getAddress();

        validateOrderItemForShipping(orderItem, store, deliveryAddress, product);

        BigDecimal distance = calculateDistanceBetweenStoreAndDelivery(store, deliveryAddress);

        ShippingCalculationRequest shippingRequest = buildShippingRequest(
            orderItem, product, distance, serviceType
        );

        return shippingCalculationService.calculateShippingFee(shippingRequest);
    }

    /**
     * Tính khoảng cách giữa Store và địa chỉ giao hàng
     * Store có latitude/longitude trực tiếp, không thông qua Address entity
     */
    private BigDecimal calculateDistanceBetweenStoreAndDelivery(Store store, Address deliveryAddress) {
        if (store.getLatitude() == null || store.getLongitude() == null) {
            throw new HttpException("Không tìm thấy tọa độ của Store", org.springframework.http.HttpStatus.BAD_REQUEST);
        }

        DistanceCalculationRequest distanceRequest = new DistanceCalculationRequest(
            store.getLatitude(),
            store.getLongitude(),
            deliveryAddress.getLatitude(),
            deliveryAddress.getLongitude()
        );

        return distanceCalculationService.calculateDistance(distanceRequest);
    }

    /**
     * Tạo ShippingCalculationRequest từ thông tin OrderItem và Product
     */
    private ShippingCalculationRequest buildShippingRequest(OrderItem orderItem, Product product, 
                                                          BigDecimal distance, ServiceType serviceType) {
        // Chuyển đổi volume từ m³ sang cm³ (KHÔNG nhân với quantity, chỉ tính cho 1 sản phẩm)
        BigDecimal volumeInCm3 = BigDecimal.ZERO;
        if (product.getVolume() != null && product.getVolume().compareTo(BigDecimal.ZERO) > 0) {
            // Chuyển từ m³ sang cm³: nhân với 1,000,000
            volumeInCm3 = product.getVolume().multiply(new BigDecimal("1000000"));
        }

        ShippingCalculationRequest request = new ShippingCalculationRequest();
        request.setWeight(product.getWeight() != null ? product.getWeight() : BigDecimal.ZERO);
        request.setActualWeight(product.getWeight() != null ? 
            product.getWeight().multiply(new BigDecimal(orderItem.getQuantity())) : BigDecimal.ZERO);
        request.setVolume(volumeInCm3);
        request.setDistance(distance);
        request.setFragile(product.getIsFragile() != null ? product.getIsFragile() : false);
        request.setServiceType(serviceType);
        request.setQuantity(orderItem.getQuantity());

        return request;
    }

    /**
     * Validation các thông tin cần thiết
     */
    private void validateOrderItemForShipping(OrderItem orderItem, Store store, 
                                            Address deliveryAddress, Product product) {
        if (orderItem == null) {
            throw new HttpException("OrderItem không được null", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        
        if (store == null) {
            throw new HttpException("Không tìm thấy Store cho Order này", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        
        if (deliveryAddress == null) {
            throw new HttpException("Không tìm thấy địa chỉ giao hàng cho Order này", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        
        if (product == null) {
            throw new HttpException("Không tìm thấy Product cho OrderItem này", org.springframework.http.HttpStatus.BAD_REQUEST);
        }

        // Kiểm tra tọa độ của Store
        if (store.getLatitude() == null || store.getLongitude() == null) {
            throw new HttpException("Store thiếu thông tin tọa độ (latitude/longitude)", org.springframework.http.HttpStatus.BAD_REQUEST);
        }

        // Kiểm tra tọa độ của địa chỉ giao hàng
        if (deliveryAddress.getLatitude() == null || deliveryAddress.getLongitude() == null) {
            throw new HttpException("Địa chỉ giao hàng thiếu thông tin tọa độ (latitude/longitude)", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
    }

    /**
     * Tính shipping fee cho danh sách OrderItem
     */
    public void calculateShippingFeeForOrderItems(List<OrderItem> orderItems, ServiceType serviceType) {
        if (orderItems == null || orderItems.isEmpty()) {
            return;
        }

        for (OrderItem orderItem : orderItems) {
            try {
                calculateAndUpdateShippingFee(orderItem, serviceType);
            } catch (Exception e) {
                // Log lỗi nhưng tiếp tục xử lý các item khác
                System.err.println("Lỗi khi tính shipping fee cho OrderItem ID: " + 
                    orderItem.getId() + " - " + e.getMessage());
            }
        }
    }

    /**
     * Tính tổng shipping fee cho danh sách OrderItem
     */
    public BigDecimal calculateTotalShippingFeeForOrderItems(List<OrderItem> orderItems) {
        if (orderItems == null || orderItems.isEmpty()) {
            return BigDecimal.ZERO;
        }

        return orderItems.stream()
            .map(OrderItem::getShippingFee)
            .filter(fee -> fee != null)
            .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    /**
     * Tính shipping fee đơn giản với các tham số cơ bản
     */
    public BigDecimal calculateSimpleShippingFee(BigDecimal weight, BigDecimal volume, 
                                                BigDecimal fromLatitude, BigDecimal fromLongitude,
                                                BigDecimal toLatitude, BigDecimal toLongitude,
                                                boolean isFragile, ServiceType serviceType, 
                                                Integer quantity) {
        // Tính khoảng cách
        BigDecimal distance = distanceCalculationService.calculateDistance(
            fromLatitude, fromLongitude, toLatitude, toLongitude
        );

        // Tạo request
        ShippingCalculationRequest request = new ShippingCalculationRequest();
        request.setActualWeight(weight != null ? weight.multiply(new BigDecimal(quantity)) : BigDecimal.ZERO);
        request.setVolume(volume != null ? volume : BigDecimal.ZERO);
        request.setDistance(distance);
        request.setFragile(isFragile);
        request.setServiceType(serviceType);
        request.setQuantity(quantity);

        return shippingCalculationService.calculateShippingFee(request).getTotalFee();
    }

    /**
     * Tính phí shipping theo ID của OrderItem - PHIÊN BẢN ĐƠN GIẢN  
     * CHỈ tính theo trọng lượng và thể tích, KHÔNG tính khoảng cách và ServiceType
     */
    public ShippingFeeBreakdown calculateBasicShippingFeeById(Long orderItemId) {
        log.info("Calculating BASIC shipping fee for OrderItem ID: {}", orderItemId);
        
        OrderItem orderItem = orderItemRepository.findById(orderItemId)
                .orElseThrow(() -> new RuntimeException("OrderItem not found with id: " + orderItemId));
        
        Product product = orderItem.getProduct();
        if (product == null) {
            throw new EntityNotFoundException("Product not found for OrderItem ID: " + orderItemId);
        }
        
        // Tạo request với thông tin cơ bản
        ShippingCalculationRequest request = new ShippingCalculationRequest();
        request.setActualWeight(product.getWeight().multiply(new BigDecimal(orderItem.getQuantity())));
        request.setVolume(product.getVolume().multiply(new BigDecimal(orderItem.getQuantity())));
        request.setQuantity(orderItem.getQuantity());
        // Không cần distance, serviceType, fragile nữa
        
        log.info("OrderItem {} - Weight: {} kg, Volume: {} cm³, Quantity: {}", 
                orderItemId, request.getActualWeight(), request.getVolume(), request.getQuantity());
        
        // Sử dụng method chính đã được cập nhật
        ShippingFeeBreakdown breakdown = shippingCalculationService.calculateShippingFee(request);
        
        log.info("Calculated BASIC shipping fee breakdown: {}", breakdown);
        
        return breakdown;
    }
}
