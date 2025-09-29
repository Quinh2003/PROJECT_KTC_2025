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

    // BẢNG GIÁ VIETTELPOST CHO ĐÀ NẴNG (Theo hình user cung cấp)
    // Phí cơ bản + Phí nội cụm
    
    // Phí cơ bản cho các mức trọng lượng (VNĐ) - Theo cột đầu tiên
    private static final BigDecimal BASE_FEE_UNDER_250G = new BigDecimal("16500");
    private static final BigDecimal BASE_FEE_250_500G = new BigDecimal("16500");
    private static final BigDecimal BASE_FEE_500_1000G = new BigDecimal("16500");
    private static final BigDecimal BASE_FEE_1000_1500G = new BigDecimal("16500");
    private static final BigDecimal BASE_FEE_1500_2000G = new BigDecimal("16500");
    private static final BigDecimal BASE_FEE_2000_2500G = new BigDecimal("16500");
    private static final BigDecimal BASE_FEE_2500_3000G = new BigDecimal("16500");
    private static final BigDecimal BASE_FEE_ADDITIONAL_500G = new BigDecimal("2500");
    
    // Phí nội cụm (VNĐ) - Theo cột "Nội cụm" 
    private static final BigDecimal INNER_AREA_FEE_UNDER_250G = new BigDecimal("17500");
    private static final BigDecimal INNER_AREA_FEE_250_500G = new BigDecimal("175500");
    private static final BigDecimal INNER_AREA_FEE_500_1000G = new BigDecimal("17750");
    private static final BigDecimal INNER_AREA_FEE_1000_1500G = new BigDecimal("18950");
    private static final BigDecimal INNER_AREA_FEE_1500_2000G = new BigDecimal("19100");
    private static final BigDecimal INNER_AREA_FEE_2000_2500G = new BigDecimal("19300");
    private static final BigDecimal INNER_AREA_FEE_2500_3000G = new BigDecimal("19500");
    private static final BigDecimal INNER_AREA_FEE_ADDITIONAL_500G = new BigDecimal("3000");
    
    // CHỈ PHỤC VỤ NỘI CỤM ĐÀ NẴNG - Không cần phí liên miền
    
    // Hệ số First Class so với Standard
    private static final double FIRST_CLASS_MULTIPLIER = 1.2; // Tăng 20% so với Standard

    /**
     * Tính delivery fee cho một Order
     * CÔNG THỨC VIETTELPOST: Phí ship dựa trên trọng lượng tổng + hệ số Service Type
     * CHỈ PHỤC VỤ NỘI THÀNH ĐÀ NẴNG
     * 
     * @param order Order cần tính delivery fee
     * @param serviceType Loại dịch vụ delivery (STANDARD hoặc FIRST_CLASS)
     * @return DeliveryFeeBreakdown chứa chi tiết tính phí
     */
    public DeliveryFeeBreakdown calculateDeliveryFee(Order order, ServiceType serviceType) {
        log.info("Calculating ViettelPost delivery fee for Order ID: {} with ServiceType: {}", order.getId(), serviceType);
        
        // 1. Tính tổng trọng lượng
        BigDecimal totalWeight = calculateTotalWeight(order);
        
        // 2. Tính phí shipping theo công thức ViettelPost
        BigDecimal viettelPostFee = calculateViettelPostShippingFee(order, serviceType);
        
        // 3. Đà Nẵng chỉ nội thành, không có phí khoảng cách
        BigDecimal distanceFee = BigDecimal.ZERO;
        BigDecimal distance = BigDecimal.ZERO; // Không cần tính khoảng cách cho nội thành
        
        // 4. Lấy hệ số service type cho hiển thị
        BigDecimal serviceMultiplier = getServiceTypeMultiplier(serviceType);
        
        // 5. Tổng delivery fee = phí ViettelPost (đã bao gồm service type)
        BigDecimal totalDeliveryFee = viettelPostFee;
        
        // 6. Tính phí base để tương thích với frontend (trước khi áp dụng service type)
        BigDecimal baseDeliveryFee = viettelPostFee;
        if (serviceType == ServiceType.FIRST_CLASS) {
            baseDeliveryFee = viettelPostFee.divide(BigDecimal.valueOf(FIRST_CLASS_MULTIPLIER), 2, RoundingMode.HALF_UP);
        }
        
        // 7. Tạo chi tiết tính toán
        String calculationDetails = buildViettelPostCalculationDetails(
            order, totalWeight, serviceType, viettelPostFee, totalDeliveryFee
        );
        
        return DeliveryFeeBreakdown.builder()
                .orderId(order.getId())
                .totalShippingFee(baseDeliveryFee) // Phí cơ bản trước service type
                .distanceFee(distanceFee) // Luôn = 0 cho Đà Nẵng
                .distance(distance) // Không áp dụng cho nội thành
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
     * Tính phí shipping theo công thức ViettelPost dựa trên trọng lượng
     * CHỈ PHỤC VỤ NỘI THÀNH ĐÀ NẴNG - KHÔNG CÓ PHÍ KHOẢNG CÁCH
     */
    private BigDecimal calculateViettelPostShippingFee(Order order, ServiceType serviceType) {
        // Tính tổng trọng lượng của tất cả OrderItems
        BigDecimal totalWeight = calculateTotalWeight(order);
        
        // Tính phí theo bảng giá ViettelPost Standard
        BigDecimal standardFee = calculateStandardShippingFee(totalWeight);
        
        // Áp dụng hệ số cho First Class nếu cần
        if (serviceType == ServiceType.FIRST_CLASS) {
            standardFee = standardFee.multiply(BigDecimal.valueOf(FIRST_CLASS_MULTIPLIER))
                    .setScale(0, RoundingMode.HALF_UP);
        }
        
        return standardFee;
    }
    
    /**
     * Tính tổng trọng lượng của tất cả OrderItems trong Order (gram)
     */
    private BigDecimal calculateTotalWeight(Order order) {
        List<OrderItem> orderItems = orderItemRepository.findByOrderId(order.getId());
        
        BigDecimal totalWeight = BigDecimal.ZERO;
        
        for (OrderItem item : orderItems) {
            Product product = item.getProduct();
            if (product != null && product.getWeight() != null && item.getQuantity() != null) {
                // Product weight trong DB đã là kg, chuyển đổi sang gram và nhân với số lượng
                BigDecimal itemWeightGrams = product.getWeight()
                        .multiply(new BigDecimal("1000")) // kg to grams
                        .multiply(BigDecimal.valueOf(item.getQuantity()));
                totalWeight = totalWeight.add(itemWeightGrams);
                
                log.debug("OrderItem {}: Product weight = {} kg, Quantity = {}, Total weight = {} grams", 
                    item.getId(), product.getWeight(), item.getQuantity(), itemWeightGrams);
            } else {
                log.warn("OrderItem {} has missing product or weight data", item.getId());
            }
        }
        
        log.info("Order {} total weight: {} grams", order.getId(), totalWeight);
        return totalWeight;
    }
    
    /**
     * Tính phí Standard theo bảng giá ViettelPost (nội cụm Đà Nẵng)
     * Sử dụng cột "Nội cụm" từ bảng giá
     */
    private BigDecimal calculateStandardShippingFee(BigDecimal weightInGrams) {
        if (weightInGrams.compareTo(new BigDecimal("250")) <= 0) {
            return INNER_AREA_FEE_UNDER_250G;
        } else if (weightInGrams.compareTo(new BigDecimal("500")) <= 0) {
            return INNER_AREA_FEE_250_500G;
        } else if (weightInGrams.compareTo(new BigDecimal("1000")) <= 0) {
            return INNER_AREA_FEE_500_1000G;
        } else if (weightInGrams.compareTo(new BigDecimal("1500")) <= 0) {
            return INNER_AREA_FEE_1000_1500G;
        } else if (weightInGrams.compareTo(new BigDecimal("2000")) <= 0) {
            return INNER_AREA_FEE_1500_2000G;
        } else if (weightInGrams.compareTo(new BigDecimal("2500")) <= 0) {
            return INNER_AREA_FEE_2000_2500G;
        } else if (weightInGrams.compareTo(new BigDecimal("3000")) <= 0) {
            return INNER_AREA_FEE_2500_3000G;
        } else {
            // Trên 3kg: phí cơ bản 3kg + phí cho từng 500g tiếp theo
            BigDecimal baseFee = INNER_AREA_FEE_2500_3000G;
            BigDecimal excessWeight = weightInGrams.subtract(new BigDecimal("3000"));
            BigDecimal additional500gBlocks = excessWeight.divide(new BigDecimal("500"), 0, RoundingMode.UP);
            BigDecimal additionalFee = additional500gBlocks.multiply(INNER_AREA_FEE_ADDITIONAL_500G);
            return baseFee.add(additionalFee);
        }
    }
    
    /**
     * DEPRECATED: Phí khoảng cách không áp dụng cho Đà Nẵng (chỉ nội thành)
     */
    private BigDecimal calculateDistanceFee(Order order) {
        // Đà Nẵng chỉ phục vụ nội thành, không có phí khoảng cách
        return BigDecimal.ZERO;
    }

    /**
     * Lấy hệ số service type
     */
    private BigDecimal getServiceTypeMultiplier(ServiceType serviceType) {
        return BigDecimal.valueOf(serviceType.getMultiplier());
    }

    /**
     * Tạo chi tiết tính toán delivery fee theo công thức ViettelPost
     */
    private String buildViettelPostCalculationDetails(Order order, BigDecimal totalWeight,
                                                     ServiceType serviceType, BigDecimal viettelPostFee, 
                                                     BigDecimal totalDeliveryFee) {
        StringBuilder details = new StringBuilder();
        
        details.append("CHI TIẾT TÍNH DELIVERY FEE THEO VIETTELPOST:\n");
        details.append("==========================================\n");
        
        // Thông tin cơ bản
        details.append("📦 THÔNG TIN ORDER:\n");
        details.append(String.format("- Order ID: %d\n", order.getId()));
        details.append(String.format("- Store: %s\n", order.getStore().getStoreName()));
        details.append(String.format("- Địa chỉ giao hàng: %s\n", order.getAddress().getAddress()));
        details.append(String.format("- Tổng trọng lượng: %.0f gram\n", totalWeight));
        details.append(String.format("- Khu vực: Nội cụm Đà Nẵng\n"));
        details.append(String.format("- Service Type: %s\n\n", serviceType.getDisplayName()));
        
        // Tính phí theo bảng giá ViettelPost
        details.append("💰 TÍNH DELIVERY FEE (VIETTELPOST):\n");
        
        // Phân loại trọng lượng
        String weightRange = getWeightRangeDescription(totalWeight);
        details.append(String.format("- Phân loại trọng lượng: %s\n", weightRange));
        
        if (serviceType == ServiceType.FIRST_CLASS) {
            BigDecimal standardFee = viettelPostFee.divide(BigDecimal.valueOf(FIRST_CLASS_MULTIPLIER), 2, RoundingMode.HALF_UP);
            details.append(String.format("- Phí Standard: %,.0f VNĐ\n", standardFee));
            details.append(String.format("- Hệ số First Class: %.2f\n", FIRST_CLASS_MULTIPLIER));
            details.append(String.format("- Phí First Class: %,.0f × %.2f = %,.0f VNĐ\n", 
                    standardFee, FIRST_CLASS_MULTIPLIER, viettelPostFee));
        } else {
            details.append(String.format("- Phí Standard: %,.0f VNĐ\n", viettelPostFee));
        }
        
        details.append(String.format("- TỔNG DELIVERY FEE: %,.0f VNĐ", totalDeliveryFee));
        
        return details.toString();
    }
    
    /**
     * Lấy mô tả phân loại trọng lượng
     */
    private String getWeightRangeDescription(BigDecimal weightInGrams) {
        if (weightInGrams.compareTo(new BigDecimal("250")) <= 0) {
            return "Dưới 250g";
        } else if (weightInGrams.compareTo(new BigDecimal("500")) <= 0) {
            return "250g - 500g";
        } else if (weightInGrams.compareTo(new BigDecimal("1000")) <= 0) {
            return "500g - 1kg";
        } else if (weightInGrams.compareTo(new BigDecimal("1500")) <= 0) {
            return "1kg - 1.5kg";
        } else if (weightInGrams.compareTo(new BigDecimal("2000")) <= 0) {
            return "1.5kg - 2kg";
        } else if (weightInGrams.compareTo(new BigDecimal("2500")) <= 0) {
            return "2kg - 2.5kg";
        } else if (weightInGrams.compareTo(new BigDecimal("3000")) <= 0) {
            return "2.5kg - 3kg";
        } else {
            return String.format("Trên 3kg (%.0fg)", weightInGrams);
        }
    }
    
    /**
     * DEPRECATED: Tạo chi tiết tính toán delivery fee (THEO FRONTEND LOGIC CŨ)
     */
    private String buildDeliveryCalculationDetails(Order order, BigDecimal totalShippingFee, 
                                                 BigDecimal distanceFee, BigDecimal distance,
                                                 ServiceType serviceType, BigDecimal serviceMultiplier,
                                                 BigDecimal adjustedShippingFee, BigDecimal totalDeliveryFee) {
        StringBuilder details = new StringBuilder();
        
        details.append("CHI TIẾT TÍNH DELIVERY FEE (DEPRECATED - FRONTEND LOGIC CŨ):\n");
        details.append("======================================================\n");
        
        // Thông tin cơ bản
        details.append("📦 THÔNG TIN ORDER:\n");
        details.append(String.format("- Order ID: %d\n", order.getId()));
        details.append(String.format("- Store: %s\n", order.getStore().getStoreName()));
        details.append(String.format("- Địa chỉ giao hàng: %s\n", order.getAddress().getAddress()));
        details.append(String.format("- Khoảng cách: %.2f km\n", distance));
        details.append(String.format("- Service Type: %s\n\n", serviceType.getDisplayName()));
        
        // Tính phí theo logic Frontend
        details.append("💰 TÍNH DELIVERY FEE (FRONTEND LOGIC CŨ):\n");
        details.append(String.format("- Tổng shipping fee OrderItems: %,.0f VNĐ\n", totalShippingFee));
        details.append(String.format("- Hệ số %s: %.1f\n", serviceType.getDisplayName(), serviceMultiplier));
        details.append(String.format("- Shipping fee sau hệ số: %,.0f × %.1f = %,.0f VNĐ\n", 
                totalShippingFee, serviceMultiplier, adjustedShippingFee));
        
        // Chi tiết phí khoảng cách (theo Frontend)
        String zoneType = "Nội thành Đà Nẵng (không có phí khoảng cách)";
        
        // details.append(String.format("- Phí khoảng cách (%s): %,.0f VNĐ\n", zoneType, distanceFee));
        details.append(String.format("- TỔNG DELIVERY FEE: %,.0f + %,.0f = %,.0f VNĐ", 
                adjustedShippingFee, distanceFee, totalDeliveryFee));
        
        return details.toString();
    }
}
