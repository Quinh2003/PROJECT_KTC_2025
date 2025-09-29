package ktc.spring_project.services;

import ktc.spring_project.dtos.ShippingCalculationRequest;
import ktc.spring_project.dtos.ShippingFeeBreakdown;
import ktc.spring_project.enums.ServiceType;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;

@Service
public class ShippingCalculationService {

    // Đơn giá cơ bản - CẬP NHẬT THEO BẢNG GIÁ MỚI
    private static final BigDecimal WEIGHT_RATE_PER_KG = new BigDecimal("5000"); // 5,000 VNĐ/kg (GIẢM GIÁ)
    private static final BigDecimal VOLUME_DIVISOR = new BigDecimal("5000"); // Chia 5000 để tính trọng lượng quy đổi

    // Phí cơ bản theo vùng - BẢNG GIÁ MỚI 
    private static final BigDecimal INNER_CITY_BASE_FEE = new BigDecimal("15000"); // 15,000 VNĐ
    private static final BigDecimal SUBURBAN_BASE_FEE = new BigDecimal("25000"); // 25,000 VNĐ  
    private static final BigDecimal INTER_PROVINCIAL_BASE_FEE = new BigDecimal("40000"); // 40,000 VNĐ

    // Đơn giá theo km - BẢNG GIÁ MỚI (GIẢM GIÁ) 
    private static final BigDecimal INNER_CITY_RATE_PER_KM = new BigDecimal("1800"); // 1,800 VNĐ/km
    private static final BigDecimal SUBURBAN_RATE_PER_KM = new BigDecimal("1500"); // 1,500 VNĐ/km
    private static final BigDecimal INTER_PROVINCIAL_RATE_PER_KM = new BigDecimal("500"); // 500 VNĐ/km (giảm từ 1,000)

    // Hệ số rủi ro cho hàng dễ vỡ
    private static final BigDecimal FRAGILE_MULTIPLIER = new BigDecimal("1.3");
    private static final BigDecimal NORMAL_MULTIPLIER = new BigDecimal("1.0");

    /**
     * Tính tổng phí shipping cho order item - PHIÊN BẢN ĐƠN GIẢN
     * CHỈ TÍNH THEO TRỌNG LƯỢNG, THỂ TÍCH VÀ HỆ SỐ RỦI RO
     * KHÔNG tính khoảng cách và ServiceType
     * CÔNG THỨC: TỔNG PHÍ = MAX(Trọng lượng thực tế, Trọng lượng quy đổi) × 5,000 × HỆ SỐ RỦI RO × SỐ LƯỢNG
     * 
     * @param request ShippingCalculationRequest chứa thông tin cần thiết
     * @return ShippingFeeBreakdown chứa chi tiết tính phí
     */
    public ShippingFeeBreakdown calculateShippingFee(ShippingCalculationRequest request) {
        // 1. Tính trọng lượng quy đổi từ thể tích
        BigDecimal volumeWeight = calculateVolumeWeight(request);
        
        // 2. Trọng lượng tính phí = MAX(trọng lượng thực tế, trọng lượng quy đổi)
        BigDecimal billableWeight = request.getActualWeight().max(volumeWeight);
        
        // 3. Phí cơ bản = trọng lượng tính phí × đơn giá
        BigDecimal baseFee = billableWeight.multiply(WEIGHT_RATE_PER_KG);

        // 4. Hệ số rủi ro (CÓ fragile, KHÔNG có ServiceType)
        BigDecimal riskMultiplier = getRiskMultiplier(request.isFragile());

        // 5. Tổng phí = phí cơ bản × hệ số rủi ro × số lượng
        BigDecimal totalFee = baseFee.multiply(riskMultiplier).multiply(new BigDecimal(request.getQuantity()));

        // 6. Tạo chuỗi mô tả chi tiết tính toán
        String calculationDetails = buildSimpleCalculationDetails(request, billableWeight, volumeWeight, baseFee, riskMultiplier, totalFee);

        return ShippingFeeBreakdown.builder()
                .baseFee(baseFee)
                .weightFee(baseFee)
                .distanceFee(BigDecimal.ZERO)
                .riskMultiplier(riskMultiplier)
                .serviceMultiplier(BigDecimal.ONE)
                .totalFee(totalFee)
                .distance(BigDecimal.ZERO)
                .billableWeight(billableWeight)
                .volumeWeight(volumeWeight)
                .calculationDetails(calculationDetails)
                .build();
    }

    /**
     * Tính phí dựa trên trọng lượng
     * Phí trọng lượng = Trọng lượng tính phí × 5,000 VNĐ/kg
     * Trọng lượng tính phí = MAX(Trọng lượng thực tế, Trọng lượng quy đổi)
     */
    private BigDecimal calculateWeightBasedFee(ShippingCalculationRequest request) {
        BigDecimal volumeWeight = calculateVolumeWeight(request);
        BigDecimal billableWeight = request.getActualWeight().max(volumeWeight);
        return billableWeight.multiply(WEIGHT_RATE_PER_KG);
    }

    /**
     * Tính trọng lượng quy đổi từ thể tích
     * Trọng lượng quy đổi = Volume ÷ 5000
     */
    private BigDecimal calculateVolumeWeight(ShippingCalculationRequest request) {
        if (request.getVolume() == null || request.getVolume().compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        
        return request.getVolume().divide(VOLUME_DIVISOR, 2, RoundingMode.HALF_UP);
    }

    /**
     * Tính phí dựa trên khoảng cách với công thức đơn giản
     * Phí khoảng cách = Khoảng cách × Đơn giá/km + Phí cơ bản vùng
     */
    private BigDecimal calculateDistanceBasedFee(ShippingCalculationRequest request) {
        BigDecimal distance = request.getDistance();
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
     * Lấy hệ số rủi ro dựa trên tính chất hàng hóa
     */
    private BigDecimal getRiskMultiplier(boolean isFragile) {
        return isFragile ? FRAGILE_MULTIPLIER : NORMAL_MULTIPLIER;
    }

    /**
     * Lấy hệ số service type
     */
    private BigDecimal getServiceTypeMultiplier(ServiceType serviceType) {
        return BigDecimal.valueOf(serviceType.getMultiplier());
    }

    /**
     * Tạo chuỗi mô tả chi tiết quá trình tính toán - PHIÊN BẢN ĐƠN GIẢN CÓ HỆ SỐ RỦI RO
     */
    private String buildSimpleCalculationDetails(ShippingCalculationRequest request, 
                                               BigDecimal billableWeight, BigDecimal volumeWeight,
                                               BigDecimal baseFee, BigDecimal riskMultiplier, BigDecimal totalFee) {
        StringBuilder details = new StringBuilder();
        
        details.append("CHI TIẾT TÍNH PHÍ SHIPPING ĐƠN GIẢN:\n");
        details.append("======================================\n");
        
        // Thông tin cơ bản
        details.append("📦 THÔNG TIN HÀNG HÓA:\n");
        details.append(String.format("- Trọng lượng thực tế: %.2f kg\n", request.getActualWeight()));
        details.append(String.format("- Thể tích: %.0f cm³\n", request.getVolume()));
        details.append(String.format("- Trọng lượng quy đổi: %.2f kg\n", volumeWeight));
        details.append(String.format("- Trọng lượng tính phí: %.2f kg\n", billableWeight));
        details.append(String.format("- Hàng dễ vỡ: %s\n", request.isFragile() ? "Có" : "Không"));
        details.append(String.format("- Số lượng: %d\n\n", request.getQuantity()));
        
        // Tính phí
        details.append("💰 TÍNH PHÍ:\n");
        details.append(String.format("- Phí cơ bản = MAX(%.2f, %.2f) × 5,000 = %,.0f VNĐ\n", 
                request.getActualWeight(), volumeWeight, baseFee));
        details.append(String.format("- Hệ số rủi ro: %.1f (hàng %s)\n", 
                riskMultiplier, request.isFragile() ? "dễ vỡ" : "thường"));
        details.append(String.format("- Phí sau hệ số = %,.0f × %.1f = %,.0f VNĐ\n", 
                baseFee, riskMultiplier, baseFee.multiply(riskMultiplier)));
        details.append(String.format("- Tổng phí = %,.0f × %d = %,.0f VNĐ\n", 
                baseFee.multiply(riskMultiplier), request.getQuantity(), totalFee));
        details.append("\n✅ CÓ hệ số rủi ro");
        details.append("\n❌ KHÔNG tính khoảng cách");
        details.append("\n❌ KHÔNG có hệ số ServiceType");
        
        return details.toString();
    }

    /**
     * Tạo chuỗi mô tả chi tiết quá trình tính toán - PHIÊN BẢN CŨ (KHÔNG DÙNG)
     */
    private String buildCalculationDetails(ShippingCalculationRequest request, 
                                         BigDecimal weightFee, BigDecimal distanceFee,
                                         BigDecimal baseFee, BigDecimal riskMultiplier, 
                                         BigDecimal serviceMultiplier, BigDecimal billableWeight,
                                         BigDecimal volumeWeight) {
        StringBuilder details = new StringBuilder();
        
        details.append("CHI TIẾT TÍNH PHÍ SHIPPING:\n");
        details.append("==================================\n");
        
        // Thông tin cơ bản
        details.append("📦 THÔNG TIN HÀNG HÓA:\n");
        details.append(String.format("- Trọng lượng thực tế: %.2f kg\n", request.getActualWeight()));
        details.append(String.format("- Thể tích: %.0f cm³\n", request.getVolume()));
        details.append(String.format("- Trọng lượng quy đổi: %.2f kg\n", volumeWeight));
        details.append(String.format("- Trọng lượng tính phí: %.2f kg\n", billableWeight));
        details.append(String.format("- Khoảng cách: %.2f km\n", request.getDistance()));
        details.append(String.format("- Hàng dễ vỡ: %s\n", request.isFragile() ? "Có" : "Không"));
        details.append(String.format("- Loại dịch vụ: %s\n", request.getServiceType().getDisplayName()));
        details.append(String.format("- Số lượng: %d\n\n", request.getQuantity()));
        
        // Tính phí cơ bản
        details.append("💰 TÍNH PHÍ CƠ BẢN:\n");
        details.append(String.format("- Phí theo trọng lượng: %.2f kg × 5,000 = %,.0f VNĐ\n", 
                billableWeight, weightFee));
        
        // Chi tiết phí khoảng cách theo công thức đơn giản
        String zoneType;
        BigDecimal zoneBaseFee;
        BigDecimal ratePerKm;
        BigDecimal distance = request.getDistance();
        
        if (distance.compareTo(new BigDecimal("15")) <= 0) {
            zoneType = "Nội thành";
            zoneBaseFee = INNER_CITY_BASE_FEE;
            ratePerKm = INNER_CITY_RATE_PER_KM;
        } else if (distance.compareTo(new BigDecimal("50")) <= 0) {
            zoneType = "Ngoại thành";
            zoneBaseFee = SUBURBAN_BASE_FEE;
            ratePerKm = SUBURBAN_RATE_PER_KM;
        } else {
            zoneType = "Liên tỉnh";
            zoneBaseFee = INTER_PROVINCIAL_BASE_FEE;
            ratePerKm = INTER_PROVINCIAL_RATE_PER_KM;
        }
        
        details.append(String.format("- Phí theo khoảng cách (%s):\n", zoneType));
        details.append(String.format("  + Phí cơ bản: %,.0f VNĐ\n", zoneBaseFee));
        details.append(String.format("  + Phí theo km: %.2f km × %,.0f VNĐ/km = %,.0f VNĐ\n", 
                distance, ratePerKm, distance.multiply(ratePerKm)));
        details.append(String.format("  → Tổng phí khoảng cách: %,.0f VNĐ\n", distanceFee));
        
        details.append(String.format("- Phí cơ bản = MAX(%,.0f, %,.0f) = %,.0f VNĐ\n\n", 
                weightFee, distanceFee, baseFee));
        
        // Hệ số
        details.append("📊 HỆ SỐ ĐIỀU CHỈNH:\n");
        details.append(String.format("- Hệ số rủi ro: %.1f (hàng %s)\n", 
                riskMultiplier, request.isFragile() ? "dễ vỡ" : "thường"));
        details.append(String.format("- Hệ số dịch vụ: %.1f (%s)\n", 
                serviceMultiplier, request.getServiceType().getDisplayName()));
        
        // Tổng phí
        BigDecimal singleItemFee = baseFee.multiply(riskMultiplier).multiply(serviceMultiplier);
        details.append(String.format("\n🧮 TÍNH TỔNG PHÍ:\n"));
        details.append(String.format("- Phí 1 sản phẩm: %,.0f × %.1f × %.1f = %,.0f VNĐ\n", 
                baseFee, riskMultiplier, serviceMultiplier, singleItemFee));
        details.append(String.format("- Tổng phí (%d sản phẩm): %,.0f × %d = %,.0f VNĐ", 
                request.getQuantity(), singleItemFee, request.getQuantity(), 
                singleItemFee.multiply(new BigDecimal(request.getQuantity()))));
        
        return details.toString();
    }

    /**
     * Tính phí shipping đơn giản chỉ với trọng lượng, thể tích và khoảng cách
     */
    public BigDecimal calculateSimpleShippingFee(BigDecimal weight, BigDecimal volume, BigDecimal distance) {
        ShippingCalculationRequest request = new ShippingCalculationRequest();
        request.setActualWeight(weight);
        request.setVolume(volume);
        request.setDistance(distance);
        request.setFragile(false);
        request.setServiceType(ServiceType.STANDARD);
        request.setQuantity(1);
        
        return calculateShippingFee(request).getTotalFee();
    }
}
