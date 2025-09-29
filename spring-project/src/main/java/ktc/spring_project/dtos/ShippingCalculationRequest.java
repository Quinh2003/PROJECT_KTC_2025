package ktc.spring_project.dtos;

import ktc.spring_project.enums.ServiceType;
import java.math.BigDecimal;

public class ShippingCalculationRequest {
    private BigDecimal weight;        // Trọng lượng thực tế
    private BigDecimal actualWeight;  // Tổng trọng lượng (weight × quantity)
    private BigDecimal volume;
    private BigDecimal distance;
    private boolean isFragile;
    private ServiceType serviceType;
    private Integer quantity;

    public ShippingCalculationRequest() {}

    public ShippingCalculationRequest(BigDecimal weight, BigDecimal volume,
                                    BigDecimal distance, boolean isFragile, 
                                    ServiceType serviceType, Integer quantity) {
        this.weight = weight;
        this.actualWeight = weight != null ? weight.multiply(new BigDecimal(quantity)) : BigDecimal.ZERO;
        this.volume = volume;
        this.distance = distance;
        this.isFragile = isFragile;
        this.serviceType = serviceType;
        this.quantity = quantity;
    }

    public ShippingCalculationRequest(BigDecimal weight, BigDecimal actualWeight, BigDecimal volume,
                                    BigDecimal distance, boolean isFragile, 
                                    ServiceType serviceType, Integer quantity) {
        this.weight = weight;
        this.actualWeight = actualWeight;
        this.volume = volume;
        this.distance = distance;
        this.isFragile = isFragile;
        this.serviceType = serviceType;
        this.quantity = quantity;
    }

    // Getters and setters
    public BigDecimal getWeight() { return weight; }
    public void setWeight(BigDecimal weight) { this.weight = weight; }

    public BigDecimal getActualWeight() { return actualWeight; }
    public void setActualWeight(BigDecimal actualWeight) { this.actualWeight = actualWeight; }

    public BigDecimal getVolume() { return volume; }
    public void setVolume(BigDecimal volume) { this.volume = volume; }

    public BigDecimal getDistance() { return distance; }
    public void setDistance(BigDecimal distance) { this.distance = distance; }

    public boolean isFragile() { return isFragile; }
    public void setFragile(boolean fragile) { isFragile = fragile; }

    public ServiceType getServiceType() { return serviceType; }
    public void setServiceType(ServiceType serviceType) { this.serviceType = serviceType; }

    public Integer getQuantity() { return quantity; }
    public void setQuantity(Integer quantity) { this.quantity = quantity; }
}
