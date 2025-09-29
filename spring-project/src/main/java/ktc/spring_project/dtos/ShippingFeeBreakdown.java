package ktc.spring_project.dtos;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import ktc.spring_project.config.MoneyFormatSerializer;
import java.math.BigDecimal;

public class ShippingFeeBreakdown {
    @JsonSerialize(using = MoneyFormatSerializer.class)
    private BigDecimal baseFee;
    
    @JsonSerialize(using = MoneyFormatSerializer.class)
    private BigDecimal weightFee;
    
    @JsonSerialize(using = MoneyFormatSerializer.class)
    private BigDecimal distanceFee;
    
    private BigDecimal riskMultiplier;
    private BigDecimal serviceMultiplier;
    
    @JsonSerialize(using = MoneyFormatSerializer.class)
    private BigDecimal totalFee;
    
    private BigDecimal distance;
    private BigDecimal billableWeight;
    private BigDecimal volumeWeight;
    private String calculationDetails;

    public ShippingFeeBreakdown() {}

    // Builder pattern
    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private ShippingFeeBreakdown breakdown = new ShippingFeeBreakdown();

        public Builder baseFee(BigDecimal baseFee) {
            breakdown.baseFee = baseFee;
            return this;
        }

        public Builder weightFee(BigDecimal weightFee) {
            breakdown.weightFee = weightFee;
            return this;
        }

        public Builder distanceFee(BigDecimal distanceFee) {
            breakdown.distanceFee = distanceFee;
            return this;
        }

        public Builder riskMultiplier(BigDecimal riskMultiplier) {
            breakdown.riskMultiplier = riskMultiplier;
            return this;
        }

        public Builder serviceMultiplier(BigDecimal serviceMultiplier) {
            breakdown.serviceMultiplier = serviceMultiplier;
            return this;
        }

        public Builder totalFee(BigDecimal totalFee) {
            breakdown.totalFee = totalFee;
            return this;
        }

        public Builder distance(BigDecimal distance) {
            breakdown.distance = distance;
            return this;
        }

        public Builder billableWeight(BigDecimal billableWeight) {
            breakdown.billableWeight = billableWeight;
            return this;
        }

        public Builder volumeWeight(BigDecimal volumeWeight) {
            breakdown.volumeWeight = volumeWeight;
            return this;
        }

        public Builder calculationDetails(String calculationDetails) {
            breakdown.calculationDetails = calculationDetails;
            return this;
        }

        public ShippingFeeBreakdown build() {
            return breakdown;
        }
    }

    // Getters and setters
    public BigDecimal getBaseFee() { return baseFee; }
    public void setBaseFee(BigDecimal baseFee) { this.baseFee = baseFee; }

    public BigDecimal getWeightFee() { return weightFee; }
    public void setWeightFee(BigDecimal weightFee) { this.weightFee = weightFee; }

    public BigDecimal getDistanceFee() { return distanceFee; }
    public void setDistanceFee(BigDecimal distanceFee) { this.distanceFee = distanceFee; }

    public BigDecimal getRiskMultiplier() { return riskMultiplier; }
    public void setRiskMultiplier(BigDecimal riskMultiplier) { this.riskMultiplier = riskMultiplier; }

    public BigDecimal getServiceMultiplier() { return serviceMultiplier; }
    public void setServiceMultiplier(BigDecimal serviceMultiplier) { this.serviceMultiplier = serviceMultiplier; }

    public BigDecimal getTotalFee() { return totalFee; }
    public void setTotalFee(BigDecimal totalFee) { this.totalFee = totalFee; }

    public BigDecimal getDistance() { return distance; }
    public void setDistance(BigDecimal distance) { this.distance = distance; }

    public BigDecimal getBillableWeight() { return billableWeight; }
    public void setBillableWeight(BigDecimal billableWeight) { this.billableWeight = billableWeight; }

    public BigDecimal getVolumeWeight() { return volumeWeight; }
    public void setVolumeWeight(BigDecimal volumeWeight) { this.volumeWeight = volumeWeight; }

    public String getCalculationDetails() { return calculationDetails; }
    public void setCalculationDetails(String calculationDetails) { this.calculationDetails = calculationDetails; }
}
