package ktc.spring_project.enums;

import java.math.BigDecimal;

/**
 * Transport modes for order delivery
 * Used in orders table.transport_mode
 * Affects shipping fee calculation
 */
public enum TransportMode {
    STANDARD("Standard Delivery", "Regular delivery within 3-5 days", new BigDecimal("1.0")),
    EXPRESS("Express Delivery", "Fast delivery within 1-2 days", new BigDecimal("1.5")),
    PRIORITY("Priority Delivery", "Same day or next day delivery", new BigDecimal("2.0"));
    
    private final String displayName;
    private final String description;
    private final BigDecimal feeMultiplier; // Multiplier for shipping fee calculation
    
    TransportMode(String displayName, String description, BigDecimal feeMultiplier) {
        this.displayName = displayName;
        this.description = description;
        this.feeMultiplier = feeMultiplier;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getDescription() {
        return description;
    }
    
    public BigDecimal getFeeMultiplier() {
        return feeMultiplier;
    }
}