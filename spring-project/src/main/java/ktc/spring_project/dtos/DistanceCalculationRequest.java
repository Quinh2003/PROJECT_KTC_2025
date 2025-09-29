package ktc.spring_project.dtos;

import java.math.BigDecimal;

public class DistanceCalculationRequest {
    private BigDecimal fromLatitude;
    private BigDecimal fromLongitude;
    private BigDecimal toLatitude;
    private BigDecimal toLongitude;

    public DistanceCalculationRequest() {}

    public DistanceCalculationRequest(BigDecimal fromLatitude, BigDecimal fromLongitude, 
                                    BigDecimal toLatitude, BigDecimal toLongitude) {
        this.fromLatitude = fromLatitude;
        this.fromLongitude = fromLongitude;
        this.toLatitude = toLatitude;
        this.toLongitude = toLongitude;
    }

    // Getters and setters
    public BigDecimal getFromLatitude() { return fromLatitude; }
    public void setFromLatitude(BigDecimal fromLatitude) { this.fromLatitude = fromLatitude; }

    public BigDecimal getFromLongitude() { return fromLongitude; }
    public void setFromLongitude(BigDecimal fromLongitude) { this.fromLongitude = fromLongitude; }

    public BigDecimal getToLatitude() { return toLatitude; }
    public void setToLatitude(BigDecimal toLatitude) { this.toLatitude = toLatitude; }

    public BigDecimal getToLongitude() { return toLongitude; }
    public void setToLongitude(BigDecimal toLongitude) { this.toLongitude = toLongitude; }
}
