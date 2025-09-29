package ktc.spring_project.services;
import ktc.spring_project.exceptions.HttpException;

import ktc.spring_project.dtos.DistanceCalculationRequest;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;

@Service
public class DistanceCalculationService {

    /**
     * Tính khoảng cách giữa 2 điểm dựa trên tọa độ latitude/longitude
     * Sử dụng công thức Haversine để tính khoảng cách trên hình cầu (Earth)
     * 
     * @param request DistanceCalculationRequest chứa tọa độ điểm đi và điểm đến
     * @return khoảng cách tính bằng km
     */
    public BigDecimal calculateDistance(DistanceCalculationRequest request) {
        return calculateDistance(
            request.getFromLatitude(),
            request.getFromLongitude(),
            request.getToLatitude(),
            request.getToLongitude()
        );
    }

    /**
     * Tính khoảng cách giữa 2 điểm dựa trên tọa độ latitude/longitude
     * Sử dụng công thức Haversine
     * 
     * @param lat1 Latitude điểm đi
     * @param lon1 Longitude điểm đi
     * @param lat2 Latitude điểm đến
     * @param lon2 Longitude điểm đến
     * @return khoảng cách tính bằng km
     */
    public BigDecimal calculateDistance(BigDecimal lat1, BigDecimal lon1, BigDecimal lat2, BigDecimal lon2) {
        if (lat1 == null || lon1 == null || lat2 == null || lon2 == null) {
            throw new HttpException("Tọa độ không được để trống", org.springframework.http.HttpStatus.BAD_REQUEST);
        }

        // Chuyển đổi từ độ sang radian
        double lat1Rad = Math.toRadians(lat1.doubleValue());
        double lon1Rad = Math.toRadians(lon1.doubleValue());
        double lat2Rad = Math.toRadians(lat2.doubleValue());
        double lon2Rad = Math.toRadians(lon2.doubleValue());

        // Công thức Haversine
        double deltaLat = lat2Rad - lat1Rad;
        double deltaLon = lon2Rad - lon1Rad;

        double a = Math.sin(deltaLat / 2) * Math.sin(deltaLat / 2) +
                   Math.cos(lat1Rad) * Math.cos(lat2Rad) *
                   Math.sin(deltaLon / 2) * Math.sin(deltaLon / 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        // Bán kính trái đất tính bằng km
        double earthRadius = 6371.0;
        double distance = earthRadius * c;

        return new BigDecimal(distance).setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Tính khoảng cách theo đường thẳng (straight line distance)
     * Đây là phương pháp đơn giản hơn nhưng ít chính xác hơn Haversine
     * 
     * @param lat1 Latitude điểm đi
     * @param lon1 Longitude điểm đi  
     * @param lat2 Latitude điểm đến
     * @param lon2 Longitude điểm đến
     * @return khoảng cách tính bằng km
     */
    public BigDecimal calculateStraightLineDistance(BigDecimal lat1, BigDecimal lon1, BigDecimal lat2, BigDecimal lon2) {
        if (lat1 == null || lon1 == null || lat2 == null || lon2 == null) {
            throw new HttpException("Tọa độ không được để trống", org.springframework.http.HttpStatus.BAD_REQUEST);
        }

        // Khoảng cách 1 độ latitude ≈ 111 km
        // Khoảng cách 1 độ longitude phụ thuộc vào latitude
        double latDiff = lat2.subtract(lat1).doubleValue();
        double lonDiff = lon2.subtract(lon1).doubleValue();

        // Tính khoảng cách theo đường thẳng
        double avgLat = (lat1.doubleValue() + lat2.doubleValue()) / 2;
        double latDistance = latDiff * 111.32; // km per degree latitude
        double lonDistance = lonDiff * 111.32 * Math.cos(Math.toRadians(avgLat)); // km per degree longitude

        double distance = Math.sqrt(latDistance * latDistance + lonDistance * lonDistance);

        return new BigDecimal(distance).setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Kiểm tra xem 2 điểm có nằm trong cùng một vùng không
     * Dựa trên khoảng cách để phân loại vùng
     * 
     * @param distance khoảng cách tính bằng km
     * @return loại vùng: "INNER_CITY", "SUBURBAN", "INTER_PROVINCIAL"
     */
    public String getZoneType(BigDecimal distance) {
        if (distance.compareTo(new BigDecimal("15")) <= 0) {
            return "INNER_CITY"; // Nội thành: 0-15km
        } else if (distance.compareTo(new BigDecimal("50")) <= 0) {
            return "SUBURBAN"; // Ngoại thành: 15-50km
        } else {
            return "INTER_PROVINCIAL"; // Liên tỉnh: >50km
        }
    }
}
