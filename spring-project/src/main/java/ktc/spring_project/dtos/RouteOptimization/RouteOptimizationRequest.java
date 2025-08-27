package ktc.spring_project.dtos.RouteOptimization;

import java.util.List;

public class RouteOptimizationRequest {
    public List<Location> locations; // Danh sách điểm dừng (pickup/delivery)
    public Location origin; // Vị trí xuất phát (tài xế)
    public Location destination; // Vị trí kết thúc (nếu khác origin)

    public static class Location {
        public double lat;
        public double lng;
        public String address;
    }
}