package ktc.spring_project.dtos.RouteOptimization;

import java.util.List;

public class RouteOptimizationResponse {
    public List<RouteStep> steps;
    public double totalDistance; // mét
    public double totalDuration; // giây

    public static class RouteStep {
        public String address;
        public double lat;
        public double lng;
        public double distance; // mét
        public double duration; // giây
    }
}