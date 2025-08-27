package ktc.spring_project.services;

import ktc.spring_project.dtos.RouteOptimization.*;

import java.util.*;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.ResponseEntity;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

@Service
public class RouteOptimizationService {

    @Value("${google.maps.api.key}")
    private String googleApiKey;

    public RouteOptimizationResponse optimizeRoute(RouteOptimizationRequest request) {
        // Build waypoints string
        StringBuilder waypoints = new StringBuilder();
        for (RouteOptimizationRequest.Location loc : request.locations) {
            if (waypoints.length() > 0) waypoints.append("|");
            waypoints.append(loc.lat).append(",").append(loc.lng);
        }

                String url = UriComponentsBuilder.fromUriString("https://maps.googleapis.com/maps/api/directions/json")
                .queryParam("origin", request.origin.lat + "," + request.origin.lng)
                .queryParam("destination", request.destination.lat + "," + request.destination.lng)
                .queryParam("waypoints", "optimize:true|" + waypoints)
                .queryParam("key", googleApiKey)
                .toUriString();

        RestTemplate restTemplate = new RestTemplate();
        ResponseEntity<Map<String, Object>> responseEntity = restTemplate.exchange(
            url,
            org.springframework.http.HttpMethod.GET,
            null,
            new ParameterizedTypeReference<Map<String, Object>>() {}
        );
        Map<String, Object> response = responseEntity.getBody();


        List<RouteOptimizationResponse.RouteStep> steps = new ArrayList<>();
        double totalDistance = 0, totalDuration = 0;

        // Kiểm tra response hợp lệ
        if (response == null || !response.containsKey("routes")) {
            RouteOptimizationResponse result = new RouteOptimizationResponse();
            result.steps = steps;
            result.totalDistance = 0;
            result.totalDuration = 0;
            return result;
        }

        List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
        if (routes == null || routes.isEmpty()) {
            RouteOptimizationResponse result = new RouteOptimizationResponse();
            result.steps = steps;
            result.totalDistance = 0;
            result.totalDuration = 0;
            return result;
        }

        Map<String, Object> firstRoute = routes.get(0);
        List<Map<String, Object>> legs = (List<Map<String, Object>>) firstRoute.get("legs");
        if (legs == null) {
            RouteOptimizationResponse result = new RouteOptimizationResponse();
            result.steps = steps;
            result.totalDistance = 0;
            result.totalDuration = 0;
            return result;
        }

        for (Map<String, Object> leg : legs) {
            RouteOptimizationResponse.RouteStep step = new RouteOptimizationResponse.RouteStep();
            Map<String, Object> endLoc = (Map<String, Object>) leg.get("end_location");
            step.lat = endLoc != null && endLoc.get("lat") != null ? ((Number) endLoc.get("lat")).doubleValue() : 0;
            step.lng = endLoc != null && endLoc.get("lng") != null ? ((Number) endLoc.get("lng")).doubleValue() : 0;
            step.address = (String) leg.getOrDefault("end_address", "");
            Object distanceObj = leg.get("distance");
            Object durationObj = leg.get("duration");
            step.distance = (distanceObj instanceof Map && ((Map<?, ?>) distanceObj).get("value") instanceof Number)
                    ? ((Number) ((Map<?, ?>) distanceObj).get("value")).doubleValue() : 0;
            step.duration = (durationObj instanceof Map && ((Map<?, ?>) durationObj).get("value") instanceof Number)
                    ? ((Number) ((Map<?, ?>) durationObj).get("value")).doubleValue() : 0;
            totalDistance += step.distance;
            totalDuration += step.duration;
            steps.add(step);
        }

        RouteOptimizationResponse result = new RouteOptimizationResponse();
        result.steps = steps;
        result.totalDistance = totalDistance;
        result.totalDuration = totalDuration;
        return result;
    }
}