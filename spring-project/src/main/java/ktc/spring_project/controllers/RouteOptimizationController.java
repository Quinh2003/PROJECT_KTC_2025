package ktc.spring_project.controllers;

import ktc.spring_project.dtos.RouteOptimization.*;
import ktc.spring_project.services.RouteOptimizationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/route")
public class RouteOptimizationController {

    @Autowired
    private RouteOptimizationService routeOptimizationService;

    @PostMapping("/optimize")
    public RouteOptimizationResponse optimizeRoute(@RequestBody RouteOptimizationRequest request) {
        return routeOptimizationService.optimizeRoute(request);
    }
}
