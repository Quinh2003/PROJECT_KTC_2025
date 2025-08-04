package ktc.spring_project.controllers;

import ktc.spring_project.services.KPIService;
import ktc.spring_project.services.AnalyticsService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * Controller responsible for dashboard and analytics
 * Based on user stories:
 * - US-OPS-KPI-01: View Real-Time KPIs
 * - US-OPS-ANALYTICS-01: Analyze Route & Driver Performance
 * - US-OPS-EXPORT-01: Export Reports & Data Files
 */
@RestController
@RequestMapping("/api/dashboard")
public class DashboardController {

    @Autowired
    private KPIService kpiService;

    @Autowired
    private AnalyticsService analyticsService;

    @Autowired
    private UserService userService;

    /**
     * Get real-time KPIs
     * US-OPS-KPI-01
     */
    @GetMapping("/kpis")
    public ResponseEntity<Map<String, Object>> getRealTimeKPIs(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String period) {

        Map<String, Object> kpis = kpiService.getRealTimeKPIs(dateFrom, dateTo, period);
        return ResponseEntity.ok(kpis);
    }

    /**
     * Get delivery performance metrics
     * US-OPS-KPI-01
     */
    @GetMapping("/delivery-performance")
    public ResponseEntity<Map<String, Object>> getDeliveryPerformance(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        Map<String, Object> performance = kpiService.getDeliveryPerformanceMetrics(dateFrom, dateTo);
        return ResponseEntity.ok(performance);
    }

    /**
     * Get driver performance analytics
     * US-OPS-ANALYTICS-01
     */
    @GetMapping("/driver-analytics")
    public ResponseEntity<List<Map<String, Object>>> getDriverAnalytics(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) Long driverId) {

        List<Map<String, Object>> analytics = analyticsService.getDriverPerformanceAnalytics(
                dateFrom, dateTo, driverId);
        return ResponseEntity.ok(analytics);
    }

    /**
     * Get route performance analytics
     * US-OPS-ANALYTICS-01
     */
    @GetMapping("/route-analytics")
    public ResponseEntity<List<Map<String, Object>>> getRouteAnalytics(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) Long routeId) {

        List<Map<String, Object>> analytics = analyticsService.getRoutePerformanceAnalytics(
                dateFrom, dateTo, routeId);
        return ResponseEntity.ok(analytics);
    }

    /**
     * Get vehicle utilization metrics
     */
    @GetMapping("/vehicle-utilization")
    public ResponseEntity<List<Map<String, Object>>> getVehicleUtilization(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) Long vehicleId) {

        List<Map<String, Object>> utilization = analyticsService.getVehicleUtilization(
                dateFrom, dateTo, vehicleId);
        return ResponseEntity.ok(utilization);
    }

    /**
     * Export dashboard reports
     * US-OPS-EXPORT-01
     */
    @PostMapping("/export")
    public ResponseEntity<byte[]> exportReport(
            @RequestBody Map<String, Object> exportRequest,
            Authentication authentication) {

        String reportType = (String) exportRequest.get("reportType");
        String format = (String) exportRequest.get("format"); // Excel, CSV, PDF
        String dateFrom = (String) exportRequest.get("dateFrom");
        String dateTo = (String) exportRequest.get("dateTo");

        byte[] reportData = analyticsService.exportReport(reportType, format, dateFrom, dateTo);

        return ResponseEntity.ok()
                .header("Content-Disposition", "attachment; filename=report." + format.toLowerCase())
                .body(reportData);
    }

    /**
     * Get summary statistics for dashboard overview
     */
    @GetMapping("/summary")
    public ResponseEntity<Map<String, Object>> getDashboardSummary(
            @RequestParam(required = false) String period) {

        Map<String, Object> summary = kpiService.getDashboardSummary(period);
        return ResponseEntity.ok(summary);
    }

    /**
     * Get chart data for dashboard visualization
     */
    @GetMapping("/charts/{chartType}")
    public ResponseEntity<Map<String, Object>> getChartData(
            @PathVariable String chartType,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String groupBy) {

        Map<String, Object> chartData = analyticsService.getChartData(chartType, dateFrom, dateTo, groupBy);
        return ResponseEntity.ok(chartData);
    }
}
