package ktc.spring_project.controllers;

import ktc.spring_project.services.OrderService;
import ktc.spring_project.services.DeliveryService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

/**
 * Controller responsible for generating reports and statistics
 * Based on user stories for analytics and reporting
 */
@RestController
@RequestMapping("/api/reports")
public class ReportController {

    @Autowired
    private OrderService orderService;

    @Autowired
    private DeliveryService deliveryService;

    @Autowired
    private UserService userService;

    /**
     * Get daily delivery summary report
     */
    @GetMapping("/daily-delivery-summary")
    public ResponseEntity<Map<String, Object>> getDailyDeliverySummary(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date) {

        Map<String, Object> report = orderService.getDailyDeliverySummary(date);
        return ResponseEntity.ok(report);
    }

    /**
     * Get driver performance report
     */
    @GetMapping("/driver-performance")
    public ResponseEntity<List<Map<String, Object>>> getDriverPerformanceReport(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(required = false) Long driverId) {

        List<Map<String, Object>> report = orderService.getDriverPerformanceReport(dateFrom, dateTo, driverId);
        return ResponseEntity.ok(report);
    }

    /**
     * Get vehicle utilization report
     */
    @GetMapping("/vehicle-utilization")
    public ResponseEntity<List<Map<String, Object>>> getVehicleUtilizationReport(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(required = false) Long vehicleId) {

        List<Map<String, Object>> report = orderService.getVehicleUtilizationReport(dateFrom, dateTo, vehicleId);
        return ResponseEntity.ok(report);
    }

    /**
     * Get on-time delivery report
     */
    @GetMapping("/on-time-delivery")
    public ResponseEntity<Map<String, Object>> getOnTimeDeliveryReport(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {

        Map<String, Object> report = orderService.getOnTimeDeliveryReport(dateFrom, dateTo);
        return ResponseEntity.ok(report);
    }

    /**
     * Get revenue by service type report
     */
    @GetMapping("/revenue-by-service")
    public ResponseEntity<List<Map<String, Object>>> getRevenueByServiceReport(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {

        List<Map<String, Object>> report = orderService.getRevenueByServiceReport(dateFrom, dateTo);
        return ResponseEntity.ok(report);
    }

    /**
     * Get customer delivery frequency report
     */
    @GetMapping("/customer-frequency")
    public ResponseEntity<List<Map<String, Object>>> getCustomerFrequencyReport(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(defaultValue = "10") int limit) {

        List<Map<String, Object>> report = orderService.getCustomerFrequencyReport(dateFrom, dateTo, limit);
        return ResponseEntity.ok(report);
    }

    /**
     * Export report as file (Excel, CSV, PDF)
     */
    @GetMapping("/export/{reportType}")
    public ResponseEntity<byte[]> exportReport(
            @PathVariable String reportType,
            @RequestParam String format,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            Authentication authentication) {

        byte[] reportContent = orderService.generateReportFile(reportType, format, dateFrom, dateTo, authentication);

        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=" + reportType + "." + format.toLowerCase());

        // Set appropriate content type based on format
        String contentType = "application/octet-stream";
        if ("PDF".equalsIgnoreCase(format)) {
            contentType = "application/pdf";
        } else if ("EXCEL".equalsIgnoreCase(format) || "XLS".equalsIgnoreCase(format)) {
            contentType = "application/vnd.ms-excel";
        } else if ("XLSX".equalsIgnoreCase(format)) {
            contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
        } else if ("CSV".equalsIgnoreCase(format)) {
            contentType = "text/csv";
        }
        headers.add("Content-Type", contentType);

        return ResponseEntity.ok()
                .headers(headers)
                .body(reportContent);
    }

    /**
     * Get delivery time analysis by region
     */
    @GetMapping("/delivery-time-by-region")
    public ResponseEntity<List<Map<String, Object>>> getDeliveryTimeByRegion(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {

        List<Map<String, Object>> report = orderService.getDeliveryTimeByRegion(dateFrom, dateTo);
        return ResponseEntity.ok(report);
    }

    /**
     * Get operational cost analysis
     */
    @GetMapping("/operational-cost")
    public ResponseEntity<Map<String, Object>> getOperationalCostAnalysis(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(required = false) String costType) {

        Map<String, Object> report = orderService.getOperationalCostAnalysis(dateFrom, dateTo, costType);
        return ResponseEntity.ok(report);
    }
}
