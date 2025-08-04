package ktc.spring_project.controllers;

import ktc.spring_project.entities.Payment;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * Controller responsible for managing payments
 * Based on database schema for payments table
 */
@RestController
@RequestMapping("/api/payments")
public class PaymentController {

    // Assume these services are implemented later
    @Autowired
    private PaymentService paymentService;

    @Autowired
    private UserService userService;

    /**
     * Get all payments with optional filters
     */
    @GetMapping
    public ResponseEntity<List<Payment>> getAllPayments(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String paymentMethod,
            @RequestParam(required = false) Long orderId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        List<Payment> payments = paymentService.getFilteredPayments(
                status, paymentMethod, orderId, dateFrom, dateTo);

        return ResponseEntity.ok(payments);
    }

    /**
     * Get payment by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<Payment> getPaymentById(@PathVariable Long id) {
        Payment payment = paymentService.getPaymentById(id);
        return ResponseEntity.ok(payment);
    }

    /**
     * Create a new payment
     */
    @PostMapping
    public ResponseEntity<Payment> createPayment(
            @Valid @RequestBody Payment payment,
            Authentication authentication) {

        Payment createdPayment = paymentService.createPayment(payment, authentication);
        return new ResponseEntity<>(createdPayment, HttpStatus.CREATED);
    }

    /**
     * Update payment information
     */
    @PutMapping("/{id}")
    public ResponseEntity<Payment> updatePayment(
            @PathVariable Long id,
            @Valid @RequestBody Payment payment,
            Authentication authentication) {

        Payment updatedPayment = paymentService.updatePayment(id, payment, authentication);
        return ResponseEntity.ok(updatedPayment);
    }

    /**
     * Update payment status
     */
    @PatchMapping("/{id}/status")
    public ResponseEntity<Payment> updatePaymentStatus(
            @PathVariable Long id,
            @RequestBody Map<String, Long> statusData,
            Authentication authentication) {

        Long statusId = statusData.get("statusId");

        Payment updatedPayment = paymentService.updatePaymentStatus(id, statusId, authentication);
        return ResponseEntity.ok(updatedPayment);
    }

    /**
     * Get payments for an order
     */
    @GetMapping("/order/{orderId}")
    public ResponseEntity<List<Payment>> getPaymentsByOrder(@PathVariable Long orderId) {
        List<Payment> payments = paymentService.getPaymentsByOrder(orderId);
        return ResponseEntity.ok(payments);
    }

    /**
     * Get payment statistics
     */
    @GetMapping("/statistics")
    public ResponseEntity<Map<String, Object>> getPaymentStatistics(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String groupBy) {

        Map<String, Object> statistics = paymentService.getPaymentStatistics(dateFrom, dateTo, groupBy);
        return ResponseEntity.ok(statistics);
    }

    /**
     * Process refund for a payment
     */
    @PostMapping("/{id}/refund")
    public ResponseEntity<Payment> processRefund(
            @PathVariable Long id,
            @RequestBody Map<String, Object> refundData,
            Authentication authentication) {

        String reason = (String) refundData.get("reason");
        Double amount = (Double) refundData.get("amount");

        Payment refundedPayment = paymentService.processRefund(id, amount, reason, authentication);
        return ResponseEntity.ok(refundedPayment);
    }

    /**
     * Verify payment with external provider
     */
    @PostMapping("/{id}/verify")
    public ResponseEntity<Map<String, Object>> verifyPayment(
            @PathVariable Long id,
            Authentication authentication) {

        Map<String, Object> verificationResult = paymentService.verifyPayment(id, authentication);
        return ResponseEntity.ok(verificationResult);
    }
}
