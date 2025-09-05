package ktc.spring_project.controllers;

import ktc.spring_project.entities.Payment;
import ktc.spring_project.repositories.PaymentRepository;
import ktc.spring_project.repositories.StatusRepository;
import ktc.spring_project.services.PaymentService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import ktc.spring_project.entities.Status;
import ktc.spring_project.enums.StatusType;
import ktc.spring_project.dtos.payment.CreatePaymentRequestDTO;
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

     @Autowired
    private PaymentRepository paymentRepository;

    @Autowired
private StatusRepository statusRepository;
    /**
     * 
     * Get all payments with optional filters
     */
    @GetMapping
    public ResponseEntity<List<Payment>> getAllPayments(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String paymentMethod,
            @RequestParam(required = false) Long orderId,
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo) {

        // Lấy tất cả các thanh toán
        List<Payment> allPayments = paymentService.findAll();

        // Trong thực tế, sẽ lọc danh sách thanh toán theo các tiêu chí
        // Đây là giải pháp tạm thời cho đến khi service được cập nhật đầy đủ

        return ResponseEntity.ok(allPayments);
    }

    /**
     * Get payment by ID
     */
      @GetMapping("/{id}")
    public ResponseEntity<Payment> getPaymentById(@PathVariable Long id) {
        return paymentService.findById(id)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }
@DeleteMapping("/{id}")
public ResponseEntity<Void> deletePayment(@PathVariable Long id) {
    if (paymentService.findById(id).isPresent()) {
        paymentService.deleteById(id);
        return ResponseEntity.noContent().build();
    } else {
        return ResponseEntity.notFound().build();
    }
}
    /**
     * Create a new payment
     */
    @PostMapping
    public ResponseEntity<Payment> createPayment(
            @Valid @RequestBody CreatePaymentRequestDTO dto,
            Authentication authentication) {

        // Map DTO to Payment entity
        Payment payment = new Payment();
        payment.setAmount(dto.getAmount());
        payment.setPaymentMethod(dto.getPaymentMethod());
        payment.setTransactionId(dto.getTransactionId());
        payment.setNotes(dto.getNotes());
        // Map order, status, createdBy nếu cần (giả sử có các repository/service)
        if (dto.getOrderId() != null) {
            payment.setOrder(new ktc.spring_project.entities.Order());
            payment.getOrder().setId(dto.getOrderId());
        }
        if (dto.getStatusId() != null) {
            payment.setStatus(new Status());
            payment.getStatus().setId(dto.getStatusId().shortValue());
        }
    // Nếu cần set createdBy, hãy implement logic lấy User từ userId ở service/repository

        Payment createdPayment = paymentService.save(payment);
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

    return paymentService.findById(id)
            .map(existingPayment -> {
                payment.setId(id);
                Payment updatedPayment = paymentService.save(payment);
                return ResponseEntity.ok(updatedPayment);
            })
            .orElse(ResponseEntity.notFound().build());
}
    /**
     * Update payment status
     *}
    /**
     * Get payments for an order
     */
    @GetMapping("/order/{orderId}")
    public ResponseEntity<List<Payment>> getPaymentsByOrder(@PathVariable Long orderId) {
        // Lấy tất cả thanh toán
        List<Payment> allPayments = paymentService.findAll();

        // Lọc thanh toán theo orderId
        List<Payment> orderPayments = allPayments.stream()
                .filter(payment -> payment.getOrder() != null && payment.getOrder().getId().equals(orderId))
                .collect(java.util.stream.Collectors.toList());

        return ResponseEntity.ok(orderPayments);
    }

    /**
     * Get payment statistics
     */
    @GetMapping("/statistics")
    public ResponseEntity<Map<String, Object>> getPaymentStatistics(
            @RequestParam(required = false) String dateFrom,
            @RequestParam(required = false) String dateTo,
            @RequestParam(required = false) String groupBy) {

        // Tạo thống kê giả tạm thời
        Map<String, Object> statistics = new java.util.HashMap<>();
        statistics.put("totalPayments", 0);
        statistics.put("totalAmount", 0.0);
        statistics.put("successfulPayments", 0);
        statistics.put("failedPayments", 0);

        // Trong thực tế, sẽ tính toán thống kê từ danh sách thanh toán

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

        // Kiểm tra xem thanh toán có tồn tại không
        return paymentService.findById(id)
                .map(payment -> {
                    // Trong thực tế, sẽ xử lý hoàn tiền và cập nhật thanh toán
                    // Đây là giải pháp tạm thời

                    return ResponseEntity.ok(payment);
                })
                .orElse(ResponseEntity.notFound().build());
    }

    /**
     * Verify payment with external provider
     */
    @PostMapping("/{id}/verify")
    public ResponseEntity<Map<String, Object>> verifyPayment(
            @PathVariable Long id,
            Authentication authentication) {

        // Kiểm tra xem thanh toán có tồn tại không
        return paymentService.findById(id)
                .map(payment -> {
                    // Tạo kết quả xác minh giả
                    Map<String, Object> verificationResult = new java.util.HashMap<>();
                    verificationResult.put("paymentId", id);
                    verificationResult.put("verified", true);
                    verificationResult.put("message", "Payment verified successfully");

                    // Trong thực tế, sẽ gọi dịch vụ bên ngoài để xác minh thanh toán

                    return ResponseEntity.ok(verificationResult);
                })
                .orElse(ResponseEntity.notFound().build());
    }
}
