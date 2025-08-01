package ktc.spring_project.services.interfaces;

import ktc.spring_project.dtos.payment.CreatePaymentRequestDTO;
import ktc.spring_project.dtos.payment.PaymentResponseDTO;
import ktc.spring_project.dtos.common.PagedResponse;
import ktc.spring_project.entities.Payment;
import ktc.spring_project.enums.PaymentMethod;
import org.springframework.data.domain.Pageable;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Service interface for Payment processing and management
 * Handles payment lifecycle, financial tracking, and reporting
 */
public interface PaymentService {
    
    // ===== PAYMENT PROCESSING =====
    /**
     * Process payment for an order
     * @param createPaymentDTO Payment creation data
     * @param processedBy User who processes the payment
     * @return Created payment response
     */
    PaymentResponseDTO processPayment(CreatePaymentRequestDTO createPaymentDTO, Long processedBy);
    
    /**
     * Process Stripe payment with payment intent
     * @param orderId Order ID
     * @param amount Payment amount
     * @param paymentIntentId Stripe payment intent ID
     * @param processedBy User who processes the payment
     * @return Created payment response
     */
    PaymentResponseDTO processStripePayment(Long orderId, BigDecimal amount, String paymentIntentId, Long processedBy);
    
    /**
     * Process cash payment
     * @param orderId Order ID
     * @param amount Payment amount
     * @param processedBy User who processes the payment
     * @return Created payment response
     */
    PaymentResponseDTO processCashPayment(Long orderId, BigDecimal amount, Long processedBy);
    
    /**
     * Process bank transfer payment
     * @param orderId Order ID
     * @param amount Payment amount
     * @param transactionId Bank transaction ID
     * @param processedBy User who processes the payment
     * @return Created payment response
     */
    PaymentResponseDTO processBankTransferPayment(Long orderId, BigDecimal amount, String transactionId, Long processedBy);
    
    // ===== PAYMENT STATUS MANAGEMENT =====
    /**
     * Update payment status
     * @param paymentId Payment ID
     * @param statusCode New status code (PENDING, COMPLETED, FAILED, CANCELLED)
     * @param updatedBy User who updates the status
     * @return Updated payment response
     */
    PaymentResponseDTO updatePaymentStatus(Long paymentId, String statusCode, Long updatedBy);
    
    /**
     * Mark payment as completed
     * @param paymentId Payment ID
     * @param completedBy User who marks as completed
     * @return Updated payment response
     */
    PaymentResponseDTO markAsCompleted(Long paymentId, Long completedBy);
    
    /**
     * Mark payment as failed
     * @param paymentId Payment ID
     * @param failedBy User who marks as failed
     * @param reason Failure reason
     * @return Updated payment response
     */
    PaymentResponseDTO markAsFailed(Long paymentId, Long failedBy, String reason);
    
    // ===== PAYMENT QUERIES =====
    /**
     * Get payment by ID
     * @param paymentId Payment ID
     * @return Payment response or empty if not found
     */
    Optional<PaymentResponseDTO> getPaymentById(Long paymentId);
    
    /**
     * Get payment entity by ID (for internal service use)
     * @param paymentId Payment ID
     * @return Payment entity or empty if not found
     */
    Optional<Payment> getPaymentEntityById(Long paymentId);
    
    /**
     * Get payment by transaction ID
     * @param transactionId Transaction ID
     * @return Payment response or empty if not found
     */
    Optional<PaymentResponseDTO> getPaymentByTransactionId(String transactionId);
    
    /**
     * Get payments by order
     * @param orderId Order ID
     * @return List of payments for the order
     */
    List<PaymentResponseDTO> getPaymentsByOrder(Long orderId);
    
    /**
     * Get all payments with pagination
     * @param pageable Pagination parameters
     * @return Paged payment responses
     */
    PagedResponse<PaymentResponseDTO> getAllPayments(Pageable pageable);
    
    /**
     * Get payments by payment method
     * @param paymentMethod Payment method
     * @param pageable Pagination parameters
     * @return Paged payments with specified method
     */
    PagedResponse<PaymentResponseDTO> getPaymentsByMethod(PaymentMethod paymentMethod, Pageable pageable);
    
    /**
     * Get payments by status
     * @param statusCode Status code
     * @param pageable Pagination parameters
     * @return Paged payments with specified status
     */
    PagedResponse<PaymentResponseDTO> getPaymentsByStatus(String statusCode, Pageable pageable);
    
    // ===== BUSINESS QUERIES =====
    /**
     * Get successful payments
     * @param pageable Pagination parameters
     * @return Paged successful payments
     */
    PagedResponse<PaymentResponseDTO> getSuccessfulPayments(Pageable pageable);
    
    /**
     * Get pending payments
     * @param pageable Pagination parameters
     * @return Paged pending payments
     */
    PagedResponse<PaymentResponseDTO> getPendingPayments(Pageable pageable);
    
    /**
     * Get failed payments
     * @param pageable Pagination parameters
     * @return Paged failed payments
     */
    PagedResponse<PaymentResponseDTO> getFailedPayments(Pageable pageable);
    
    /**
     * Get high-value payments (above threshold)
     * @param threshold Amount threshold
     * @param pageable Pagination parameters
     * @return Paged high-value payments
     */
    PagedResponse<PaymentResponseDTO> getHighValuePayments(BigDecimal threshold, Pageable pageable);
    
    /**
     * Get online payments (non-cash)
     * @param pageable Pagination parameters
     * @return Paged online payments
     */
    PagedResponse<PaymentResponseDTO> getOnlinePayments(Pageable pageable);
    
    /**
     * Get cash payments
     * @param pageable Pagination parameters
     * @return Paged cash payments
     */
    PagedResponse<PaymentResponseDTO> getCashPayments(Pageable pageable);
    
    /**
     * Get payments within date range
     * @param startDate Start date
     * @param endDate End date
     * @param pageable Pagination parameters
     * @return Paged payments within date range
     */
    PagedResponse<PaymentResponseDTO> getPaymentsByDateRange(LocalDateTime startDate, LocalDateTime endDate, Pageable pageable);
    
    // ===== FINANCIAL ANALYTICS =====
    /**
     * Calculate total revenue for date range
     * @param startDate Start date
     * @param endDate End date
     * @return Total revenue amount
     */
    BigDecimal calculateRevenue(LocalDateTime startDate, LocalDateTime endDate);
    
    /**
     * Calculate revenue by payment method
     * @param startDate Start date
     * @param endDate End date
     * @return Map of revenue by payment method
     */
    java.util.Map<PaymentMethod, BigDecimal> getRevenueByPaymentMethod(LocalDateTime startDate, LocalDateTime endDate);
    
    /**
     * Calculate average payment amount
     * @param startDate Start date
     * @param endDate End date
     * @return Average payment amount
     */
    BigDecimal calculateAveragePaymentAmount(LocalDateTime startDate, LocalDateTime endDate);
    
    /**
     * Get payment count by status
     * @return Map of payment counts by status
     */
    java.util.Map<String, Long> getPaymentCountByStatus();
    
    /**
     * Get daily revenue for date range
     * @param startDate Start date
     * @param endDate End date
     * @return List of daily revenue data
     */
    List<java.util.Map<String, Object>> getDailyRevenue(LocalDateTime startDate, LocalDateTime endDate);
    
    /**
     * Get monthly revenue for year
     * @param year Year
     * @return List of monthly revenue data
     */
    List<java.util.Map<String, Object>> getMonthlyRevenue(int year);
    
    // ===== PAYMENT STATISTICS =====
    /**
     * Get payment statistics
     * @return Map of payment statistics (total, by method, by status, etc.)
     */
    java.util.Map<String, Object> getPaymentStatistics();
    
    /**
     * Get success rate for date range
     * @param startDate Start date
     * @param endDate End date
     * @return Payment success rate percentage
     */
    BigDecimal getSuccessRate(LocalDateTime startDate, LocalDateTime endDate);
    
    /**
     * Get top payment amounts
     * @param limit Number of top payments to return
     * @return List of highest payment amounts
     */
    List<PaymentResponseDTO> getTopPayments(int limit);
    
    // ===== VALIDATION =====
    /**
     * Validate payment amount against order total
     * @param orderId Order ID
     * @param paymentAmount Payment amount
     * @return true if payment amount matches order total
     */
    boolean validatePaymentAmount(Long orderId, BigDecimal paymentAmount);
    
    /**
     * Check if order is already paid
     * @param orderId Order ID
     * @return true if order has successful payment
     */
    boolean isOrderPaid(Long orderId);
    
    /**
     * Get total paid amount for order
     * @param orderId Order ID
     * @return Total amount paid for the order
     */
    BigDecimal getTotalPaidAmount(Long orderId);
}