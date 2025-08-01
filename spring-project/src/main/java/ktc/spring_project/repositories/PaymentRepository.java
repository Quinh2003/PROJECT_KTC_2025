package ktc.spring_project.repositories;

import ktc.spring_project.entities.Payment;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.User;
import ktc.spring_project.enums.PaymentMethod;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repository for Payment entity
 * Handles payment processing and financial tracking
 */
@Repository
public interface PaymentRepository extends BaseRepository<Payment, Long> {
    
    /**
     * Find payments by order
     */
    List<Payment> findByOrderOrderByCreatedAtDesc(@Param("order") Order order);
    
    /**
     * Find payment by transaction ID
     */
    Optional<Payment> findByTransactionId(@Param("transactionId") String transactionId);
    
    /**
     * Find payments by payment method
     */
    List<Payment> findByPaymentMethodOrderByCreatedAtDesc(@Param("paymentMethod") PaymentMethod paymentMethod);
    
    /**
     * Find payments by status code
     */
    @Query("SELECT p FROM Payment p WHERE p.status.code = :statusCode ORDER BY p.createdAt DESC")
    List<Payment> findByStatusCode(@Param("statusCode") String statusCode);
    
    /**
     * Find successful payments
     */
    @Query("SELECT p FROM Payment p WHERE p.status.code = 'COMPLETED' ORDER BY p.createdAt DESC")
    Page<Payment> findSuccessfulPayments(Pageable pageable);
    
    /**
     * Find pending payments
     */
    @Query("SELECT p FROM Payment p WHERE p.status.code = 'PENDING' ORDER BY p.createdAt ASC")
    List<Payment> findPendingPayments();
    
    /**
     * Find failed payments
     */
    @Query("SELECT p FROM Payment p WHERE p.status.code = 'FAILED' ORDER BY p.createdAt DESC")
    List<Payment> findFailedPayments();
    
    /**
     * Calculate total revenue by date range
     */
    @Query("SELECT SUM(p.amount) FROM Payment p WHERE p.status.code = 'COMPLETED' " +
           "AND p.createdAt BETWEEN :startDate AND :endDate")
    BigDecimal calculateRevenue(@Param("startDate") LocalDateTime startDate, 
                               @Param("endDate") LocalDateTime endDate);
    
    /**
     * Get revenue by payment method
     */
    @Query("SELECT p.paymentMethod, SUM(p.amount) as totalAmount, COUNT(p) as paymentCount " +
           "FROM Payment p WHERE p.status.code = 'COMPLETED' " +
           "AND p.createdAt BETWEEN :startDate AND :endDate " +
           "GROUP BY p.paymentMethod ORDER BY totalAmount DESC")
    List<Object[]> getRevenueByPaymentMethod(@Param("startDate") LocalDateTime startDate, 
                                            @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find payments by creator (for tracking who processed payments)
     */
    List<Payment> findByCreatedByOrderByCreatedAtDesc(@Param("createdBy") User createdBy);
    
    /**
     * Find payments within amount range
     */
    @Query("SELECT p FROM Payment p WHERE p.amount BETWEEN :minAmount AND :maxAmount " +
           "ORDER BY p.amount DESC")
    List<Payment> findByAmountRange(@Param("minAmount") BigDecimal minAmount, 
                                   @Param("maxAmount") BigDecimal maxAmount);
    
    /**
     * Find high-value payments (above threshold)
     */
    @Query("SELECT p FROM Payment p WHERE p.amount >= :threshold " +
           "AND p.status.code = 'COMPLETED' ORDER BY p.amount DESC")
    List<Payment> findHighValuePayments(@Param("threshold") BigDecimal threshold);
    
    /**
     * Get payment statistics by date range
     */
    @Query("SELECT p.status.code, COUNT(p) as count, SUM(p.amount) as totalAmount, " +
           "AVG(p.amount) as avgAmount FROM Payment p " +
           "WHERE p.createdAt BETWEEN :startDate AND :endDate " +
           "GROUP BY p.status.code ORDER BY p.status.code")
    List<Object[]> getPaymentStatistics(@Param("startDate") LocalDateTime startDate, 
                                       @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find online payments (Stripe, Credit Card)
     */
    @Query("SELECT p FROM Payment p WHERE p.paymentMethod IN ('STRIPE', 'CREDIT_CARD') " +
           "ORDER BY p.createdAt DESC")
    List<Payment> findOnlinePayments();
    
    /**
     * Find cash payments
     */
    @Query("SELECT p FROM Payment p WHERE p.paymentMethod = 'CASH' " +
           "ORDER BY p.createdAt DESC")
    List<Payment> findCashPayments();
    
    /**
     * Calculate average payment amount
     */
    @Query("SELECT AVG(p.amount) FROM Payment p WHERE p.status.code = 'COMPLETED' " +
           "AND p.createdAt BETWEEN :startDate AND :endDate")
    BigDecimal calculateAveragePaymentAmount(@Param("startDate") LocalDateTime startDate, 
                                            @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find payments requiring refund (if business supports refunds)
     */
    @Query("SELECT p FROM Payment p WHERE p.status.code = 'REFUND_REQUESTED' " +
           "ORDER BY p.createdAt ASC")
    List<Payment> findPaymentsRequiringRefund();
    
    /**
     * Count payments by method and date range
     */
    @Query("SELECT COUNT(p) FROM Payment p WHERE p.paymentMethod = :paymentMethod " +
           "AND p.createdAt BETWEEN :startDate AND :endDate")
    Long countByMethodAndDateRange(@Param("paymentMethod") PaymentMethod paymentMethod,
                                  @Param("startDate") LocalDateTime startDate,
                                  @Param("endDate") LocalDateTime endDate);
}