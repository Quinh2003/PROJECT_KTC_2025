package ktc.spring_project.repositories;

import ktc.spring_project.entities.Payment;
import ktc.spring_project.enums.PaymentMethod;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface PaymentRepository extends JpaRepository<Payment, Long> {
    
    List<Payment> findByOrderId(Long orderId);
    
    List<Payment> findByPaymentMethod(PaymentMethod paymentMethod);
    
    List<Payment> findByStatusId(Short statusId);
    
    Optional<Payment> findByTransactionId(String transactionId);
    
    @Query("SELECT p FROM Payment p WHERE p.order.id = :orderId")
List<Payment> findByOrderIdUsingQuery(@Param("orderId") Long orderId);

    
    @Query("SELECT p FROM Payment p WHERE p.status.name = :statusName ORDER BY p.createdAt DESC")
    List<Payment> findByStatusNameOrderByCreatedAtDesc(@Param("statusName") String statusName);
    
    @Query("SELECT p FROM Payment p WHERE p.createdAt BETWEEN :startDate AND :endDate ORDER BY p.createdAt DESC")
    List<Payment> findPaymentsBetweenDates(@Param("startDate") LocalDateTime startDate, 
                                         @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT p FROM Payment p WHERE p.amount BETWEEN :minAmount AND :maxAmount")
    List<Payment> findByAmountRange(@Param("minAmount") BigDecimal minAmount, 
                                  @Param("maxAmount") BigDecimal maxAmount);
    
    @Query("SELECT SUM(p.amount) FROM Payment p WHERE p.status.name = 'SUCCESS' " +
           "AND p.createdAt BETWEEN :startDate AND :endDate")
    BigDecimal getTotalSuccessfulPaymentsInPeriod(@Param("startDate") LocalDateTime startDate, 
                                                @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT COUNT(p) FROM Payment p WHERE p.paymentMethod = :method AND p.status.name = 'SUCCESS'")
    long countSuccessfulPaymentsByMethod(@Param("method") PaymentMethod method);
    
    @Query("SELECT COUNT(p) FROM Payment p WHERE DATE(p.createdAt) = CURRENT_DATE")
    long countTodayPayments();
    
    @Query("SELECT AVG(p.amount) FROM Payment p WHERE p.status.name = 'SUCCESS'")
    BigDecimal getAverageSuccessfulPaymentAmount();
}

