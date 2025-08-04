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
    
    Optional<Payment> findByTransactionId(String transactionId);
    
    List<Payment> findByStatusId(Long statusId);
    
    @Query("SELECT p FROM Payment p WHERE p.createdAt BETWEEN :startDate AND :endDate")
    List<Payment> findByDateRange(@Param("startDate") LocalDateTime startDate, 
                                 @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT SUM(p.amount) FROM Payment p WHERE p.status.code = 'COMPLETED'")
    BigDecimal getTotalCompletedPayments();
    
    @Query("SELECT p FROM Payment p WHERE p.status.code = :statusCode")
    List<Payment> findByStatusCode(@Param("statusCode") String statusCode);
}
