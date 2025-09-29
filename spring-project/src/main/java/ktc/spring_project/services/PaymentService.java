package ktc.spring_project.services;

import ktc.spring_project.entities.Payment;
import ktc.spring_project.repositories.PaymentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class PaymentService {

    @Autowired
    private PaymentRepository paymentRepository;
    
    @Autowired
    private ChecklistService checklistService;

    public List<Payment> findAll() {
        return paymentRepository.findAll();
    }

    public Optional<Payment> findById(Long id) {
        return paymentRepository.findById(id);
    }

    public Payment save(Payment payment) {
        Payment savedPayment = paymentRepository.save(payment);
        
        // ✅ Log checklist step: Customer đã thanh toán
        try {
            if (payment.getOrder() != null && payment.getOrder().getCreatedBy() != null && 
                payment.getStatus() != null && 
                ("Completed".equals(payment.getStatus().getName()) || "Success".equals(payment.getStatus().getName()))) {
                
                checklistService.markStepCompleted(
                    payment.getOrder().getCreatedBy().getId(), 
                    payment.getOrder().getId(),
                    "CUSTOMER_PAYMENT", 
                    "Payment completed for Order: " + payment.getOrder().getId() + " - Amount: " + payment.getAmount()
                );
            }
        } catch (Exception e) {
            // Log warning but don't break payment flow
            System.err.println("Failed to log checklist step CUSTOMER_PAYMENT: " + e.getMessage());
        }
        
        return savedPayment;
    }

    public void delete(Long id) {
        paymentRepository.deleteById(id);
    }
    // ...existing code...
public void deleteById(Long id) {
    paymentRepository.deleteById(id);
}
// ...existing code...
}
