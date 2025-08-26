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

    public List<Payment> findAll() {
        return paymentRepository.findAll();
    }

    public Optional<Payment> findById(Long id) {
        return paymentRepository.findById(id);
    }

    public Payment save(Payment entities) {
        return paymentRepository.save(entities);
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
