package ktc.spring_project.repositories;

import ktc.spring_project.entities.DeliveryProof;
import ktc.spring_project.enums.ProofType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface DeliveryProofRepository extends JpaRepository<DeliveryProof, Long> {
    
    List<DeliveryProof> findByOrderId(Long orderId);
    
    List<DeliveryProof> findByProofType(ProofType proofType);
    
    List<DeliveryProof> findByUploadedById(Long uploadedById);
    
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.capturedAt BETWEEN :startDate AND :endDate")
    List<DeliveryProof> findByCapturedDateRange(@Param("startDate") LocalDateTime startDate, 
                                               @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.order.id = :orderId AND dp.proofType = :proofType")
    List<DeliveryProof> findByOrderIdAndProofType(@Param("orderId") Long orderId, @Param("proofType") ProofType proofType);
}
