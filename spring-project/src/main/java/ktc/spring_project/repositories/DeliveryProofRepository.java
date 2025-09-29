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
    
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.order.id = :orderId")
List<DeliveryProof> findByOrderOrderId(@Param("orderId") Long orderId);

    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.capturedAt BETWEEN :startDate AND :endDate ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findProofsCapturedBetween(@Param("startDate") LocalDateTime startDate, 
                                                @Param("endDate") LocalDateTime endDate);
    
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.uploadedBy.id = :userId ORDER BY dp.createdAt DESC")
    List<DeliveryProof> findByUploadedByOrderByCreatedAtDesc(@Param("userId") Long userId);
    
    @Query("SELECT COUNT(dp) FROM DeliveryProof dp WHERE dp.proofType = :proofType")
    long countByProofType(@Param("proofType") ProofType proofType);
    
    @Query("SELECT COUNT(dp) FROM DeliveryProof dp WHERE dp.order.id = :orderId")
    long countProofsByOrderId(@Param("orderId") Long orderId);
    
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.filePath IS NOT NULL AND dp.filePath != ''")
    List<DeliveryProof> findProofsWithFiles();
}

