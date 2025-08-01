package ktc.spring_project.repositories;

import ktc.spring_project.entities.DeliveryProof;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.User;
import ktc.spring_project.enums.ProofType;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repository for DeliveryProof entity
 * Handles delivery confirmation and proof management
 */
@Repository
public interface DeliveryProofRepository extends BaseRepository<DeliveryProof, Long> {
    
    /**
     * Find proofs by order
     */
    List<DeliveryProof> findByOrderOrderByCapturedAtDesc(@Param("order") Order order);
    
    /**
     * Find proofs by proof type
     */
    List<DeliveryProof> findByProofTypeOrderByCapturedAtDesc(@Param("proofType") ProofType proofType);
    
    /**
     * Find proofs uploaded by user
     */
    List<DeliveryProof> findByUploadedByOrderByCapturedAtDesc(@Param("uploadedBy") User uploadedBy);
    
    /**
     * Find photo proofs
     */
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.proofType = 'PHOTO' ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findPhotoProofs();
    
    /**
     * Find signature proofs
     */
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.proofType = 'SIGNATURE' ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findSignatureProofs();
    
    /**
     * Find receipt proofs
     */
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.proofType = 'RECEIPT' ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findReceiptProofs();
    
    /**
     * Find proofs by recipient name
     */
    @Query("SELECT dp FROM DeliveryProof dp WHERE LOWER(dp.recipientName) LIKE LOWER(CONCAT('%', :recipientName, '%')) " +
           "ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findByRecipientName(@Param("recipientName") String recipientName);
    
    /**
     * Find proofs within date range
     */
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.capturedAt BETWEEN :startDate AND :endDate " +
           "ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findByCapturedAtRange(@Param("startDate") LocalDateTime startDate, 
                                             @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find proofs with files (photos/receipts)
     */
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.filePath IS NOT NULL " +
           "ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findProofsWithFiles();
    
    /**
     * Find proofs with signatures
     */
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.recipientSignature IS NOT NULL " +
           "ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findProofsWithSignatures();
    
    /**
     * Check if order has proof of specific type
     */
    @Query("SELECT COUNT(dp) > 0 FROM DeliveryProof dp WHERE dp.order = :order AND dp.proofType = :proofType")
    boolean orderHasProofType(@Param("order") Order order, @Param("proofType") ProofType proofType);
    
    /**
     * Find orders missing required proofs
     */
    @Query("SELECT DISTINCT o FROM Order o WHERE o.status.code = 'DELIVERED' " +
           "AND o.id NOT IN (SELECT dp.order.id FROM DeliveryProof dp WHERE dp.proofType = 'PHOTO') " +
           "ORDER BY o.actualDeliveryTime DESC")
    List<Order> findOrdersMissingPhotoProof();
    
    /**
     * Get proof statistics by type
     */
    @Query("SELECT dp.proofType, COUNT(dp) as count FROM DeliveryProof dp " +
           "WHERE dp.capturedAt BETWEEN :startDate AND :endDate " +
           "GROUP BY dp.proofType ORDER BY count DESC")
    List<Object[]> getProofStatistics(@Param("startDate") LocalDateTime startDate, 
                                     @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find recent proofs for audit
     */
    @Query("SELECT dp FROM DeliveryProof dp ORDER BY dp.createdAt DESC")
    List<DeliveryProof> findRecentProofs();
    
    /**
     * Find proofs by file extension (for file management)
     */
    @Query("SELECT dp FROM DeliveryProof dp WHERE dp.fileName LIKE CONCAT('%', :extension) " +
           "ORDER BY dp.capturedAt DESC")
    List<DeliveryProof> findByFileExtension(@Param("extension") String extension);
    
    /**
     * Count proofs by user and date range
     */
    @Query("SELECT COUNT(dp) FROM DeliveryProof dp WHERE dp.uploadedBy = :user " +
           "AND dp.capturedAt BETWEEN :startDate AND :endDate")
    Long countByUserAndDateRange(@Param("user") User user,
                                @Param("startDate") LocalDateTime startDate,
                                @Param("endDate") LocalDateTime endDate);
    
    /**
     * Find proof by file path (for file operations)
     */
    Optional<DeliveryProof> findByFilePath(@Param("filePath") String filePath);
}