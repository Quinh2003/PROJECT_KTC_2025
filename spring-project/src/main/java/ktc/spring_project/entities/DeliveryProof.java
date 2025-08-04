package ktc.spring_project.entities;

import jakarta.persistence.*;
import ktc.spring_project.enums.ProofType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Table(name = "delivery_proofs")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DeliveryProof {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Enumerated(EnumType.STRING)
    @Column(name = "proof_type", nullable = false, length = 50)
    private ProofType proofType = ProofType.PHOTO;
    
    @Column(name = "file_path", length = 500)
    private String filePath;
    
    @Column(name = "file_name", length = 255)
    private String fileName;
    
    @Column(name = "recipient_name", length = 255)
    private String recipientName;
    
    @Column(name = "recipient_signature", columnDefinition = "TEXT")
    private String recipientSignature;
    
    @Column(name = "captured_at")
    private LocalDateTime capturedAt;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id", nullable = false)
    private Order order;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "uploaded_by")
    private User uploadedBy;
    
    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;
    
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
    }
}
