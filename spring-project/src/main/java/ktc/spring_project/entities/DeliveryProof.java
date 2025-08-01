package ktc.spring_project.entities;

import ktc.spring_project.enums.ProofType;
import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "delivery_proofs")
public class DeliveryProof {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Enumerated(EnumType.STRING)
    @Column(name = "proof_type", nullable = false)
    private ProofType proofType;
    
    @Column(name = "file_path", length = 500)
    private String filePath;
    
    @Column(name = "file_name", length = 255)
    private String fileName;
    
    @Column(name = "recipient_name", length = 255)
    private String recipientName;
    
    @Column(name = "recipient_signature", columnDefinition = "TEXT")
    private String recipientSignature; // Base64 encoded signature
    
    @Column(name = "captured_at")
    private LocalDateTime capturedAt;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id", nullable = false, foreignKey = @ForeignKey(name = "FK_PROOF_ORDER"))
    private Order order;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "uploaded_by", foreignKey = @ForeignKey(name = "FK_PROOF_UPLOADED_BY"))
    private User uploadedBy;
    
    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;
    
    // Constructors
    public DeliveryProof() {}
    
    public DeliveryProof(ProofType proofType, String filePath, String fileName, Order order, User uploadedBy) {
        this.proofType = proofType;
        this.filePath = filePath;
        this.fileName = fileName;
        this.order = order;
        this.uploadedBy = uploadedBy;
    }
    
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        if (capturedAt == null) {
            capturedAt = LocalDateTime.now();
        }
    }
    
    // Getters and Setters
    public Long getId() { 
        return id; 
    }
    
    public void setId(Long id) { 
        this.id = id; 
    }
    
    public ProofType getProofType() { 
        return proofType; 
    }
    
    public void setProofType(ProofType proofType) { 
        this.proofType = proofType; 
    }
    
    public String getFilePath() { 
        return filePath; 
    }
    
    public void setFilePath(String filePath) { 
        this.filePath = filePath; 
    }
    
    public String getFileName() { 
        return fileName; 
    }
    
    public void setFileName(String fileName) { 
        this.fileName = fileName; 
    }
    
    public String getRecipientName() { 
        return recipientName; 
    }
    
    public void setRecipientName(String recipientName) { 
        this.recipientName = recipientName; 
    }
    
    public String getRecipientSignature() { 
        return recipientSignature; 
    }
    
    public void setRecipientSignature(String recipientSignature) { 
        this.recipientSignature = recipientSignature; 
    }
    
    public LocalDateTime getCapturedAt() { 
        return capturedAt; 
    }
    
    public void setCapturedAt(LocalDateTime capturedAt) { 
        this.capturedAt = capturedAt; 
    }
    
    public LocalDateTime getCreatedAt() { 
        return createdAt; 
    }
    
    public void setCreatedAt(LocalDateTime createdAt) { 
        this.createdAt = createdAt; 
    }
    
    public Order getOrder() { 
        return order; 
    }
    
    public void setOrder(Order order) { 
        this.order = order; 
    }
    
    public User getUploadedBy() { 
        return uploadedBy; 
    }
    
    public void setUploadedBy(User uploadedBy) { 
        this.uploadedBy = uploadedBy; 
    }
    
    public String getNotes() { 
        return notes; 
    }
    
    public void setNotes(String notes) { 
        this.notes = notes; 
    }
    
    @Override
    public String toString() {
        return "DeliveryProof{" +
                "id=" + id +
                ", proofType=" + proofType +
                ", filePath='" + filePath + '\'' +
                ", fileName='" + fileName + '\'' +
                ", recipientName='" + recipientName + '\'' +
                ", capturedAt=" + capturedAt +
                ", createdAt=" + createdAt +
                ", order=" + (order != null ? order.getOrderCode() : "null") +
                ", uploadedBy=" + (uploadedBy != null ? uploadedBy.getName() : "null") +
                ", notes='" + notes + '\'' +
                '}';
    }
}
