package ktc.spring_project.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "delivery_proofs")
public class DeliveryProof {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "proof_type", nullable = false)
    private String proofType; // PHOTO, SIGNATURE, RECEIPT
    
    @Column(name = "file_path")
    private String filePath; // Path to uploaded file
    
    @Column(name = "file_name")
    private String fileName;
    
    @Column(name = "file_size")
    private Long fileSize;
    
    @Column(name = "mime_type")
    private String mimeType;
    
    @Column(columnDefinition = "TEXT")
    private String description;
    
    @Column(name = "recipient_name")
    private String recipientName;
    
    @Column(name = "recipient_signature", columnDefinition = "TEXT")
    private String recipientSignature; // Base64 encoded signature
    
    @Column(name = "captured_at")
    private LocalDateTime capturedAt;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @ManyToOne
    @JoinColumn(name = "delivery_order_id", nullable = false)
    private DeliveryOrder deliveryOrder;
    
    @ManyToOne
    @JoinColumn(name = "uploaded_by")
    private User uploadedBy;
    
    // Constructors
    public DeliveryProof() {
        this.createdAt = LocalDateTime.now();
        this.capturedAt = LocalDateTime.now();
    }
    
    public DeliveryProof(String proofType, DeliveryOrder deliveryOrder, User uploadedBy) {
        this();
        this.proofType = proofType;
        this.deliveryOrder = deliveryOrder;
        this.uploadedBy = uploadedBy;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getProofType() { return proofType; }
    public void setProofType(String proofType) { this.proofType = proofType; }
    
    public String getFilePath() { return filePath; }
    public void setFilePath(String filePath) { this.filePath = filePath; }
    
    public String getFileName() { return fileName; }
    public void setFileName(String fileName) { this.fileName = fileName; }
    
    public Long getFileSize() { return fileSize; }
    public void setFileSize(Long fileSize) { this.fileSize = fileSize; }
    
    public String getMimeType() { return mimeType; }
    public void setMimeType(String mimeType) { this.mimeType = mimeType; }
    
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    
    public String getRecipientName() { return recipientName; }
    public void setRecipientName(String recipientName) { this.recipientName = recipientName; }
    
    public String getRecipientSignature() { return recipientSignature; }
    public void setRecipientSignature(String recipientSignature) { this.recipientSignature = recipientSignature; }
    
    public LocalDateTime getCapturedAt() { return capturedAt; }
    public void setCapturedAt(LocalDateTime capturedAt) { this.capturedAt = capturedAt; }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    
    // Relationship getters/setters
    public DeliveryOrder getDeliveryOrder() { return deliveryOrder; }
    public void setDeliveryOrder(DeliveryOrder deliveryOrder) { this.deliveryOrder = deliveryOrder; }
    
    public User getUploadedBy() { return uploadedBy; }
    public void setUploadedBy(User uploadedBy) { this.uploadedBy = uploadedBy; }
}
