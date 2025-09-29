
package ktc.spring_project.dtos.deliveryproof;


import ktc.spring_project.enums.ProofType;
import java.sql.Timestamp;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.NotBlank;

/**
 * DTO for creating a new delivery proof
 */
public class CreateDeliveryProofRequestDTO {

    @NotNull(message = "Order ID is required")
    private Long orderId;

    @NotNull(message = "Proof type is required")
    private ProofType proofType;

    private String filePath;
    private String fileName;

    @NotBlank(message = "Recipient name is required")
    private String recipientName;

    private String recipientSignature;
    private Timestamp capturedAt;
    private Long uploadedBy;
    private String notes;
    
    // Constructors
    public CreateDeliveryProofRequestDTO() {}
    
    public CreateDeliveryProofRequestDTO(Long orderId, ProofType proofType, String recipientName) {
        this.orderId = orderId;
        this.proofType = proofType;
        this.recipientName = recipientName;
    }
    
    // Getters and Setters
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public ProofType getProofType() { return proofType; }
    public void setProofType(ProofType proofType) { this.proofType = proofType; }
    
    public String getFilePath() { return filePath; }
    public void setFilePath(String filePath) { this.filePath = filePath; }
    
    public String getFileName() { return fileName; }
    public void setFileName(String fileName) { this.fileName = fileName; }
    
    public String getRecipientName() { return recipientName; }
    public void setRecipientName(String recipientName) { this.recipientName = recipientName; }
    
    public String getRecipientSignature() { return recipientSignature; }
    public void setRecipientSignature(String recipientSignature) { this.recipientSignature = recipientSignature; }
    
    public Timestamp getCapturedAt() { return capturedAt; }
    public void setCapturedAt(Timestamp capturedAt) { this.capturedAt = capturedAt; }
    
    public Long getUploadedBy() { return uploadedBy; }
    public void setUploadedBy(Long uploadedBy) { this.uploadedBy = uploadedBy; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    // Validation methods
    public boolean isValid() {
        return orderId != null && proofType != null && recipientName != null && !recipientName.trim().isEmpty();
    }
    
    public boolean hasFile() {
        return filePath != null && !filePath.trim().isEmpty();
    }
    
    public boolean hasSignature() {
        return recipientSignature != null && !recipientSignature.trim().isEmpty();
    }
    
    public boolean hasNotes() {
        return notes != null && !notes.trim().isEmpty();
    }
    
    public boolean isPhotoProof() {
        return ProofType.PHOTO.equals(proofType);
    }
    
    public boolean isSignatureProof() {
        return ProofType.SIGNATURE.equals(proofType);
    }
    
    public boolean isDocumentProof() {
        return ProofType.DOCUMENT.equals(proofType);
    }
    
    public boolean isVideoProof() {
        return ProofType.VIDEO.equals(proofType);
    }
    
    public boolean isAudioProof() {
        return ProofType.AUDIO.equals(proofType);
    }
    
    public String getProofTypeDisplay() {
        return proofType != null ? proofType.name().replace("_", " ") : "Unknown";
    }
    
    public boolean requiresFile() {
        return isPhotoProof() || isDocumentProof() || isVideoProof() || isAudioProof();
    }
    
    public boolean requiresSignature() {
        return isSignatureProof();
    }
}