package ktc.spring_project.dtos.deliveryproof;

import ktc.spring_project.enums.ProofType;
import java.sql.Timestamp;

/**
 * DTO for delivery proof response data
 */
public class DeliveryProofResponseDTO {
    
    private Long id;
    
    // Order information
    private Long orderId;
    private String orderNumber;
    
    private ProofType proofType;
    private String filePath;
    private String fileName;
    private String recipientName;
    private String recipientSignature;
    private String notes;
    private Timestamp capturedAt;
    
    // Uploader information
    private Long uploadedByUserId;
    private String uploadedByUserName;
    private String uploaderRole;
    
    private Timestamp createdAt;
    
    // File information
    private Long fileSize;
    private String mimeType;
    private Boolean fileExists;
    
    // Constructors
    public DeliveryProofResponseDTO() {}
    
    public DeliveryProofResponseDTO(Long id, Long orderId, ProofType proofType, 
                                   String recipientName, Timestamp capturedAt) {
        this.id = id;
        this.orderId = orderId;
        this.proofType = proofType;
        this.recipientName = recipientName;
        this.capturedAt = capturedAt;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public Long getOrderId() { return orderId; }
    public void setOrderId(Long orderId) { this.orderId = orderId; }
    
    public String getOrderNumber() { return orderNumber; }
    public void setOrderNumber(String orderNumber) { this.orderNumber = orderNumber; }
    
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
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Timestamp getCapturedAt() { return capturedAt; }
    public void setCapturedAt(Timestamp capturedAt) { this.capturedAt = capturedAt; }
    
    public Long getUploadedByUserId() { return uploadedByUserId; }
    public void setUploadedByUserId(Long uploadedByUserId) { this.uploadedByUserId = uploadedByUserId; }
    
    public String getUploadedByUserName() { return uploadedByUserName; }
    public void setUploadedByUserName(String uploadedByUserName) { this.uploadedByUserName = uploadedByUserName; }
    
    public String getUploaderRole() { return uploaderRole; }
    public void setUploaderRole(String uploaderRole) { this.uploaderRole = uploaderRole; }
    
    public Timestamp getCreatedAt() { return createdAt; }
    public void setCreatedAt(Timestamp createdAt) { this.createdAt = createdAt; }
    
    public Long getFileSize() { return fileSize; }
    public void setFileSize(Long fileSize) { this.fileSize = fileSize; }
    
    public String getMimeType() { return mimeType; }
    public void setMimeType(String mimeType) { this.mimeType = mimeType; }
    
    public Boolean getFileExists() { return fileExists; }
    public void setFileExists(Boolean fileExists) { this.fileExists = fileExists; }
    
    // Utility methods
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
    
    public boolean isFileAccessible() {
        return Boolean.TRUE.equals(fileExists) && hasFile();
    }
    
    public String getFileDisplayName() {
        return fileName != null ? fileName : "Unknown file";
    }
    
    public String getFormattedFileSize() {
        if (fileSize == null) return "Unknown size";
        
        if (fileSize < 1024) return fileSize + " B";
        if (fileSize < 1024 * 1024) return String.format("%.1f KB", fileSize / 1024.0);
        if (fileSize < 1024 * 1024 * 1024) return String.format("%.1f MB", fileSize / (1024.0 * 1024.0));
        return String.format("%.1f GB", fileSize / (1024.0 * 1024.0 * 1024.0));
    }
    
    public String getUploaderDisplay() {
        if (uploadedByUserName != null) {
            return uploaderRole != null ? 
                String.format("%s (%s)", uploadedByUserName, uploaderRole) : 
                uploadedByUserName;
        }
        return "Unknown uploader";
    }
    
    public String getProofSummary() {
        StringBuilder summary = new StringBuilder();
        summary.append(getProofTypeDisplay());
        
        if (recipientName != null) {
            summary.append(" from ").append(recipientName);
        }
        
        if (capturedAt != null) {
            summary.append(" captured on ").append(capturedAt.toString());
        }
        
        return summary.toString();
    }
    
    public boolean isImageFile() {
        return mimeType != null && mimeType.startsWith("image/");
    }
    
    public boolean isVideoFile() {
        return mimeType != null && mimeType.startsWith("video/");
    }
    
    public boolean isAudioFile() {
        return mimeType != null && mimeType.startsWith("audio/");
    }
    
    public boolean isPdfFile() {
        return mimeType != null && mimeType.equals("application/pdf");
    }
    
    public String getFileTypeIcon() {
        if (isImageFile()) return "🖼️";
        if (isVideoFile()) return "🎥";
        if (isAudioFile()) return "🎵";
        if (isPdfFile()) return "📄";
        return "📁";
    }
}