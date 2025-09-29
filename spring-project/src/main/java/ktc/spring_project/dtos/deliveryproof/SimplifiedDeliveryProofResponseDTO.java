package ktc.spring_project.dtos.deliveryproof;

import ktc.spring_project.enums.ProofType;
import java.sql.Timestamp;

/**
 * Simplified DTO for delivery proof response data - only essential fields for mobile app
 */
public class SimplifiedDeliveryProofResponseDTO {
    
    private Long id;
    private ProofType proofType;
    private String filePath;
    private String fileName;
    private String recipientName;
    private String notes;
    private Timestamp capturedAt;
    
    // Constructors
    public SimplifiedDeliveryProofResponseDTO() {}
    
    public SimplifiedDeliveryProofResponseDTO(Long id, ProofType proofType, String filePath, 
                                            String fileName, String recipientName, 
                                            String notes, Timestamp capturedAt) {
        this.id = id;
        this.proofType = proofType;
        this.filePath = filePath;
        this.fileName = fileName;
        this.recipientName = recipientName;
        this.notes = notes;
        this.capturedAt = capturedAt;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public ProofType getProofType() { return proofType; }
    public void setProofType(ProofType proofType) { this.proofType = proofType; }
    
    public String getFilePath() { return filePath; }
    public void setFilePath(String filePath) { this.filePath = filePath; }
    
    public String getFileName() { return fileName; }
    public void setFileName(String fileName) { this.fileName = fileName; }
    
    public String getRecipientName() { return recipientName; }
    public void setRecipientName(String recipientName) { this.recipientName = recipientName; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public Timestamp getCapturedAt() { return capturedAt; }
    public void setCapturedAt(Timestamp capturedAt) { this.capturedAt = capturedAt; }
}