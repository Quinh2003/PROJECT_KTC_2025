package ktc.spring_project.enums;

/**
 * Delivery proof types for verification
 * Used in delivery_proofs table.proof_type
 */
public enum ProofType {
    PHOTO("Photo", "Photo evidence of delivery", true),
    SIGNATURE("Signature", "Customer signature confirmation", false),
    RECEIPT("Receipt", "Delivery receipt document", true);
    
    private final String displayName;
    private final String description;
    private final boolean requiresFile; // Whether this proof type requires file upload
    
    ProofType(String displayName, String description, boolean requiresFile) {
        this.displayName = displayName;
        this.description = description;
        this.requiresFile = requiresFile;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public String getDescription() {
        return description;
    }
    
    public boolean requiresFile() {
        return requiresFile;
    }
}