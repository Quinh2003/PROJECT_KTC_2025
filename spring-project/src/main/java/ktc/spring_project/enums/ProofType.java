package ktc.spring_project.enums;

public enum ProofType {
    PHOTO("Photo"),
    SIGNATURE("Signature"),
    AUDIO("Audio"),
    VIDEO("Video"),
    DOCUMENT("Document"),
    RECEIPT("Receipt");

    private final String displayName;

    ProofType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
