package ktc.spring_project.enums;

public enum ProofType {
    PHOTO("Ảnh"),
    SIGNATURE("Chữ ký"),
    AUDIO("Ghi âm"),
    VIDEO("Video");

    private final String description;

    ProofType(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
