package ktc.spring_project.enums;

public enum TransactionType {
    IN("Nhập kho"),
    OUT("Xuất kho"),
    TRANSFER("Chuyển kho");

    private final String description;

    TransactionType(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
