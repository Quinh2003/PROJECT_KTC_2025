package ktc.spring_project.enums;

public enum PaymentMethod {
    CASH("Tiền mặt"),
    CARD("Thẻ"),
    BANK_TRANSFER("Chuyển khoản"),
    E_WALLET("Ví điện tử"),
    COD("Thu hộ");

    private final String description;

    PaymentMethod(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
