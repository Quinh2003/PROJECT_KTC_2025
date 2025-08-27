package ktc.spring_project.enums;

public enum PaymentMethod {
    CASH("Cash"),
    CREDIT_CARD("Credit Card"),
    DEBIT("Debit Card"),
    BANK_TRANSFER("Bank Transfer"),
    TRANSFER("Transfer"),
    STRIPE("Stripe");

    private final String displayName;

    PaymentMethod(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}