package ktc.spring_project.enums;

public enum PaymentMethod {
    CASH("Cash"),
    CREDIT_CARD("Credit Card"),
    BANK_TRANSFER("Bank Transfer"),
    STRIPE("Stripe");

    private final String displayName;

    PaymentMethod(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }
}
