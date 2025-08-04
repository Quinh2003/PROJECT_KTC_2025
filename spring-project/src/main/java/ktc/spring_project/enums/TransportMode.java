package ktc.spring_project.enums;

public enum TransportMode {
    ROAD("Đường bộ"),
    AIR("Hàng không"),
    SEA("Đường biển"),
    RAIL("Đường sắt");

    private final String description;

    TransportMode(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
