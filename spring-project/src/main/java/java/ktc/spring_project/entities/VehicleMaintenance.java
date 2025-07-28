@Entity
public class VehicleMaintenance {
    @Id @GeneratedValue
    private Long id;

    private String description;
    private LocalDateTime maintenanceDate;

    @ManyToOne
    @JoinColumn(name = "vehicle_id")
    private Vehicle vehicle;

    @ManyToOne
    @JoinColumn(name = "creator_id")
    private User creator;
}
