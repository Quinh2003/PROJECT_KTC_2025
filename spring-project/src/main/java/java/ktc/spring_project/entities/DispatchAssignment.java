@Entity
public class DispatchAssignment {
    @Id @GeneratedValue
    private Long id;

    private LocalDateTime assignedAt;

    @ManyToOne
    @JoinColumn(name = "delivery_order_id")
    private DeliveryOrder deliveryOrder;

    @ManyToOne
    @JoinColumn(name = "vehicle_id")
    private Vehicle vehicle;

    @ManyToOne
    @JoinColumn(name = "driver_id")
    private User driver; // role = DRIVER
}
