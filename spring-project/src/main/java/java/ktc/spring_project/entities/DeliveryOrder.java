@Entity
public class DeliveryOrder {
    @Id @GeneratedValue
    private Long id;

    private String code;
    private String status;

    @ManyToOne
    @JoinColumn(name = "created_by")
    private User createdBy;

    @ManyToOne
    @JoinColumn(name = "vehicle_id")
    private Vehicle vehicle;

    @OneToMany(mappedBy = "order")
    private List<OrderTracking> trackings;

    @OneToMany(mappedBy = "deliveryOrder")
    private List<DispatchAssignment> dispatchAssignments;
}
