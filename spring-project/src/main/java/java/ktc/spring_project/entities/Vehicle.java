@Entity
public class Vehicle {
    @Id @GeneratedValue
    private Long id;

    private String licensePlate;
    private String type;

    @OneToMany(mappedBy = "vehicle")
    private List<DeliveryOrder> deliveryOrders;

    @OneToMany(mappedBy = "vehicle")
    private List<VehicleMaintenance> maintenances;

    @OneToMany(mappedBy = "vehicle")
    private List<DispatchAssignment> dispatchAssignments;
}
