@Entity
public class OrderTracking {
    @Id @GeneratedValue
    private Long id;

    private String status;
    private LocalDateTime timestamp;

    @ManyToOne
    @JoinColumn(name = "delivery_order_id")
    private DeliveryOrder deliveryOrder;
}
