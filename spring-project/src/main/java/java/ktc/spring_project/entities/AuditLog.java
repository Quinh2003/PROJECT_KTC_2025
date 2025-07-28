@Entity
public class AuditLog {
    @Id @GeneratedValue
    private Long id;

    private String action; // CREATE, UPDATE, DELETE
    private LocalDateTime timestamp;

    @ManyToOne
    @JoinColumn(name = "user_id")
    private User user;
}
