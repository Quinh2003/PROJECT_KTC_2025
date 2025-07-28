@Entity
public class PasswordResetToken {
    @Id @GeneratedValue
    private Long id;

    private String token;
    private LocalDateTime expiry;

    @ManyToOne
    @JoinColumn(name = "user_id")
    private User user;
}
