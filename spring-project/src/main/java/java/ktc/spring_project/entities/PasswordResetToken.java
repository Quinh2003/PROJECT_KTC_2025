package java.ktc.spring_project.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "password_reset_tokens")
public class PasswordResetToken {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true, nullable = false)
    private String token;
    
    @Column(nullable = false)
    private LocalDateTime expiry;
    
    @Column(nullable = false)
    private Boolean used = false;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @ManyToOne
    @JoinColumn(name = "user_id")
    private User user;
    
    // Constructors
    public PasswordResetToken() {
        this.createdAt = LocalDateTime.now();
    }
    
    public PasswordResetToken(String token, User user) {
        this();
        this.token = token;
        this.user = user;
        // Token expires in 15 minutes as per requirements
        this.expiry = LocalDateTime.now().plusMinutes(15);
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getToken() { return token; }
    public void setToken(String token) { this.token = token; }
    
    public LocalDateTime getExpiry() { return expiry; }
    public void setExpiry(LocalDateTime expiry) { this.expiry = expiry; }
    
    public Boolean getUsed() { return used; }
    public void setUsed(Boolean used) { this.used = used; }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    
    // Relationship getters/setters
    public User getUser() { return user; }
    public void setUser(User user) { this.user = user; }
    
    // Utility methods
    public boolean isExpired() {
        return LocalDateTime.now().isAfter(this.expiry);
    }
    
    public boolean isValid() {
        return !used && !isExpired();
    }
}
