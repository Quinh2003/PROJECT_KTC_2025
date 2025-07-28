package java.ktc.spring_project.entities;

import java.ktc.spring_project.enums.UserRole;
import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "users")
public class User {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;
    
    @Column(unique = true, nullable = false)
    private String email;
    
    @Column(nullable = false)
    private String password;
    
    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private UserRole role;
    
    private String phone;
    private String address;
    
    @Column(nullable = false)
    private Boolean isActive = true;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @OneToMany(mappedBy = "user")
    private List<AuditLog> auditLogs;

    @OneToMany(mappedBy = "user")
    private List<PasswordResetToken> resetTokens;

    @OneToMany(mappedBy = "createdBy")
    private List<DeliveryOrder> createdOrders;

    @OneToMany(mappedBy = "creator")
    private List<VehicleMaintenance> maintenances;

    @OneToMany(mappedBy = "driver")
    private List<DispatchAssignment> dispatchAssignments;
    
    @OneToMany(mappedBy = "recipient", cascade = CascadeType.ALL)
    private List<Notification> receivedNotifications;
    
    @OneToMany(mappedBy = "sender", cascade = CascadeType.ALL)
    private List<Notification> sentNotifications;
    
    // Constructors
    public User() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }
    
    public User(String name, String email, String password, UserRole role) {
        this();
        this.name = name;
        this.email = email;
        this.password = password;
        this.role = role;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getName() { return name; }
    public void setName(String name) { 
        this.name = name; 
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getEmail() { return email; }
    public void setEmail(String email) { 
        this.email = email;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getPassword() { return password; }
    public void setPassword(String password) { 
        this.password = password;
        this.updatedAt = LocalDateTime.now();
    }
    
    public UserRole getRole() { return role; }
    public void setRole(UserRole role) { 
        this.role = role;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getPhone() { return phone; }
    public void setPhone(String phone) { 
        this.phone = phone;
        this.updatedAt = LocalDateTime.now();
    }
    
    public String getAddress() { return address; }
    public void setAddress(String address) { 
        this.address = address;
        this.updatedAt = LocalDateTime.now();
    }
    
    public Boolean getIsActive() { return isActive; }
    public void setIsActive(Boolean isActive) { 
        this.isActive = isActive;
        this.updatedAt = LocalDateTime.now();
    }
    
    public LocalDateTime getCreatedAt() { return createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    
    // Relationship getters/setters
    public List<AuditLog> getAuditLogs() { return auditLogs; }
    public void setAuditLogs(List<AuditLog> auditLogs) { this.auditLogs = auditLogs; }
    
    public List<PasswordResetToken> getResetTokens() { return resetTokens; }
    public void setResetTokens(List<PasswordResetToken> resetTokens) { this.resetTokens = resetTokens; }
    
    public List<DeliveryOrder> getCreatedOrders() { return createdOrders; }
    public void setCreatedOrders(List<DeliveryOrder> createdOrders) { this.createdOrders = createdOrders; }
    
    public List<VehicleMaintenance> getMaintenances() { return maintenances; }
    public void setMaintenances(List<VehicleMaintenance> maintenances) { this.maintenances = maintenances; }
    
    public List<DispatchAssignment> getDispatchAssignments() { return dispatchAssignments; }
    public void setDispatchAssignments(List<DispatchAssignment> dispatchAssignments) { this.dispatchAssignments = dispatchAssignments; }
    
    public List<Notification> getReceivedNotifications() { return receivedNotifications; }
    public void setReceivedNotifications(List<Notification> receivedNotifications) { this.receivedNotifications = receivedNotifications; }
    
    public List<Notification> getSentNotifications() { return sentNotifications; }
    public void setSentNotifications(List<Notification> sentNotifications) { this.sentNotifications = sentNotifications; }
}
