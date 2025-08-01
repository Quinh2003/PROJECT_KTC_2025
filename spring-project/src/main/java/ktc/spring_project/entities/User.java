package ktc.spring_project.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "users")
public class User {
    @Id 
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "name", nullable = false, length = 255)
    private String name;
    
    @Column(name = "email", unique = true, nullable = false, length = 255)
    private String email;
    
    @Column(name = "password", nullable = false, length = 255)
    private String password;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "role_id", nullable = false, foreignKey = @ForeignKey(name = "FK_USER_ROLE"))
    private Role role;
    
    @Column(name = "phone", length = 50)
    private String phone;
    
    @Column(name = "address", columnDefinition = "TEXT")
    private String address;
    
    @Column(name = "is_active", nullable = false)
    private Boolean isActive = true;
    
    @Column(name = "created_at")
    private LocalDateTime createdAt;
    
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
    
    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;

    // Relationships
    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Token> tokens;

    @OneToMany(mappedBy = "createdBy", fetch = FetchType.LAZY)
    private List<Order> createdOrders;

    @OneToMany(mappedBy = "customer", fetch = FetchType.LAZY)
    private List<Order> customerOrders;

    @OneToMany(mappedBy = "driver", fetch = FetchType.LAZY)
    private List<Order> driverOrders;

    @OneToMany(mappedBy = "createdBy", fetch = FetchType.LAZY)
    private List<Warehouse> createdWarehouses;

    @OneToMany(mappedBy = "createdBy", fetch = FetchType.LAZY)
    private List<Product> createdProducts;

    @OneToMany(mappedBy = "createdBy", fetch = FetchType.LAZY)
    private List<InventoryTransaction> inventoryTransactions;

    @OneToMany(mappedBy = "createdBy", fetch = FetchType.LAZY)
    private List<Route> createdRoutes;

    @OneToMany(mappedBy = "uploadedBy", fetch = FetchType.LAZY)
    private List<DeliveryProof> uploadedProofs;

    @OneToMany(mappedBy = "createdBy", fetch = FetchType.LAZY)
    private List<Payment> createdPayments;
    
    // Constructors
    public User() {}
    
    public User(String name, String email, String password, Role role) {
        this.name = name;
        this.email = email;
        this.password = password;
        this.role = role;
    }
    
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }
    
    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }
    
    // Getters and Setters
    public Long getId() { 
        return id; 
    }
    
    public void setId(Long id) { 
        this.id = id; 
    }
    
    public String getName() { 
        return name; 
    }
    
    public void setName(String name) { 
        this.name = name; 
    }
    
    public String getEmail() { 
        return email; 
    }
    
    public void setEmail(String email) { 
        this.email = email;
    }
    
    public String getPassword() { 
        return password; 
    }
    
    public void setPassword(String password) { 
        this.password = password;
    }
    
    public Role getRole() { 
        return role; 
    }
    
    public void setRole(Role role) { 
        this.role = role;
    }
    
    public String getPhone() { 
        return phone; 
    }
    
    public void setPhone(String phone) { 
        this.phone = phone;
    }
    
    public String getAddress() { 
        return address; 
    }
    
    public void setAddress(String address) { 
        this.address = address;
    }
    
    public Boolean getIsActive() { 
        return isActive; 
    }
    
    public void setIsActive(Boolean isActive) { 
        this.isActive = isActive;
    }
    
    public LocalDateTime getCreatedAt() { 
        return createdAt; 
    }
    
    public void setCreatedAt(LocalDateTime createdAt) { 
        this.createdAt = createdAt; 
    }
    
    public LocalDateTime getUpdatedAt() { 
        return updatedAt; 
    }
    
    public void setUpdatedAt(LocalDateTime updatedAt) { 
        this.updatedAt = updatedAt; 
    }
    
    public String getNotes() { 
        return notes; 
    }
    
    public void setNotes(String notes) { 
        this.notes = notes; 
    }
    
    // Relationship getters/setters
    public List<Token> getTokens() { 
        return tokens; 
    }
    
    public void setTokens(List<Token> tokens) { 
        this.tokens = tokens; 
    }
    
    public List<Order> getCreatedOrders() { 
        return createdOrders; 
    }
    
    public void setCreatedOrders(List<Order> createdOrders) { 
        this.createdOrders = createdOrders; 
    }
    
    public List<Order> getCustomerOrders() { 
        return customerOrders; 
    }
    
    public void setCustomerOrders(List<Order> customerOrders) { 
        this.customerOrders = customerOrders; 
    }
    
    public List<Order> getDriverOrders() { 
        return driverOrders; 
    }
    
    public void setDriverOrders(List<Order> driverOrders) { 
        this.driverOrders = driverOrders; 
    }
    
    public List<Warehouse> getCreatedWarehouses() { 
        return createdWarehouses; 
    }
    
    public void setCreatedWarehouses(List<Warehouse> createdWarehouses) { 
        this.createdWarehouses = createdWarehouses; 
    }
    
    public List<Product> getCreatedProducts() { 
        return createdProducts; 
    }
    
    public void setCreatedProducts(List<Product> createdProducts) { 
        this.createdProducts = createdProducts; 
    }
    
    public List<InventoryTransaction> getInventoryTransactions() { 
        return inventoryTransactions; 
    }
    
    public void setInventoryTransactions(List<InventoryTransaction> inventoryTransactions) { 
        this.inventoryTransactions = inventoryTransactions; 
    }
    
    public List<Route> getCreatedRoutes() { 
        return createdRoutes; 
    }
    
    public void setCreatedRoutes(List<Route> createdRoutes) { 
        this.createdRoutes = createdRoutes; 
    }
    
    public List<DeliveryProof> getUploadedProofs() { 
        return uploadedProofs; 
    }
    
    public void setUploadedProofs(List<DeliveryProof> uploadedProofs) { 
        this.uploadedProofs = uploadedProofs; 
    }
    
    public List<Payment> getCreatedPayments() { 
        return createdPayments; 
    }
    
    public void setCreatedPayments(List<Payment> createdPayments) { 
        this.createdPayments = createdPayments; 
    }
    
    @Override
    public String toString() {
        return "User{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", email='" + email + '\'' +
                ", role=" + (role != null ? role.getRoleName() : "null") +
                ", phone='" + phone + '\'' +
                ", address='" + address + '\'' +
                ", isActive=" + isActive +
                ", createdAt=" + createdAt +
                ", updatedAt=" + updatedAt +
                ", notes='" + notes + '\'' +
                '}';
    }
}
