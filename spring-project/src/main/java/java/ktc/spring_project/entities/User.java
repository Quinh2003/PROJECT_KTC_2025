package com.ktc.springbootproject.entities;

public @Entity
public class User {
    @Id @GeneratedValue
    private Long id;

    private String name;
    private String email;
    private String password;
    private String role; // ADMIN, DRIVER, etc.

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
}
 {
    
}
