package ktc.spring_project.entities;

import jakarta.persistence.*;
import ktc.spring_project.enums.VehicleType;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.math.BigDecimal;
import java.sql.Timestamp;

@Entity
@Table(name = "vehicles")
public class Vehicle {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "license_plate", length = 20, nullable = false, unique = true)
    private String licensePlate;

    @Enumerated(EnumType.STRING)
    @Column(name = "vehicle_type", length = 50, nullable = false, columnDefinition = "VARCHAR(50) DEFAULT 'TRUCK'")
    private VehicleType vehicleType = VehicleType.TRUCK;

    @Column(name = "capacity_weight_kg", precision = 10, scale = 2, columnDefinition = "DECIMAL(10,2) DEFAULT 0.00")
    private BigDecimal capacityWeightKg = BigDecimal.ZERO;

    @Column(name = "capacity_volume_m3", precision = 10, scale = 2, columnDefinition = "DECIMAL(10,2) DEFAULT 0.00")
    private BigDecimal capacityVolumeM3 = BigDecimal.ZERO;

    @ManyToOne
    @JoinColumn(name = "current_driver_id")
    private User currentDriver;

    @Column(columnDefinition = "TEXT")
    private String notes;

    @ManyToOne
    @JoinColumn(name = "status_id", nullable = false)
    private Status status;

    @CreationTimestamp
    @Column(name = "created_at")
    private Timestamp createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private Timestamp updatedAt;

    // Constructor mặc định
    public Vehicle() {}

    // Getters và Setters

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getLicensePlate() {
        return licensePlate;
    }

    public void setLicensePlate(String licensePlate) {
        this.licensePlate = licensePlate;
    }

    public VehicleType getVehicleType() {
        return vehicleType;
    }

    public void setVehicleType(VehicleType vehicleType) {
        this.vehicleType = vehicleType;
    }

    public BigDecimal getCapacityWeightKg() {
        return capacityWeightKg;
    }

    public void setCapacityWeightKg(BigDecimal capacityWeightKg) {
        this.capacityWeightKg = capacityWeightKg;
    }

    public BigDecimal getCapacityVolumeM3() {
        return capacityVolumeM3;
    }

    public void setCapacityVolumeM3(BigDecimal capacityVolumeM3) {
        this.capacityVolumeM3 = capacityVolumeM3;
    }

    public User getCurrentDriver() {
        return currentDriver;
    }

    public void setCurrentDriver(User currentDriver) {
        this.currentDriver = currentDriver;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    public Timestamp getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Timestamp createdAt) {
        this.createdAt = createdAt;
    }

    public Timestamp getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(Timestamp updatedAt) {
        this.updatedAt = updatedAt;
    }
}