package ktc.spring_project.dtos.maintenance;

import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for returning maintenance request details matching vehicle_maintenance table
 */
public class MaintenanceRequestResponseDTO {

    private Long id;
    private String description;
    private String maintenanceType;
    private BigDecimal cost;
    private Timestamp maintenanceDate;
    private Timestamp nextDueDate;
    private String notes;
    private Timestamp createdAt;
    private Timestamp updatedAt;

    // Vehicle information
    private VehicleBasicInfo vehicle;

    // Creator information
    private UserBasicInfo createdBy;

    // Status information
    private StatusInfo status;

    // Nested DTOs for basic information
    public static class VehicleBasicInfo {
        private Long id;
        private String licensePlate;
        private String vehicleType;

        // Constructors
        public VehicleBasicInfo() {}

        public VehicleBasicInfo(Long id, String licensePlate, String vehicleType) {
            this.id = id;
            this.licensePlate = licensePlate;
            this.vehicleType = vehicleType;
        }

        // Getters and setters
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

        public String getVehicleType() {
            return vehicleType;
        }

        public void setVehicleType(String vehicleType) {
            this.vehicleType = vehicleType;
        }
    }

    public static class UserBasicInfo {
        private Long id;
        private String fullName;
        private String email;

        // Constructors
        public UserBasicInfo() {}

        public UserBasicInfo(Long id, String fullName, String email) {
            this.id = id;
            this.fullName = fullName;
            this.email = email;
        }

        // Getters and setters
        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getFullName() {
            return fullName;
        }

        public void setFullName(String fullName) {
            this.fullName = fullName;
        }

        public String getEmail() {
            return email;
        }

        public void setEmail(String email) {
            this.email = email;
        }
    }

    public static class StatusInfo {
        private Long id;
        private String name;

        // Constructors
        public StatusInfo() {}

        public StatusInfo(Long id, String name) {
            this.id = id;
            this.name = name;
        }

        // Getters and setters
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
    }

    // Constructors
    public MaintenanceRequestResponseDTO() {}

    // Main getters and setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getMaintenanceType() {
        return maintenanceType;
    }

    public void setMaintenanceType(String maintenanceType) {
        this.maintenanceType = maintenanceType;
    }

    public BigDecimal getCost() {
        return cost;
    }

    public void setCost(BigDecimal cost) {
        this.cost = cost;
    }

    public Timestamp getMaintenanceDate() {
        return maintenanceDate;
    }

    public void setMaintenanceDate(Timestamp maintenanceDate) {
        this.maintenanceDate = maintenanceDate;
    }

    public Timestamp getNextDueDate() {
        return nextDueDate;
    }

    public void setNextDueDate(Timestamp nextDueDate) {
        this.nextDueDate = nextDueDate;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
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

    public VehicleBasicInfo getVehicle() {
        return vehicle;
    }

    public void setVehicle(VehicleBasicInfo vehicle) {
        this.vehicle = vehicle;
    }

    public UserBasicInfo getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(UserBasicInfo createdBy) {
        this.createdBy = createdBy;
    }

    public StatusInfo getStatus() {
        return status;
    }

    public void setStatus(StatusInfo status) {
        this.status = status;
    }
}