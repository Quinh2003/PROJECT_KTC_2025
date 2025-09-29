package ktc.spring_project.dtos.maintenance;

import jakarta.validation.constraints.*;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * DTO for creating maintenance requests by drivers (auto-detect vehicle)
 */
public class CreateMaintenanceRequestByDriverDTO {

    @NotBlank(message = "Description is required")
    @Size(max = 2000, message = "Description must not exceed 2000 characters")
    private String description;

    @NotBlank(message = "Maintenance type is required")
    private String maintenanceType;

    @NotNull(message = "Status ID is required")
    private Long statusId = 51L; // Default to MAINTENANCE_PENDING for driver requests

    @DecimalMin(value = "0.0", message = "Cost must be positive")
    private BigDecimal cost;

    private Timestamp maintenanceDate;

    private Timestamp nextDueDate;

    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    private String notes;

    // Constructors
    public CreateMaintenanceRequestByDriverDTO() {}

    // Getters and setters
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

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
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
}