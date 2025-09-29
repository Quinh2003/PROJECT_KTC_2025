package ktc.spring_project.dtos.vehiclemaintenance;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public class CreateVehicleMaintenanceRequestDTO {
    private Long vehicleId;
    private LocalDateTime maintenanceDate;
    private LocalDateTime nextDueDate;
    private String maintenanceType;
    private String description;
    private BigDecimal cost;
    private Short statusId;
    private Long createdBy;
    private String notes;

    public Long getVehicleId() { return vehicleId; }
    public void setVehicleId(Long vehicleId) { this.vehicleId = vehicleId; }
    public LocalDateTime getMaintenanceDate() { return maintenanceDate; }
    public void setMaintenanceDate(LocalDateTime maintenanceDate) { this.maintenanceDate = maintenanceDate; }
    public LocalDateTime getNextDueDate() { return nextDueDate; }
    public void setNextDueDate(LocalDateTime nextDueDate) { this.nextDueDate = nextDueDate; }
    public String getMaintenanceType() { return maintenanceType; }
    public void setMaintenanceType(String maintenanceType) { this.maintenanceType = maintenanceType; }
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    public BigDecimal getCost() { return cost; }
    public void setCost(BigDecimal cost) { this.cost = cost; }
    public Short getStatusId() { return statusId; }
    public void setStatusId(Short statusId) { this.statusId = statusId; }
    public Long getCreatedBy() { return createdBy; }
    public void setCreatedBy(Long createdBy) { this.createdBy = createdBy; }
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
}
