package ktc.spring_project.dtos.activitylog;

import jakarta.validation.constraints.*;
import java.sql.Timestamp;

/**
 * DTO for creating new activity logs
 */
public class CreateActivityLogRequestDTO {
    
    // Actor ID có thể NULL cho system actions
    private Long actorId;
    
    // Role ID có thể NULL khi actorId là NULL
    private Long roleId;
    
    private Long statusId;
    
    @NotNull(message = "Action timestamp is required")
    private Timestamp actionTimestamp;
    
    @Size(max = 1000, message = "Device info must not exceed 1000 characters")
    private String deviceInfo;
    
    @Size(max = 2000, message = "Metadata must not exceed 2000 characters")
    private String metadata;
    
    // Constructors
    public CreateActivityLogRequestDTO() {}
    
    public CreateActivityLogRequestDTO(Long actorId, Long roleId, Timestamp actionTimestamp) {
        this.actorId = actorId;
        this.roleId = roleId;
        this.actionTimestamp = actionTimestamp;
    }
    
    // Getters and Setters
    public Long getActorId() { return actorId; }
    public void setActorId(Long actorId) { this.actorId = actorId; }
    
    public Long getRoleId() { return roleId; }
    public void setRoleId(Long roleId) { this.roleId = roleId; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public Timestamp getActionTimestamp() { return actionTimestamp; }
    public void setActionTimestamp(Timestamp actionTimestamp) { this.actionTimestamp = actionTimestamp; }
    
    public String getDeviceInfo() { return deviceInfo; }
    public void setDeviceInfo(String deviceInfo) { this.deviceInfo = deviceInfo; }
    
    public String getMetadata() { return metadata; }
    public void setMetadata(String metadata) { this.metadata = metadata; }
}