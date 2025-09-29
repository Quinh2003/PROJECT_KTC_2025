package ktc.spring_project.dtos.activitylog;

import java.sql.Timestamp;

/**
 * DTO for activity log response data
 */
public class ActivityLogResponseDTO {
    
    private Long id;
    private Long actorId;
    private String actorName;
    
    // Role information
    private Long roleId;
    private String roleName;
    private String roleDescription;
    
    // Status information
    private Long statusId;
    private String statusCode;
    private String statusDescription;
    
    private Timestamp actionTimestamp;
    private String deviceInfo;
    private String metadata;
    
    // Constructors
    public ActivityLogResponseDTO() {}
    
    public ActivityLogResponseDTO(Long id, Long actorId, String actorName, Timestamp actionTimestamp) {
        this.id = id;
        this.actorId = actorId;
        this.actorName = actorName;
        this.actionTimestamp = actionTimestamp;
    }
    
    // Getters and Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public Long getActorId() { return actorId; }
    public void setActorId(Long actorId) { this.actorId = actorId; }
    
    public String getActorName() { return actorName; }
    public void setActorName(String actorName) { this.actorName = actorName; }
    
    public Long getRoleId() { return roleId; }
    public void setRoleId(Long roleId) { this.roleId = roleId; }
    
    public String getRoleName() { return roleName; }
    public void setRoleName(String roleName) { this.roleName = roleName; }
    
    public String getRoleDescription() { return roleDescription; }
    public void setRoleDescription(String roleDescription) { this.roleDescription = roleDescription; }
    
    public Long getStatusId() { return statusId; }
    public void setStatusId(Long statusId) { this.statusId = statusId; }
    
    public String getStatusCode() { return statusCode; }
    public void setStatusCode(String statusCode) { this.statusCode = statusCode; }
    
    public String getStatusDescription() { return statusDescription; }
    public void setStatusDescription(String statusDescription) { this.statusDescription = statusDescription; }
    
    public Timestamp getActionTimestamp() { return actionTimestamp; }
    public void setActionTimestamp(Timestamp actionTimestamp) { this.actionTimestamp = actionTimestamp; }
    
    public String getDeviceInfo() { return deviceInfo; }
    public void setDeviceInfo(String deviceInfo) { this.deviceInfo = deviceInfo; }
    
    public String getMetadata() { return metadata; }
    public void setMetadata(String metadata) { this.metadata = metadata; }
    
    // Utility methods
    public String getDisplayInfo() {
        return String.format("%s (%s) - %s", 
            actorName != null ? actorName : "User #" + actorId,
            roleName != null ? roleName : "Role #" + roleId,
            statusDescription != null ? statusDescription : statusCode);
    }
    
    public boolean hasDeviceInfo() {
        return deviceInfo != null && !deviceInfo.trim().isEmpty();
    }
    
    public boolean hasMetadata() {
        return metadata != null && !metadata.trim().isEmpty();
    }
}