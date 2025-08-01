package ktc.spring_project.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "activity_logs")
public class ActivityLog {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "actor_id")
    private Long actorId; // ID of user or system actor
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "role_id", nullable = false, foreignKey = @ForeignKey(name = "FK_ACTIVITY_ROLE"))
    private Role role;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "status_id", nullable = false, foreignKey = @ForeignKey(name = "FK_ACTIVITY_STATUS"))
    private Status status;
    
    @Column(name = "action_timestamp", nullable = false)
    private LocalDateTime actionTimestamp;
    
    @Column(name = "session_id", length = 255)
    private String sessionId; // User session identifier for tracking
    
    @Column(name = "device_info", columnDefinition = "JSON")
    private String deviceInfo; // Device details JSON
    
    @Column(name = "metadata", columnDefinition = "JSON")
    private String metadata; // Combined details and notes JSON
    
    // Constructors
    public ActivityLog() {}
    
    public ActivityLog(Long actorId, Role role, Status status, LocalDateTime actionTimestamp, String sessionId) {
        this.actorId = actorId;
        this.role = role;
        this.status = status;
        this.actionTimestamp = actionTimestamp;
        this.sessionId = sessionId;
    }
    
    @PrePersist
    protected void onCreate() {
        if (actionTimestamp == null) {
            actionTimestamp = LocalDateTime.now();
        }
    }
    
    // Getters and Setters
    public Long getId() {
        return id;
    }
    
    public void setId(Long id) {
        this.id = id;
    }
    
    public Long getActorId() {
        return actorId;
    }
    
    public void setActorId(Long actorId) {
        this.actorId = actorId;
    }
    
    public Role getRole() {
        return role;
    }
    
    public void setRole(Role role) {
        this.role = role;
    }
    
    public Status getStatus() {
        return status;
    }
    
    public void setStatus(Status status) {
        this.status = status;
    }
    
    public LocalDateTime getActionTimestamp() {
        return actionTimestamp;
    }
    
    public void setActionTimestamp(LocalDateTime actionTimestamp) {
        this.actionTimestamp = actionTimestamp;
    }
    
    public String getSessionId() {
        return sessionId;
    }
    
    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }
    
    public String getDeviceInfo() {
        return deviceInfo;
    }
    
    public void setDeviceInfo(String deviceInfo) {
        this.deviceInfo = deviceInfo;
    }
    
    public String getMetadata() {
        return metadata;
    }
    
    public void setMetadata(String metadata) {
        this.metadata = metadata;
    }
    
    @Override
    public String toString() {
        return "ActivityLog{" +
                "id=" + id +
                ", actorId=" + actorId +
                ", role=" + (role != null ? role.getRoleName() : "null") +
                ", status=" + (status != null ? status.getCode() : "null") +
                ", actionTimestamp=" + actionTimestamp +
                ", sessionId='" + sessionId + '\'' +
                ", deviceInfo='" + deviceInfo + '\'' +
                ", metadata='" + metadata + '\'' +
                '}';
    }
}