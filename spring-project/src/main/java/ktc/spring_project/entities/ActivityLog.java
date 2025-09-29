package ktc.spring_project.entities;

import jakarta.persistence.*;
import ktc.spring_project.enums.ActionType;
import java.sql.Timestamp;
import java.time.LocalDateTime;

@Entity
@Table(name = "activity_logs")
public class ActivityLog {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "actor_id")
    private Long actorId;

    @ManyToOne(optional = true)
    @JoinColumn(name = "actor_id", insertable = false, updatable = false)
    private User actor;

    @ManyToOne(optional = true)
    @JoinColumn(name = "role_id", nullable = true)
    private Role role;

    @ManyToOne
    @JoinColumn(name = "status_id")
    private Status status;

    @Enumerated(EnumType.STRING)
    @Column(name = "action_type")
    private ActionType actionType;

    @Column(name = "table_name")
    private String tableName;

    @Column(name = "record_id")
    private Long recordId;

    @Column(name = "action_timestamp")
    private Timestamp actionTimestamp;

    @Column(columnDefinition = "json")
    private String metadata;

    public ActivityLog() {}

    public ActivityLog(Long actorId, ActionType actionType) {
        this.actorId = actorId;
        this.actionType = actionType;
        this.actionTimestamp = Timestamp.valueOf(LocalDateTime.now());
    }

    // Getters and setters

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

    public User getActor() {
        return actor;
    }

    public void setActor(User actor) {
        this.actor = actor;
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


    public ActionType getActionType() {
        return actionType;
    }

    public void setActionType(ActionType actionType) {
        this.actionType = actionType;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public Long getRecordId() {
        return recordId;
    }

    public void setRecordId(Long recordId) {
        this.recordId = recordId;
    }

    public Timestamp getActionTimestamp() {
        return actionTimestamp;
    }

    public void setActionTimestamp(Timestamp actionTimestamp) {
        this.actionTimestamp = actionTimestamp;
    }

    public String getMetadata() {
        return metadata;
    }

    public void setMetadata(String metadata) {
        this.metadata = metadata;
    }
}