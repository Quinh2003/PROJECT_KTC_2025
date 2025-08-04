package ktc.spring_project.entities;

import jakarta.persistence.*;
import ktc.spring_project.enums.ActionType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Table(name = "activity_logs")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ActivityLog {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "actor_id")
    private User actor;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "role_id", nullable = false)
    private Role role;
    
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "status_id", nullable = false)
    private Status status;
    
    @Enumerated(EnumType.STRING)
    @Column(name = "action_type", nullable = false, length = 50)
    private ActionType actionType;
    
    @Column(name = "table_name", length = 64)
    private String tableName;
    
    @Column(name = "record_id")
    private Long recordId;
    
    @Column(name = "action_timestamp", nullable = false)
    private LocalDateTime actionTimestamp;
    
    @Column(name = "metadata", columnDefinition = "JSON")
    private String metadata;
    
    @PrePersist
    protected void onCreate() {
        if (actionTimestamp == null) {
            actionTimestamp = LocalDateTime.now();
        }
    }
}
