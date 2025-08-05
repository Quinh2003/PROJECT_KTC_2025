package ktc.spring_project.services;

import ktc.spring_project.entities.ActivityLog;
import ktc.spring_project.repositories.ActivityLogRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ActivityLogService {

    @Autowired
    private ActivityLogRepository activityLogRepository;

    public ActivityLog createActivityLog(ActivityLog activityLog) {
        return activityLogRepository.save(activityLog);
    }

    public ActivityLog getActivityLogById(Long id) {
        return activityLogRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("ActivityLog not found with id: " + id));
    }

    public List<ActivityLog> getAllActivityLogs() {
        return activityLogRepository.findAll();
    }

    public ActivityLog updateActivityLog(Long id, ActivityLog activityLogDetails) {
        ActivityLog activityLog = getActivityLogById(id);
        activityLog.setActorId(activityLogDetails.getActorId());
        activityLog.setActor(activityLogDetails.getActor());
        activityLog.setRole(activityLogDetails.getRole());
        activityLog.setStatus(activityLogDetails.getStatus());
        activityLog.setActionType(activityLogDetails.getActionType());
        activityLog.setTableName(activityLogDetails.getTableName());
        activityLog.setRecordId(activityLogDetails.getRecordId());
        activityLog.setActionTimestamp(activityLogDetails.getActionTimestamp());
        activityLog.setMetadata(activityLogDetails.getMetadata());
        return activityLogRepository.save(activityLog);
    }

    // Utility methods cho relationships
    public List<ActivityLog> getActivityLogsByActor(Long actorId) {
        return activityLogRepository.findByActorId(actorId);
    }

    public List<ActivityLog> getActivityLogsByRole(Long roleId) {
        return activityLogRepository.findByRoleId(roleId);
    }

    public List<ActivityLog> getSystemActivityLogs() {
        return activityLogRepository.findByActorIdIsNull();
    }

    public void deleteActivityLog(Long id) {
        ActivityLog activityLog = getActivityLogById(id);
        activityLogRepository.delete(activityLog);
    }
}