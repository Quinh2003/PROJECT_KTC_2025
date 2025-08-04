package ktc.spring_project.services;

import ktc.spring_project.entities.ActivityLog;
import ktc.spring_project.repository.ActivityLogRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class ActivityLogService {
    private final ActivityLogRepository activityLogRepository;

    public List<ActivityLog> findAll() {
        return activityLogRepository.findAll();
    }

    public Optional<ActivityLog> findById(Long id) {
        return activityLogRepository.findById(id);
    }

    public ActivityLog save(ActivityLog entities) {
        return activityLogRepository.save(entities);
    }

    public void delete(Long id) {
        activityLogRepository.deleteById(id);
    }
}
