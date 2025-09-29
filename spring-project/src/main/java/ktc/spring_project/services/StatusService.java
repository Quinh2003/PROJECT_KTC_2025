package ktc.spring_project.services;

import ktc.spring_project.entities.Status;
import ktc.spring_project.repositories.StatusRepository;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import ktc.spring_project.enums.StatusType;


@Service
public class StatusService {

    private final StatusRepository statusRepository;

    public StatusService(StatusRepository statusRepository) {
        this.statusRepository = statusRepository;
    }

    public List<Status> getStatusesByType(String type) {
    StatusType statusType = StatusType.valueOf(type.toUpperCase());
    return statusRepository.findByStatusType(statusType);
}
public List<Status> getAllStatuses() {
    return statusRepository.findAll();
}
public Optional<Status> getStatusById(Short id) {
    return statusRepository.findById(id);
}

public Optional<Status> getStatusByTypeAndName(String type, String name) {
    StatusType statusType = StatusType.valueOf(type.toUpperCase());
    return statusRepository.findByStatusTypeAndName(statusType, name);
}

public List<Status> getStatusesByTypeOrderByName(String type) {
    StatusType statusType = StatusType.valueOf(type.toUpperCase());
    return statusRepository.findByStatusTypeOrderByName(statusType);
}

public boolean existsByTypeAndName(String type, String name) {
    StatusType statusType = StatusType.valueOf(type.toUpperCase());
    return statusRepository.existsByStatusTypeAndName(statusType, name);
}

    public Status saveStatus(Status status) {
        return statusRepository.save(status);
    }

    public void deleteStatusById(Short id) {
        statusRepository.deleteById(id);
    }
}