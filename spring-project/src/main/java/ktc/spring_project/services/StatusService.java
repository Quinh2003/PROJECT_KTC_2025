package ktc.spring_project.services;

import ktc.spring_project.entities.Status;
import ktc.spring_project.repositories.StatusRepository;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class StatusService {

    private final StatusRepository statusRepository;

    public StatusService(StatusRepository statusRepository) {
        this.statusRepository = statusRepository;
    }

    public List<Status> getStatusesByType(String type) {
        return statusRepository.findByType(type);
    }

    public Optional<Status> getStatusByTypeAndName(String type, String name) {
        return statusRepository.findByTypeAndName(type, name);
    }

    public List<Status> getStatusesByTypeOrderByName(String type) {
        return statusRepository.findByTypeOrderByName(type);
    }

    public boolean existsByTypeAndName(String type, String name) {
        return statusRepository.existsByTypeAndName(type, name);
    }

    public Status saveStatus(Status status) {
        return statusRepository.save(status);
    }

    public void deleteStatusById(Short id) {
        statusRepository.deleteById(id);
    }
}