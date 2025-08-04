package ktc.spring_project.services;

import ktc.spring_project.entities.Status;
import ktc.spring_project.repository.StatusRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class StatusService {
    private final StatusRepository statusRepository;

    public List<Status> findAll() {
        return statusRepository.findAll();
    }

    public Optional<Status> findById(Long id) {
        return statusRepository.findById(id);
    }

    public Status save(Status entities) {
        return statusRepository.save(entities);
    }

    public void delete(Long id) {
        statusRepository.deleteById(id);
    }
}
