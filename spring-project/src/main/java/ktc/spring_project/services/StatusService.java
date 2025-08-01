package com.ktc.logistics.service;

import com.ktc.logistics.entity.Status;
import com.ktc.logistics.repository.StatusRepository;
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

    public Status save(Status entity) {
        return statusRepository.save(entity);
    }

    public void delete(Long id) {
        statusRepository.deleteById(id);
    }
}
