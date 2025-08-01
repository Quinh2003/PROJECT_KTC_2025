package com.ktc.logistics.service;

import com.ktc.logistics.entity.Role;
import com.ktc.logistics.repository.RoleRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class RoleService {
    private final RoleRepository roleRepository;

    public List<Role> findAll() {
        return roleRepository.findAll();
    }

    public Optional<Role> findById(Long id) {
        return roleRepository.findById(id);
    }

    public Role save(Role entity) {
        return roleRepository.save(entity);
    }

    public void delete(Long id) {
        roleRepository.deleteById(id);
    }
}
