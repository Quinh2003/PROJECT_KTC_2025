package ktc.spring_project.services;

import ktc.spring_project.entities.Role;
import ktc.spring_project.repositories.RoleRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class RoleService {
    private RoleRepository roleRepository;

    public List<Role> findAll() {
        return roleRepository.findAll();
    }

    public Optional<Role> findById(Long id) {
        return roleRepository.findById(id);
    }

    public Role save(Role entities) {
        return roleRepository.save(entities);
    }

    public void delete(Long id) {
        roleRepository.deleteById(id);
    }
}
