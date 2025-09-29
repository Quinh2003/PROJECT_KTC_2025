package ktc.spring_project.repositories;

import ktc.spring_project.entities.ChecklistStep;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ChecklistStepRepository extends JpaRepository<ChecklistStep, Long> {
    // Add custom query methods if needed
}
