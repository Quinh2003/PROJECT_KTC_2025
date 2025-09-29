package ktc.spring_project.repositories;

import ktc.spring_project.entities.ChecklistProgress;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.ChecklistStep;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ChecklistProgressRepository extends JpaRepository<ChecklistProgress, Long> {
    java.util.Optional<ChecklistProgress> findByUserAndOrderAndStep(User user, Order order, ChecklistStep step);

    // Tìm tất cả progress của một đơn hàng
    java.util.List<ChecklistProgress> findByOrder(Order order);
}
