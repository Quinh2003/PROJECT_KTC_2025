package ktc.spring_project.repositories;

import ktc.spring_project.entities.Warehouse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface WarehouseRepository extends JpaRepository<Warehouse, Long> {
    
    List<Warehouse> findByIsActiveTrue();
    
    List<Warehouse> findByNameContainingIgnoreCase(String name);
    
    @Query("SELECT w FROM Warehouse w WHERE w.capacity >= :minCapacity")
    List<Warehouse> findByMinCapacity(@Param("minCapacity") Double minCapacity);
    
    @Query("SELECT w FROM Warehouse w WHERE w.maxWeight >= :minWeight")
    List<Warehouse> findByMinWeight(@Param("minWeight") Double minWeight);
}
