package ktc.spring_project.repositories;

import ktc.spring_project.entities.Warehouse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;


@Repository
public interface WarehouseRepository extends JpaRepository<Warehouse, Long> {
    

    
    List<Warehouse> findByIsActive(Boolean isActive);
    
    @Query("SELECT w FROM Warehouse w WHERE w.isActive = true ORDER BY w.name")
    List<Warehouse> findActiveWarehousesOrderByName();
    
    @Query("SELECT w FROM Warehouse w WHERE w.latitude BETWEEN :minLat AND :maxLat " +
           "AND w.longitude BETWEEN :minLng AND :maxLng AND w.isActive = true")
    List<Warehouse> findWarehousesInArea(@Param("minLat") BigDecimal minLat,
                                       @Param("maxLat") BigDecimal maxLat,
                                       @Param("minLng") BigDecimal minLng,
                                       @Param("maxLng") BigDecimal maxLng);
    

    
    @Query("SELECT COUNT(w) FROM Warehouse w WHERE w.isActive = true")
    long countActiveWarehouses();
}

