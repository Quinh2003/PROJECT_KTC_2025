package ktc.spring_project.repositories;

import ktc.spring_project.entities.Store;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;

@Repository
public interface StoreRepository extends JpaRepository<Store, Long> {
        
    List<Store> findByIsActive(Boolean isActive);
    
    @Query("SELECT s FROM Store s WHERE s.isActive = true ORDER BY s.storeName")
    List<Store> findActiveStoresOrderByName();
    
    @Query("SELECT s FROM Store s WHERE s.latitude BETWEEN :minLat AND :maxLat " +
           "AND s.longitude BETWEEN :minLng AND :maxLng AND s.isActive = true")
    List<Store> findStoresInArea(@Param("minLat") BigDecimal minLat,
                               @Param("maxLat") BigDecimal maxLat,
                               @Param("minLng") BigDecimal minLng,
                               @Param("maxLng") BigDecimal maxLng);
    
    @Query("SELECT s FROM Store s WHERE LOWER(s.storeName) LIKE LOWER(CONCAT('%', :name, '%')) AND s.isActive = true")
    List<Store> findByStoreNameContainingIgnoreCase(@Param("name") String name);
        
    @Query("SELECT COUNT(s) FROM Store s WHERE s.isActive = true")
    long countActiveStores();
    
    // Find stores by userId (createdBy)
    @Query("SELECT s FROM Store s WHERE s.createdBy.id = :userId")
    List<Store> findByCreatedById(@Param("userId") Long userId);
}

