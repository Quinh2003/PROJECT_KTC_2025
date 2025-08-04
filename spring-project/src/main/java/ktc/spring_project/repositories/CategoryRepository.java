package ktc.spring_project.repositories;

import ktc.spring_project.entities.Category;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {
    
    Optional<Category> findByCategoryId(String categoryId);
    
    List<Category> findByIsActive(Boolean isActive);
    
    List<Category> findByParentId(Long parentId);
    
    @Query("SELECT c FROM Category c WHERE c.parent IS NULL AND c.isActive = true")
    List<Category> findRootCategories();
    
    @Query("SELECT c FROM Category c WHERE c.parent.id = :parentId AND c.isActive = true")
    List<Category> findActiveChildCategories(@Param("parentId") Long parentId);
    
    @Query("SELECT c FROM Category c WHERE c.isActive = true ORDER BY c.name")
    List<Category> findActiveCategoriesOrderByName();
    
    boolean existsByCategoryId(String categoryId);
    
    @Query("SELECT COUNT(c) FROM Category c WHERE c.parent.id = :parentId")
    long countChildCategories(@Param("parentId") Long parentId);
}

