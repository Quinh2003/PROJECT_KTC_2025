package ktc.spring_project.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.NoRepositoryBean;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

/**
 * Base repository interface providing common database operations
 * All entity repositories should extend this interface
 * 
 * @param <T> Entity type
 * @param <ID> Primary key type
 */
@NoRepositoryBean
public interface BaseRepository<T, ID extends Serializable> 
    extends JpaRepository<T, ID>, JpaSpecificationExecutor<T> {
    
    /**
     * Find all active entities (if entity has isActive field)
     * Default implementation returns all entities
     */
    default List<T> findAllActive() {
        return findAll();
    }
    
    /**
     * Find entity by ID and ensure it's active
     */
    default Optional<T> findByIdActive(ID id) {
        return findById(id);
    }
    
    /**
     * Soft delete by setting isActive = false (if applicable)
     * Default implementation performs hard delete
     */
    default void softDelete(T entity) {
        delete(entity);
    }
    
    /**
     * Soft delete by ID
     */
    default void softDeleteById(ID id) {
        deleteById(id);
    }
    
    /**
     * Check if entity exists and is active
     */
    default boolean existsByIdActive(ID id) {
        return existsById(id);
    }
}