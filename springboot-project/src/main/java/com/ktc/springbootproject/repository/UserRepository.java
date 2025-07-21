package com.ktc.springbootproject.repository;

import com.ktc.springbootproject.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {
    
    // Find user by email
    Optional<User> findByEmail(String email);
    
    // Find users by first name containing (case-insensitive)
    List<User> findByFirstNameContainingIgnoreCase(String firstName);
    
    // Find users by last name containing (case-insensitive)
    List<User> findByLastNameContainingIgnoreCase(String lastName);
    
    // Check if email exists
    boolean existsByEmail(String email);
    
    // Custom query to find users by full name
    @Query("SELECT u FROM User u WHERE LOWER(CONCAT(u.firstName, ' ', u.lastName)) LIKE LOWER(CONCAT('%', :fullName, '%'))")
    List<User> findByFullNameContainingIgnoreCase(@Param("fullName") String fullName);
    
    // Find all users ordered by created date
    @Query("SELECT u FROM User u ORDER BY u.createdAt DESC")
    List<User> findAllOrderByCreatedAtDesc();
}
