package com.ktc.springbootproject.service;

import com.ktc.springbootproject.dto.UserDTO;
import com.ktc.springbootproject.model.User;
import com.ktc.springbootproject.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Transactional
public class UserService {
    
    private final UserRepository userRepository;
    
    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }
    
    // Get all users
    @Transactional(readOnly = true)
    public List<UserDTO> getAllUsers() {
        return userRepository.findAllOrderByCreatedAtDesc()
                .stream()
                .map(this::convertToDTO)
                .collect(Collectors.toList());
    }
    
    // Get user by ID
    @Transactional(readOnly = true)
    public Optional<UserDTO> getUserById(Long id) {
        return userRepository.findById(id)
                .map(this::convertToDTO);
    }
    
    // Get user by email
    @Transactional(readOnly = true)
    public Optional<UserDTO> getUserByEmail(String email) {
        return userRepository.findByEmail(email)
                .map(this::convertToDTO);
    }
    
    // Create new user
    public UserDTO createUser(String firstName, String lastName, String email, String password) {
        // Check if email already exists
        if (userRepository.existsByEmail(email)) {
            throw new RuntimeException("Email already exists: " + email);
        }
        
        User user = new User(firstName, lastName, email, password);
        User savedUser = userRepository.save(user);
        return convertToDTO(savedUser);
    }
    
    // Update user
    public Optional<UserDTO> updateUser(Long id, String firstName, String lastName, String email) {
        return userRepository.findById(id)
                .map(user -> {
                    // Check if email is being changed and if new email already exists
                    if (!user.getEmail().equals(email) && userRepository.existsByEmail(email)) {
                        throw new RuntimeException("Email already exists: " + email);
                    }
                    
                    user.setFirstName(firstName);
                    user.setLastName(lastName);
                    user.setEmail(email);
                    
                    User updatedUser = userRepository.save(user);
                    return convertToDTO(updatedUser);
                });
    }
    
    // Delete user
    public boolean deleteUser(Long id) {
        if (userRepository.existsById(id)) {
            userRepository.deleteById(id);
            return true;
        }
        return false;
    }
    
    // Search users by name
    @Transactional(readOnly = true)
    public List<UserDTO> searchUsersByName(String name) {
        return userRepository.findByFullNameContainingIgnoreCase(name)
                .stream()
                .map(this::convertToDTO)
                .collect(Collectors.toList());
    }
    
    // Check if email exists
    @Transactional(readOnly = true)
    public boolean emailExists(String email) {
        return userRepository.existsByEmail(email);
    }
    
    // Convert User entity to UserDTO
    private UserDTO convertToDTO(User user) {
        return new UserDTO(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getEmail()
        );
    }
}
