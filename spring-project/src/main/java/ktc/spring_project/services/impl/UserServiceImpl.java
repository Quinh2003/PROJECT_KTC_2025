package ktc.spring_project.services.impl;

import ktc.spring_project.dtos.user.CreateUserRequestDTO;
import ktc.spring_project.dtos.user.UpdateUserRequestDTO;
import ktc.spring_project.dtos.user.UserResponseDTO;
import ktc.spring_project.dtos.common.PagedResponse;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Role;
import ktc.spring_project.repositories.UserRepository;
import ktc.spring_project.repositories.RoleRepository;
import ktc.spring_project.services.interfaces.UserService;
import ktc.spring_project.services.exceptions.UserServiceException;
import ktc.spring_project.services.utils.ServiceUtils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implementation of UserService interface
 * Handles user management operations with transaction support
 */
@Service
@Transactional
public class UserServiceImpl implements UserService {
    
    @Autowired
    private UserRepository userRepository;
    
    @Autowired
    private RoleRepository roleRepository;
    
    // ===== CRUD OPERATIONS =====
    
    @Override
    public UserResponseDTO createUser(CreateUserRequestDTO createUserDTO) {
        // Validate email uniqueness
        if (userRepository.existsByEmail(createUserDTO.getEmail())) {
            throw UserServiceException.emailAlreadyExists(createUserDTO.getEmail());
        }
        
        // Validate role exists
        Optional<Role> roleOpt = roleRepository.findById(createUserDTO.getRoleId());
        if (roleOpt.isEmpty()) {
            throw UserServiceException.invalidRole(createUserDTO.getRoleId());
        }
        
        // Create new user entity
        User user = new User();
        user.setName(createUserDTO.getName());
        user.setEmail(createUserDTO.getEmail());
        user.setPassword(ServiceUtils.hashPassword(createUserDTO.getPassword()));
        user.setRole(roleOpt.get());
        user.setPhone(createUserDTO.getPhone());
        user.setAddress(createUserDTO.getAddress());
        user.setIsActive(true);
        user.setCreatedAt(LocalDateTime.now());
        user.setUpdatedAt(LocalDateTime.now());
        
        // Save and return response
        User savedUser = userRepository.save(user);
        return convertToResponseDTO(savedUser);
    }
    
    @Override
    public UserResponseDTO updateUser(Long id, UpdateUserRequestDTO updateUserDTO) {
        User user = userRepository.findById(id)
            .orElseThrow(() -> UserServiceException.userNotFound(id));
        
        // Check email uniqueness if email is being changed
        if (!user.getEmail().equals(updateUserDTO.getEmail()) && 
            userRepository.existsByEmail(updateUserDTO.getEmail())) {
            throw UserServiceException.emailAlreadyExists(updateUserDTO.getEmail());
        }
        
        // Validate role if it's being changed
        if (updateUserDTO.getRoleId() != null && !updateUserDTO.getRoleId().equals(user.getRole().getId())) {
            Optional<Role> roleOpt = roleRepository.findById(updateUserDTO.getRoleId());
            if (roleOpt.isEmpty()) {
                throw UserServiceException.invalidRole(updateUserDTO.getRoleId());
            }
            user.setRole(roleOpt.get());
        }
        
        // Update user fields
        user.setName(updateUserDTO.getName());
        user.setEmail(updateUserDTO.getEmail());
        user.setPhone(updateUserDTO.getPhone());
        user.setAddress(updateUserDTO.getAddress());
        user.setUpdatedAt(LocalDateTime.now());
        
        User updatedUser = userRepository.save(user);
        return convertToResponseDTO(updatedUser);
    }
    
    @Override
    @Transactional(readOnly = true)
    public Optional<UserResponseDTO> getUserById(Long id) {
        return userRepository.findById(id)
            .map(this::convertToResponseDTO);
    }
    
    @Override
    @Transactional(readOnly = true)
    public Optional<User> getUserEntityById(Long id) {
        return userRepository.findById(id);
    }
    
    @Override
    @Transactional(readOnly = true)
    public Optional<UserResponseDTO> getUserByEmail(String email) {
        return userRepository.findByEmail(email)
            .map(this::convertToResponseDTO);
    }
    
    @Override
    @Transactional(readOnly = true)
    public PagedResponse<UserResponseDTO> getAllUsers(Pageable pageable) {
        Page<User> userPage = userRepository.findAll(pageable);
        Page<UserResponseDTO> responsePage = userPage.map(this::convertToResponseDTO);
        return ServiceUtils.convertToPagedResponse(responsePage);
    }
    
    @Override
    public void deleteUser(Long id) {
        User user = userRepository.findById(id)
            .orElseThrow(() -> UserServiceException.userNotFound(id));
        
        // Soft delete by setting isActive to false
        user.setIsActive(false);
        user.setUpdatedAt(LocalDateTime.now());
        userRepository.save(user);
    }
    
    // ===== ROLE-BASED QUERIES =====
    
    @Override
    @Transactional(readOnly = true)
    public List<UserResponseDTO> getUsersByRole(Long roleId) {
        List<User> users = userRepository.findByRoleId(roleId);
        return users.stream()
            .map(this::convertToResponseDTO)
            .collect(Collectors.toList());
    }
    
    @Override
    @Transactional(readOnly = true)
    public List<UserResponseDTO> getAllDrivers() {
        List<User> drivers = userRepository.findAllDrivers();
        return drivers.stream()
            .map(this::convertToResponseDTO)
            .collect(Collectors.toList());
    }
    
    @Override
    @Transactional(readOnly = true)
    public List<UserResponseDTO> getAvailableDrivers() {
        List<User> availableDrivers = userRepository.findAvailableDrivers();
        return availableDrivers.stream()
            .map(this::convertToResponseDTO)
            .collect(Collectors.toList());
    }
    
    @Override
    @Transactional(readOnly = true)
    public List<UserResponseDTO> getAllCustomers() {
        List<User> customers = userRepository.findAllCustomers();
        return customers.stream()
            .map(this::convertToResponseDTO)
            .collect(Collectors.toList());
    }
    
    // ===== AUTHENTICATION SUPPORT =====
    
    @Override
    @Transactional(readOnly = true)
    public Optional<User> authenticateUser(String email, String password) {
        String hashedPassword = ServiceUtils.hashPassword(password);
        Optional<User> userOpt = userRepository.findByEmail(email);
        
        if (userOpt.isPresent()) {
            User user = userOpt.get();
            if (user.getPassword().equals(hashedPassword)) {
                if (!user.getIsActive()) {
                    throw UserServiceException.userInactive(email);
                }
                return Optional.of(user);
            }
        }
        
        return Optional.empty();
    }
    
    @Override
    @Transactional(readOnly = true)
    public boolean isEmailTaken(String email) {
        return userRepository.existsByEmail(email);
    }
    
    @Override
    public void updatePassword(Long userId, String newPassword) {
        User user = userRepository.findById(userId)
            .orElseThrow(() -> UserServiceException.userNotFound(userId));
        
        user.setPassword(ServiceUtils.hashPassword(newPassword));
        user.setUpdatedAt(LocalDateTime.now());
        userRepository.save(user);
    }
    
    // ===== SEARCH AND FILTER =====
    
    @Override
    @Transactional(readOnly = true)
    public PagedResponse<UserResponseDTO> searchUsers(String searchTerm, Pageable pageable) {
        Page<User> userPage = userRepository.searchByNameOrEmail(searchTerm, pageable);
        Page<UserResponseDTO> responsePage = userPage.map(this::convertToResponseDTO);
        return ServiceUtils.convertToPagedResponse(responsePage);
    }
    
    @Override
    @Transactional(readOnly = true)
    public PagedResponse<UserResponseDTO> getActiveUsers(Pageable pageable) {
        Page<User> userPage = userRepository.findByIsActiveTrue(pageable);
        Page<UserResponseDTO> responsePage = userPage.map(this::convertToResponseDTO);
        return ServiceUtils.convertToPagedResponse(responsePage);
    }
    
    // ===== BUSINESS OPERATIONS =====
    
    @Override
    public void activateUser(Long userId) {
        User user = userRepository.findById(userId)
            .orElseThrow(() -> UserServiceException.userNotFound(userId));
        
        user.setIsActive(true);
        user.setUpdatedAt(LocalDateTime.now());
        userRepository.save(user);
    }
    
    @Override
    public void deactivateUser(Long userId) {
        User user = userRepository.findById(userId)
            .orElseThrow(() -> UserServiceException.userNotFound(userId));
        
        user.setIsActive(false);
        user.setUpdatedAt(LocalDateTime.now());
        userRepository.save(user);
    }
    
    @Override
    @Transactional(readOnly = true)
    public Map<String, Object> getUserStatistics() {
        return Map.of(
            "totalUsers", userRepository.count(),
            "activeUsers", userRepository.countByRoleName("CUSTOMER") + userRepository.countByRoleName("DRIVER") + userRepository.countByRoleName("MANAGER") + userRepository.countByRoleName("ADMIN"),
            "inactiveUsers", 0L, // Temporary
            "customerCount", userRepository.countByRoleName("CUSTOMER"),
            "driverCount", userRepository.countByRoleName("DRIVER"),
            "managerCount", userRepository.countByRoleName("MANAGER"),
            "adminCount", userRepository.countByRoleName("ADMIN")
        );
    }
    
    // ===== HELPER METHODS =====
    
    /**
     * Convert User entity to UserResponseDTO
     */
    private UserResponseDTO convertToResponseDTO(User user) {
        UserResponseDTO response = new UserResponseDTO();
        response.setId(user.getId());
        response.setName(user.getName());
        response.setEmail(user.getEmail());
        response.setRoleId(user.getRole().getId());
        response.setRoleName(user.getRole().getRoleName());
        response.setPhone(user.getPhone());
        response.setAddress(user.getAddress());
        response.setIsActive(user.getIsActive());
        response.setCreatedAt(user.getCreatedAt());
        response.setUpdatedAt(user.getUpdatedAt());
        return response;
    }
}