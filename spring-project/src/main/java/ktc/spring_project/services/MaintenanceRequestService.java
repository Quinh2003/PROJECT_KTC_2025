package ktc.spring_project.services;

import ktc.spring_project.dtos.maintenance.CreateMaintenanceRequestByDriverDTO;
import ktc.spring_project.dtos.maintenance.MaintenanceRequestResponseDTO;
import ktc.spring_project.dtos.maintenance.UpdateMaintenanceRequestDTO;
import ktc.spring_project.entities.MaintenanceRequest;
import ktc.spring_project.entities.Status;
import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Vehicle;
import ktc.spring_project.exceptions.EntityNotFoundException;
import ktc.spring_project.repositories.MaintenanceRequestRepository;
import ktc.spring_project.repositories.StatusRepository;
import ktc.spring_project.repositories.UserRepository;
import ktc.spring_project.repositories.VehicleRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Transactional
public class MaintenanceRequestService {

    @Autowired
    private MaintenanceRequestRepository maintenanceRequestRepository;

    @Autowired
    private VehicleRepository vehicleRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private StatusRepository statusRepository;

    @Autowired
    private DriverService driverService;

    /**
     * Create maintenance request with auto vehicle detection from driver
     */
    public MaintenanceRequestResponseDTO createMaintenanceRequestByDriver(CreateMaintenanceRequestByDriverDTO createDTO, Long driverId) {
        // Validate driver exists using DriverService
        User driver = userRepository.findById(driverId)
                .orElseThrow(() -> new EntityNotFoundException("Driver not found with ID: " + driverId));

        // Use DriverService to get current vehicle (follows existing pattern)
        Vehicle vehicle = driverService.getCurrentVehicle(driverId);
        if (vehicle == null) {
            throw new EntityNotFoundException("No vehicle assigned to driver with ID: " + driverId);
        }

        // Validate status exists  
        Status status = statusRepository.findById(createDTO.getStatusId().shortValue())
                .orElseThrow(() -> new EntityNotFoundException("Status not found with ID: " + createDTO.getStatusId()));

        // Create new maintenance request
        MaintenanceRequest maintenanceRequest = new MaintenanceRequest();
        maintenanceRequest.setVehicle(vehicle);
        maintenanceRequest.setCreatedBy(driver);
        maintenanceRequest.setDescription(createDTO.getDescription());
        maintenanceRequest.setMaintenanceType(createDTO.getMaintenanceType());
        maintenanceRequest.setStatus(status);
        maintenanceRequest.setCost(createDTO.getCost());
        maintenanceRequest.setMaintenanceDate(createDTO.getMaintenanceDate());
        maintenanceRequest.setNextDueDate(createDTO.getNextDueDate());
        maintenanceRequest.setNotes(createDTO.getNotes());

    // Save the maintenance request
    MaintenanceRequest savedRequest = maintenanceRequestRepository.save(maintenanceRequest);

    // Sau khi tạo yêu cầu bảo trì, cập nhật trạng thái xe sang MAINTENANCE_PENDING (status_id=51)
    // Có thể lấy statusId từ createDTO hoặc hardcode 51 nếu luôn là "Cần bảo trì"
    vehicleRepository.updateStatus(vehicle.getId(), status.getId());

    return convertToDTO(savedRequest);
    }

    /**
     * Create monthly maintenance request by fleet manager
     */
    public MaintenanceRequestResponseDTO createMonthlyMaintenanceRequest(CreateMaintenanceRequestByDriverDTO createDTO, Long vehicleId) {
        // Validate vehicle exists
        Vehicle vehicle = vehicleRepository.findById(vehicleId)
                .orElseThrow(() -> new EntityNotFoundException("Vehicle not found with ID: " + vehicleId));

        // For monthly maintenance, we don't need a specific driver - it's initiated by fleet
        // Use a system user or admin as creator (assuming admin user ID = 1)
        User systemUser = userRepository.findById(1L)
                .orElseThrow(() -> new EntityNotFoundException("System user not found"));

        // Validate status exists  
        Status status = statusRepository.findById(createDTO.getStatusId().shortValue())
                .orElseThrow(() -> new EntityNotFoundException("Status not found with ID: " + createDTO.getStatusId()));

        // Create new maintenance request
        MaintenanceRequest maintenanceRequest = new MaintenanceRequest();
        maintenanceRequest.setVehicle(vehicle);
        maintenanceRequest.setCreatedBy(systemUser); // Created by fleet/admin
        maintenanceRequest.setDescription(createDTO.getDescription());
        maintenanceRequest.setMaintenanceType(createDTO.getMaintenanceType());
        maintenanceRequest.setStatus(status);
        maintenanceRequest.setCost(createDTO.getCost());
        maintenanceRequest.setMaintenanceDate(createDTO.getMaintenanceDate());
        maintenanceRequest.setNextDueDate(createDTO.getNextDueDate());
        maintenanceRequest.setNotes(createDTO.getNotes());

        // Save the maintenance request
        MaintenanceRequest savedRequest = maintenanceRequestRepository.save(maintenanceRequest);

        return convertToDTO(savedRequest);
    }

    /**
     * Get maintenance requests for specific driver with pagination
     */
    public Page<MaintenanceRequestResponseDTO> getDriverMaintenanceRequests(Long driverId, Pageable pageable) {
        // Find maintenance requests created by this driver
        Page<MaintenanceRequest> requestsPage = maintenanceRequestRepository.findByCreatedBy_IdOrderByCreatedAtDesc(driverId, pageable);
        
        return requestsPage.map(this::convertToDTO);
    }

    /**
     * Get all maintenance requests with pagination
     */
    public Page<MaintenanceRequestResponseDTO> getAllMaintenanceRequests(Pageable pageable) {
        Page<MaintenanceRequest> requestsPage = maintenanceRequestRepository.findAll(pageable);
        return requestsPage.map(this::convertToDTO);
    }

    /**
     * Get maintenance request by ID
     */
    public MaintenanceRequestResponseDTO getMaintenanceRequestById(Long id) {
        MaintenanceRequest request = maintenanceRequestRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Maintenance request not found with ID: " + id));
        return convertToDTO(request);
    }

    /**
     * Update maintenance request
     */
    public MaintenanceRequestResponseDTO updateMaintenanceRequest(Long id, UpdateMaintenanceRequestDTO updateDTO) {
        MaintenanceRequest request = maintenanceRequestRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Maintenance request not found with ID: " + id));

        // Update fields if provided
        if (updateDTO.getDescription() != null) {
            request.setDescription(updateDTO.getDescription());
        }
        if (updateDTO.getMaintenanceType() != null) {
            request.setMaintenanceType(updateDTO.getMaintenanceType());
        }
        Status status = null;
        if (updateDTO.getStatusId() != null) {
            status = statusRepository.findById(updateDTO.getStatusId().shortValue())
                    .orElseThrow(() -> new EntityNotFoundException("Status not found with ID: " + updateDTO.getStatusId()));
            request.setStatus(status);
        }
        if (updateDTO.getCost() != null) {
            request.setCost(updateDTO.getCost());
        }
        if (updateDTO.getMaintenanceDate() != null) {
            request.setMaintenanceDate(updateDTO.getMaintenanceDate());
        }
        if (updateDTO.getNextDueDate() != null) {
            request.setNextDueDate(updateDTO.getNextDueDate());
        }
        if (updateDTO.getNotes() != null) {
            request.setNotes(updateDTO.getNotes());
        }

        MaintenanceRequest savedRequest = maintenanceRequestRepository.save(request);

        // Đồng bộ trạng thái vehicle nếu statusId được cập nhật
        if (status != null) {
            Vehicle vehicle = request.getVehicle();
            vehicle.setStatus(status);
            vehicleRepository.save(vehicle);
        }

        return convertToDTO(savedRequest);
    }

    /**
     * Delete maintenance request
     */
    public void deleteMaintenanceRequest(Long id) {
        if (!maintenanceRequestRepository.existsById(id)) {
            throw new EntityNotFoundException("Maintenance request not found with ID: " + id);
        }
        maintenanceRequestRepository.deleteById(id);
    }

    /**
     * Convert entity to DTO
     */
    private MaintenanceRequestResponseDTO convertToDTO(MaintenanceRequest request) {
        MaintenanceRequestResponseDTO dto = new MaintenanceRequestResponseDTO();
        
        dto.setId(request.getId());
        dto.setDescription(request.getDescription());
        dto.setMaintenanceType(request.getMaintenanceType());
        dto.setCost(request.getCost());
        dto.setMaintenanceDate(request.getMaintenanceDate());
        dto.setNextDueDate(request.getNextDueDate());
        dto.setNotes(request.getNotes());
        dto.setCreatedAt(request.getCreatedAt());
        dto.setUpdatedAt(request.getUpdatedAt());

        // Vehicle information
        if (request.getVehicle() != null) {
            Vehicle vehicle = request.getVehicle();
            MaintenanceRequestResponseDTO.VehicleBasicInfo vehicleInfo = 
                new MaintenanceRequestResponseDTO.VehicleBasicInfo(
                    vehicle.getId(),
                    vehicle.getLicensePlate(),
                    vehicle.getVehicleType().toString()
                );
            dto.setVehicle(vehicleInfo);
        }

        // Creator information
        if (request.getCreatedBy() != null) {
            User creator = request.getCreatedBy();
            MaintenanceRequestResponseDTO.UserBasicInfo creatorInfo = 
                new MaintenanceRequestResponseDTO.UserBasicInfo(
                    creator.getId(),
                    creator.getFullName(),
                    creator.getEmail()
                );
            dto.setCreatedBy(creatorInfo);
        }

        // Status information
        if (request.getStatus() != null) {
            Status status = request.getStatus();
            MaintenanceRequestResponseDTO.StatusInfo statusInfo = 
                new MaintenanceRequestResponseDTO.StatusInfo(
                    (long) status.getId(),
                    status.getName()
                );
            dto.setStatus(statusInfo);
        }

        return dto;
    }

    /**
     * Get maintenance requests summary for dashboard
     */
    public List<MaintenanceRequestResponseDTO> getMaintenanceRequestsSummary() {
        List<MaintenanceRequest> recentRequests = maintenanceRequestRepository
                .findTop10ByOrderByCreatedAtDesc();
        return recentRequests.stream()
                .map(this::convertToDTO)
                .collect(Collectors.toList());
    }

    /**
     * Search maintenance requests by keyword
     */
    public Page<MaintenanceRequestResponseDTO> searchMaintenanceRequests(String keyword, Pageable pageable) {
        Page<MaintenanceRequest> requestsPage = maintenanceRequestRepository
                .searchMaintenanceRequests(keyword, pageable);
        return requestsPage.map(this::convertToDTO);
    }

    /**
     * Get maintenance requests with filters
     */
    public Page<MaintenanceRequestResponseDTO> getMaintenanceRequestsWithFilters(
            String status, String maintenanceType, Long vehicleId, Pageable pageable) {
        
        // For now, return all requests - you can add custom filtering logic here
        return getAllMaintenanceRequests(pageable);
    }
}