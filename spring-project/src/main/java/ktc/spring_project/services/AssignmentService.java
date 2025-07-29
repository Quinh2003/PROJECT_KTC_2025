package ktc.spring_project.services;

import ktc.spring_project.entities.Assignment;
import ktc.spring_project.repositories.AssignmentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;


// Phân công xe - tài xế
@Service
public class AssignmentService {

    @Autowired
    private AssignmentRepository assignmentRepository;

    public Assignment assignDriverAndVehicle(Assignment assignment) {
        return assignmentRepository.save(assignment);
    }

    public List<Assignment> getAssignmentsByDriverId(Long driverId) {
        return assignmentRepository.findByDriverId(driverId);
    }

    public List<Assignment> getAssignmentsByVehicleId(Long vehicleId) {
        return assignmentRepository.findByVehicleId(vehicleId);
    }
}