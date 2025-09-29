package ktc.spring_project.controllers;

import ktc.spring_project.entities.VehicleMaintenance;
import ktc.spring_project.services.VehicleMaintenanceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/vehicle-maintenance")
public class VehicleMaintenanceController {
    @Autowired
    private VehicleMaintenanceService service;

    @GetMapping
    public List<VehicleMaintenance> getAll() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public ResponseEntity<VehicleMaintenance> getById(@PathVariable Long id) {
        Optional<VehicleMaintenance> result = service.findById(id);
        return result.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.notFound().build());
    }

    @PostMapping
    public VehicleMaintenance create(@RequestBody VehicleMaintenance entity) {
        return service.save(entity);
    }

    @PutMapping("/{id}")
    public ResponseEntity<VehicleMaintenance> update(@PathVariable Long id, @RequestBody VehicleMaintenance entity) {
        Optional<VehicleMaintenance> existing = service.findById(id);
        if (!existing.isPresent()) {
            return ResponseEntity.notFound().build();
        }
        VehicleMaintenance updated = service.updateFields(existing.get(), entity);
        return ResponseEntity.ok(updated);
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable Long id) {
        if (!service.findById(id).isPresent()) {
            return ResponseEntity.notFound().build();
        }
        service.deleteById(id);
        return ResponseEntity.noContent().build();
    }
}
