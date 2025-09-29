package ktc.spring_project.controllers;

import ktc.spring_project.entities.DeliveryProof;
import ktc.spring_project.entities.Order;
import ktc.spring_project.enums.ProofType;
import ktc.spring_project.services.DeliveryProofService;
import ktc.spring_project.services.OrderService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import jakarta.persistence.EntityNotFoundException;
import jakarta.validation.Valid;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Controller responsible for managing delivery proofs
 * Based on user stories:
 * - US-DRIVER-STATUS-UPDATE-01: Update Delivery Status (with photos and signatures)
 * - Handling proof of delivery documentation
 */
@RestController
@RequestMapping("/api/delivery-proofs")
public class DeliveryProofController {

    @Autowired
    private DeliveryProofService deliveryProofService;

    @Autowired
    private UserService userService;

    @Autowired
    private OrderService orderService;

    // Lấy tất cả delivery proofs
@GetMapping
public ResponseEntity<List<DeliveryProof>> getAllDeliveryProofs() {
    return ResponseEntity.ok(deliveryProofService.findAll());
}

// Lấy delivery proof theo ID
@GetMapping("/{id}")
public ResponseEntity<DeliveryProof> getDeliveryProofById(@PathVariable Long id) {
    Optional<DeliveryProof> proofOptional = deliveryProofService.findById(id);
    return proofOptional.map(ResponseEntity::ok).orElse(ResponseEntity.notFound().build());
}

// Tạo mới delivery proof (POST)
@PostMapping(value = "/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
public ResponseEntity<DeliveryProof> uploadDeliveryProof(
        @RequestParam("orderId") Long orderId,
        @RequestParam("proofType") String proofTypeStr,
        @RequestParam(value = "file", required = false) MultipartFile file,
        @RequestParam(value = "recipientName", required = false) String recipientName,
        @RequestParam(value = "recipientSignature", required = false) String recipientSignature,
        @RequestParam(value = "notes", required = false) String notes,
        Authentication authentication) {
    try {
        DeliveryProof proof = deliveryProofService.createProof(
            orderId, proofTypeStr, file, recipientName, recipientSignature, notes, authentication
        );
        return ResponseEntity.ok(proof);
    } catch (Exception e) {
        e.printStackTrace(); // Log lỗi chi tiết ra console
        return ResponseEntity.badRequest().body(null);
        // Nếu muốn trả về message rõ ràng:
        // return ResponseEntity.badRequest().body(e.getMessage());
    }
}

// Cập nhật delivery proof theo ID (PUT)
@PutMapping("/{id}")
public ResponseEntity<DeliveryProof> updateDeliveryProof(
        @PathVariable Long id,
        @Valid @RequestBody Map<String, Object> updateData,
        Authentication authentication) {
    try {
        DeliveryProof updatedProof = deliveryProofService.updateProof(id, updateData, authentication);
        if (updatedProof != null) {
            return ResponseEntity.ok(updatedProof);
        } else {
            return ResponseEntity.notFound().build();
        }
    } catch (Exception e) {
        return ResponseEntity.badRequest().build();
    }
}

// Xóa delivery proof theo ID (DELETE)
@DeleteMapping("/{id}")
public ResponseEntity<Void> deleteDeliveryProof(
        @PathVariable Long id,
        Authentication authentication) {
    try {
        deliveryProofService.deleteById(id, authentication);
        return ResponseEntity.noContent().build(); // 204 No Content on successful delete
    } catch (EntityNotFoundException e) {
        return ResponseEntity.notFound().build();
    } catch (Exception e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
    }
}

    /**
     * Get delivery proofs for an order
     * TO-DO: Implement findByOrder method in DeliveryProofRepository
     */
        @GetMapping("/order/{orderId}")
        public ResponseEntity<List<DeliveryProof>> getDeliveryProofsByOrder(@PathVariable Long orderId) {
            List<DeliveryProof> orderProofs = deliveryProofService.findByOrderId(orderId);
            return ResponseEntity.ok(orderProofs);
        }

    /**
     * Serve delivery proof image file with authentication
     * This endpoint provides authenticated access to proof images
     * @param filename The filename of the image to retrieve
     * @return The image file as a byte array with appropriate content type
     */
    @GetMapping("/files/{filename:.+}")
    public ResponseEntity<byte[]> getDeliveryProofFile(@PathVariable String filename) {
        try {
            // Construct file path
            String rootPath = System.getProperty("user.dir");
            String filePath = rootPath + java.io.File.separator + "uploads" + 
                              java.io.File.separator + "delivery_proofs" + 
                              java.io.File.separator + filename;
            
            java.io.File file = new java.io.File(filePath);
            if (!file.exists()) {
                return ResponseEntity.notFound().build();
            }
            
            // Read file bytes
            byte[] fileContent = java.nio.file.Files.readAllBytes(file.toPath());
            
            // Determine content type
            String contentType = determineContentType(filename);
            
            // Return file with proper content type
            return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(contentType))
                .body(fileContent);
                
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }
    
    /**
     * Helper method to determine content type from filename
     * @param filename The file name to analyze
     * @return Content type string (MIME type)
     */
    private String determineContentType(String filename) {
        if (filename.toLowerCase().endsWith(".jpg") || filename.toLowerCase().endsWith(".jpeg")) {
            return "image/jpeg";
        } else if (filename.toLowerCase().endsWith(".png")) {
            return "image/png";
        } else if (filename.toLowerCase().endsWith(".gif")) {
            return "image/gif";
        } else if (filename.toLowerCase().endsWith(".pdf")) {
            return "application/pdf";
        }
        // Default to binary stream if unknown
        return "application/octet-stream";
    }
}
