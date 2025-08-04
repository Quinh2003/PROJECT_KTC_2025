package ktc.spring_project.controllers;

import ktc.spring_project.entities.DeliveryProof;
import ktc.spring_project.services.DeliveryOrderService;
import ktc.spring_project.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;

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
    private DeliveryOrderService deliveryOrderService;

    @Autowired
    private UserService userService;

    /**
     * Upload delivery proof (photo, signature, etc.)
     * US-DRIVER-STATUS-UPDATE-01
     */
    @PostMapping(value = "/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<DeliveryProof> uploadDeliveryProof(
            @RequestParam("orderId") Long orderId,
            @RequestParam("proofType") String proofType,
            @RequestParam(value = "file", required = false) MultipartFile file,
            @RequestParam(value = "recipientName", required = false) String recipientName,
            @RequestParam(value = "recipientSignature", required = false) String recipientSignature,
            @RequestParam(value = "notes", required = false) String notes,
            Authentication authentication) {

        DeliveryProof proof = deliveryOrderService.uploadDeliveryProof(
                orderId, proofType, file, recipientName, recipientSignature, notes, authentication);

        return new ResponseEntity<>(proof, HttpStatus.CREATED);
    }

    /**
     * Get delivery proofs for an order
     */
    @GetMapping("/order/{orderId}")
    public ResponseEntity<List<DeliveryProof>> getDeliveryProofsByOrder(@PathVariable Long orderId) {
        List<DeliveryProof> proofs = deliveryOrderService.getDeliveryProofsByOrder(orderId);
        return ResponseEntity.ok(proofs);
    }

    /**
     * Get delivery proof by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<DeliveryProof> getDeliveryProofById(@PathVariable Long id) {
        DeliveryProof proof = deliveryOrderService.getDeliveryProofById(id);
        return ResponseEntity.ok(proof);
    }

    /**
     * Delete delivery proof
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteDeliveryProof(
            @PathVariable Long id,
            Authentication authentication) {

        deliveryOrderService.deleteDeliveryProof(id, authentication);
        return ResponseEntity.noContent().build();
    }

    /**
     * Update delivery proof information
     */
    @PutMapping("/{id}")
    public ResponseEntity<DeliveryProof> updateDeliveryProof(
            @PathVariable Long id,
            @Valid @RequestBody Map<String, Object> updateData,
            Authentication authentication) {

        DeliveryProof updatedProof = deliveryOrderService.updateDeliveryProof(id, updateData, authentication);
        return ResponseEntity.ok(updatedProof);
    }
}
