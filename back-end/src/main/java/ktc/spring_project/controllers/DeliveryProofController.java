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

    /**
     * Upload delivery proof (photo, signature, etc.)
     * US-DRIVER-STATUS-UPDATE-01
     * TO-DO: Implement file handling, authentication check, and order validation
     */
    @PostMapping(value = "/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<DeliveryProof> uploadDeliveryProof(
            @RequestParam("orderId") Long orderId,
            @RequestParam("proofType") String proofTypeStr,
            @RequestParam(value = "file", required = false) MultipartFile file,
            @RequestParam(value = "recipientName", required = false) String recipientName,
            @RequestParam(value = "recipientSignature", required = false) String recipientSignature,
            @RequestParam(value = "notes", required = false) String notes,
            Authentication authentication) {

        // TO-DO: This is a temporary implementation.
        // In the future, this will handle file uploads, save them to appropriate storage,
        // and create proper delivery proof records in the database

        // Get the order entity
        Optional<Order> orderOptional = Optional.ofNullable(orderService.getOrderById(orderId));
        if (orderOptional.isEmpty()) {
            return ResponseEntity.badRequest().build();
        }

        // Convert string to ProofType enum
        ProofType proofType;
        try {
            proofType = ProofType.valueOf(proofTypeStr.toUpperCase());
        } catch (IllegalArgumentException e) {
            // Default to PHOTO if invalid type provided
            proofType = ProofType.PHOTO;
        }

        // Create a basic delivery proof entity with the provided information
        DeliveryProof proof = new DeliveryProof();
        proof.setOrder(orderOptional.get());
        proof.setProofType(proofType);
        proof.setRecipientName(recipientName);
        proof.setRecipientSignature(recipientSignature);
        proof.setNotes(notes);
        proof.setCapturedAt(Timestamp.from(Instant.now()));

        // TO-DO: Handle file upload and storage
        // TO-DO: Set the file path and name

        // TO-DO: Set the user who uploaded this proof
        // proof.setUploadedBy(userService.getUserFromAuthentication(authentication));

        // Save the proof to the database
        return new ResponseEntity<>(deliveryProofService.save(proof), HttpStatus.CREATED);
    }

    /**
     * Get delivery proofs for an order
     * TO-DO: Implement findByOrder method in DeliveryProofRepository
     */
    @GetMapping("/order/{orderId}")
    public ResponseEntity<List<DeliveryProof>> getDeliveryProofsByOrder(@PathVariable Long orderId) {
        // TO-DO: This is a temporary implementation.
        // In the future, implement findByOrder in the repository and service

        // Get the order entity
        try {
            Order order = orderService.getOrderById(orderId);

            // For now, return all proofs and filter manually (inefficient, for development only)
            List<DeliveryProof> allProofs = deliveryProofService.findAll();
            List<DeliveryProof> orderProofs = new ArrayList<>();

            for (DeliveryProof proof : allProofs) {
                if (proof.getOrder() != null && proof.getOrder().getId().equals(orderId)) {
                    orderProofs.add(proof);
                }
            }

            return ResponseEntity.ok(orderProofs);
        } catch (EntityNotFoundException e) {
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * Get delivery proof by ID
     */
    @GetMapping("/{id}")
    public ResponseEntity<DeliveryProof> getDeliveryProofById(@PathVariable Long id) {
        Optional<DeliveryProof> proofOptional = deliveryProofService.findById(id);

        if (proofOptional.isPresent()) {
            return ResponseEntity.ok(proofOptional.get());
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * Delete delivery proof
     * TO-DO: Add permission checks before deletion
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteDeliveryProof(
            @PathVariable Long id,
            Authentication authentication) {

        // TO-DO: Add permission checks - only admin or original uploader should delete
        // TO-DO: Add authentication validation

        // Check if proof exists before deletion
        Optional<DeliveryProof> proofOptional = deliveryProofService.findById(id);
        if (proofOptional.isEmpty()) {
            return ResponseEntity.notFound().build();
        }

        deliveryProofService.delete(id);
        return ResponseEntity.noContent().build();
    }

    /**
     * Update delivery proof information
     * TO-DO: Implement full update capability with security checks
     */
    @PutMapping("/{id}")
    public ResponseEntity<DeliveryProof> updateDeliveryProof(
            @PathVariable Long id,
            @Valid @RequestBody Map<String, Object> updateData,
            Authentication authentication) {

        // TO-DO: This is a temporary implementation.
        // In the future, implement proper update logic with permission checks

        // Check if proof exists
        Optional<DeliveryProof> proofOptional = deliveryProofService.findById(id);
        if (proofOptional.isEmpty()) {
            return ResponseEntity.notFound().build();
        }

        // Get the existing proof
        DeliveryProof proof = proofOptional.get();

        // TO-DO: Add permission checks - only admin or original uploader should update

        // Update fields based on provided data
        if (updateData.containsKey("recipientName")) {
            proof.setRecipientName((String) updateData.get("recipientName"));
        }

        if (updateData.containsKey("notes")) {
            proof.setNotes((String) updateData.get("notes"));
        }

        // TO-DO: Handle updates to other fields including file replacement if needed

        // Save the updated proof
        return ResponseEntity.ok(deliveryProofService.save(proof));
    }
}
