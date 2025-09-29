package ktc.spring_project.services;

import ktc.spring_project.entities.DeliveryProof;
import ktc.spring_project.repositories.DeliveryProofRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.security.core.Authentication;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Service
public class DeliveryProofService {
    @Autowired
    private DeliveryProofRepository deliveryProofRepository;
    @Autowired
    private ktc.spring_project.repositories.OrderRepository orderRepository;
    @Autowired
    private ktc.spring_project.repositories.UserRepository userRepository;


    public List<DeliveryProof> findAll() {
        return deliveryProofRepository.findAll();
    }

    public Optional<DeliveryProof> findById(Long id) {
        return deliveryProofRepository.findById(id);
    }

    public DeliveryProof save(DeliveryProof entities) {
        return deliveryProofRepository.save(entities);
    }

    public void delete(Long id) {
        deliveryProofRepository.deleteById(id);
    }

    public DeliveryProof createProof(
            Long orderId,
            String proofTypeStr,
            MultipartFile file,
            String recipientName,
            String recipientSignature,
            String notes,
            Authentication authentication) {
        // 1. Kiểm tra đơn hàng tồn tại
        var orderOpt = orderRepository.findById(orderId);
        if (orderOpt.isEmpty()) throw new IllegalArgumentException("Order not found");
        var order = orderOpt.get();

        // 2. Lấy user hiện tại (driver)
        String username = authentication.getName();
        var userOpt = userRepository.findByUsername(username);
        if (userOpt.isEmpty()) throw new IllegalArgumentException("User not found");
        var user = userOpt.get();

        // 3. Kiểm tra quyền driver (tùy hệ thống, ở đây chỉ kiểm tra role nếu có)
        // if (!user.getRole().equals("DRIVER")) throw new SecurityException("Not driver");

        // 4. Parse proofType (normalize input)
        ktc.spring_project.enums.ProofType proofType;
        try {
            String proofTypeNormalized = proofTypeStr.trim().toUpperCase();
            proofType = ktc.spring_project.enums.ProofType.valueOf(proofTypeNormalized);
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid proofType: " + proofTypeStr);
        }

        // 5. Lưu file nếu có
        String filePath = null;
        String fileName = null;
        if (file != null && !file.isEmpty()) {
            try {
                // Lưu file vào thư mục uploads/delivery_proofs/ trong thư mục gốc project
                String rootPath = System.getProperty("user.dir");
                String uploadDir = rootPath + java.io.File.separator + "uploads" + java.io.File.separator + "delivery_proofs" + java.io.File.separator;
                java.io.File dir = new java.io.File(uploadDir);
                if (!dir.exists()) dir.mkdirs();
                fileName = System.currentTimeMillis() + "_" + file.getOriginalFilename();
                java.io.File dest = new java.io.File(dir, fileName);
                file.transferTo(dest);
                // Lưu đường dẫn tương đối để trả về client
                filePath = "/uploads/delivery_proofs/" + fileName;
            } catch (Exception e) {
                throw new RuntimeException("File upload failed", e);
            }
        }

        // 6. Tạo entity DeliveryProof
        DeliveryProof proof = new DeliveryProof();
        proof.setOrder(order);
        proof.setProofType(proofType);
        proof.setFilePath(filePath);
        proof.setFileName(fileName);
        proof.setRecipientName(recipientName);
        proof.setRecipientSignature(recipientSignature);
        proof.setNotes(notes);
        proof.setCapturedAt(new java.sql.Timestamp(System.currentTimeMillis()));
        proof.setUploadedBy(user);

        // 7. Lưu DB
        return deliveryProofRepository.save(proof);
    }

public DeliveryProof updateProof(
        Long id,
        Map<String, Object> updateData,
        Authentication authentication) {
    // TODO: Implement the update logic
    throw new UnsupportedOperationException("Not implemented yet");
        }
    public void deleteById(Long id, Authentication authentication) {
    // TODO: Implement the logic to delete a DeliveryProof by id, possibly checking authentication
    throw new UnsupportedOperationException("Not implemented yet");
}
}
