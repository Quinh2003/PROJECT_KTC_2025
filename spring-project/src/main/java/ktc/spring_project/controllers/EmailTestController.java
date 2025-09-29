package ktc.spring_project.controllers;

import ktc.spring_project.dtos.common.ApiResponse;
import ktc.spring_project.services.EmailService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * Controller để test chức năng email
 * CHỈ sử dụng trong môi trường development/testing
 */
@RestController
@RequestMapping("/api/test/email")
@Slf4j
public class EmailTestController {

    @Autowired
    private EmailService emailService;

    /**
     * Test gửi email đơn giản để kiểm tra cấu hình
     * GET /api/test/email/send-test?email=test@example.com
     */
    @GetMapping("/send-test")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ApiResponse<String>> sendTestEmail(@RequestParam String email) {
        try {
            log.info("🧪 TEST: Gửi test email đến: {}", email);
            
            boolean result = emailService.sendTestEmail(email);
            
            if (result) {
                return ResponseEntity.ok(ApiResponse.success(
                    "Email test đã được gửi thành công đến: " + email,
                    "Test email thành công"
                ));
            } else {
                return ResponseEntity.badRequest().body(ApiResponse.error(
                    "Không thể gửi test email. Kiểm tra cấu hình email."
                ));
            }
            
        } catch (Exception e) {
            log.error("Lỗi khi gửi test email: ", e);
            return ResponseEntity.internalServerError().body(ApiResponse.error(
                "Lỗi hệ thống: " + e.getMessage()
            ));
        }
    }

    /**
     * Kiểm tra trạng thái email service
     * GET /api/test/email/status
     */
    @GetMapping("/status")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ApiResponse<Object>> checkEmailServiceStatus() {
        try {
            boolean isAvailable = emailService.isEmailServiceAvailable();
            
            java.util.Map<String, Object> status = new java.util.HashMap<>();
            status.put("available", isAvailable);
            status.put("timestamp", new java.util.Date());
            status.put("message", isAvailable ? 
                "Email service đang hoạt động bình thường" : 
                "Email service không khả dụng");
            
            return ResponseEntity.ok(ApiResponse.success(status, "Kiểm tra trạng thái email service"));
            
        } catch (Exception e) {
            log.error("Lỗi khi kiểm tra email service: ", e);
            return ResponseEntity.internalServerError().body(ApiResponse.error(
                "Lỗi hệ thống: " + e.getMessage()
            ));
        }
    }
}

