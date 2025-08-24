package ktc.spring_project.services;

import ktc.spring_project.entities.ElectronicInvoice;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.stereotype.Service;

// Email imports - sẽ được resolve sau khi dependencies load
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;

// Thymeleaf imports cho template engine
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.Context;

// Jakarta Mail imports
import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import java.io.File;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.CompletableFuture;

/**
 * Service xử lý gửi email hóa đơn điện tử
 * 
 * Chức năng chính:
 * - Gửi email hóa đơn có attachment PDF
 * - Sử dụng HTML template đẹp mắt
 * - Hỗ trợ gửi email async để không block UI
 * - Xử lý lỗi và logging chi tiết
 */
@Service
@Slf4j
public class EmailService {

    @Autowired
    private JavaMailSender mailSender;

    @Autowired
    private TemplateEngine templateEngine;

    // ============ CẤU HÌNH EMAIL TỪ APPLICATION.PROPERTIES ============
    
    @Value("${invoice.email.from:noreply@ktc-logistics.com}")
    private String fromEmail;

    @Value("${invoice.email.enabled:true}")
    private boolean emailEnabled;

    @Value("${invoice.email.subject.prefix:KTC Logistics - }")
    private String subjectPrefix;

    @Value("${invoice.pdf.storage.path:/var/invoices/pdfs}")
    private String pdfStoragePath;

    // ============ THÔNG TIN CÔNG TY ============
    
    @Value("${company.name:KTC Logistics}")
    private String companyName;

    @Value("${company.phone:+84-915-101-664}")
    private String companyPhone;

    @Value("${company.email:support@ktc-logistics.com}")
    private String companyEmail;

    @Value("${company.website:https://ktc-logistics.com}")
    private String companyWebsite;

    @Value("${company.address: 32 Phi Binh 8, Ngu Hanh Son, Da Nang}")
    private String companyAddress;

    /**
     * Gửi email hóa đơn điện tử (ASYNC - không block UI)
     * 
     * @param invoice Hóa đơn cần gửi
     * @param recipientEmail Email người nhận
     * @return CompletableFuture<Boolean> - true nếu gửi thành công
     */
    public CompletableFuture<Boolean> sendInvoiceEmailAsync(ElectronicInvoice invoice, String recipientEmail) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                log.info("Bắt đầu gửi email ASYNC cho hóa đơn {} đến {}", 
                    invoice.getInvoiceNumber(), recipientEmail);
                
                boolean result = sendInvoiceEmail(invoice, recipientEmail);
                
                if (result) {
                    log.info("✅ Gửi email ASYNC thành công cho hóa đơn {}", invoice.getInvoiceNumber());
                } else {
                    log.error("❌ Gửi email ASYNC thất bại cho hóa đơn {}", invoice.getInvoiceNumber());
                }
                
                return result;
                
            } catch (Exception e) {
                log.error("🚨 Lỗi khi gửi email ASYNC cho hóa đơn {}: ", invoice.getInvoiceNumber(), e);
                return false;
            }
        });
    }

    /**
     * Gửi email hóa đơn điện tử (SYNC - block until complete)
     * 
     * @param invoice Hóa đơn cần gửi
     * @param recipientEmail Email người nhận
     * @return true nếu gửi thành công
     */
    public boolean sendInvoiceEmail(ElectronicInvoice invoice, String recipientEmail) {
        // Kiểm tra email service có được bật không
        if (!emailEnabled) {
            log.warn("⚠️ Email service đã bị DISABLE, bỏ qua gửi email cho hóa đơn {}", 
                invoice.getInvoiceNumber());
            return false;
        }

        // Validate input
        if (invoice == null || recipientEmail == null || recipientEmail.trim().isEmpty()) {
            log.error("❌ Dữ liệu đầu vào không hợp lệ: invoice={}, email={}", 
                invoice, recipientEmail);
            return false;
        }

        try {
            log.info("📧 Bắt đầu gửi email hóa đơn {} đến {}", 
                invoice.getInvoiceNumber(), recipientEmail);

            // Bước 1: Tạo email message
            MimeMessage message = mailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(message, true, "UTF-8");

            // Bước 2: Set thông tin email cơ bản
            helper.setFrom(fromEmail);
            helper.setTo(recipientEmail.trim());
            helper.setSubject(subjectPrefix + "Hóa đơn điện tử " + invoice.getInvoiceNumber());

            // Bước 3: Tạo HTML content từ template
            String htmlContent = generateEmailContent(invoice);
            helper.setText(htmlContent, true); // true = HTML content

            // Bước 4: Attach PDF file nếu có
            attachPdfFileIfExists(helper, invoice);

            // Bước 5: Gửi email
            mailSender.send(message);
            
            log.info("✅ Đã gửi THÀNH CÔNG email hóa đơn {} đến {}", 
                invoice.getInvoiceNumber(), recipientEmail);
            return true;

        } catch (MessagingException e) {
            log.error("❌ Lỗi MessagingException khi gửi email hóa đơn {}: ", 
                invoice.getInvoiceNumber(), e);
            return false;
            
        } catch (Exception e) {
            log.error("🚨 Lỗi không xác định khi gửi email hóa đơn {}: ", 
                invoice.getInvoiceNumber(), e);
            return false;
        }
    }

    /**
     * Attach file PDF vào email nếu file tồn tại
     */
    private void attachPdfFileIfExists(MimeMessageHelper helper, ElectronicInvoice invoice) 
            throws MessagingException {
        
        if (invoice.getPdfFilePath() == null || invoice.getPdfFilePath().isEmpty()) {
            log.info("📄 Không có file PDF để attach cho hóa đơn {}", invoice.getInvoiceNumber());
            return;
        }

        // Tạo đường dẫn tuyệt đối đến file PDF
        File pdfFile = new File(pdfStoragePath, invoice.getPdfFileName());
        
        if (pdfFile.exists() && pdfFile.isFile()) {
            FileSystemResource fileResource = new FileSystemResource(pdfFile);
            helper.addAttachment(invoice.getPdfFileName(), fileResource);
            
            log.info("📎 Đã attach file PDF: {} (size: {} KB)", 
                invoice.getPdfFileName(), pdfFile.length() / 1024);
        } else {
            log.warn("⚠️ Không tìm thấy file PDF: {}", pdfFile.getAbsolutePath());
        }
    }

    /**
     * Tạo HTML content cho email từ Thymeleaf template
     * 
     * @param invoice Hóa đơn
     * @return HTML content
     */
    private String generateEmailContent(ElectronicInvoice invoice) {
        try {
            Context context = new Context();
            
            // ============ THÔNG TIN HÓA ĐƠN ============
            context.setVariable("invoiceNumber", invoice.getInvoiceNumber());
            context.setVariable("customerName", 
                invoice.getCustomerName() != null ? invoice.getCustomerName() : "Quý khách");
            context.setVariable("totalAmount", formatCurrency(invoice.getTotalAmount()));
            context.setVariable("taxAmount", formatCurrency(invoice.getTaxAmount()));
            context.setVariable("netAmount", formatCurrency(invoice.getNetAmount()));
            context.setVariable("issuedDate", formatDate(invoice.getIssuedAt()));
            
            // ============ THÔNG TIN ĐƠN HÀNG ============
            if (invoice.getOrder() != null) {
                context.setVariable("orderId", invoice.getOrder().getId());
                context.setVariable("orderDescription", 
                    invoice.getOrder().getDescription() != null ? 
                    invoice.getOrder().getDescription() : "Đơn hàng logistics");
            } else {
                context.setVariable("orderId", "N/A");
                context.setVariable("orderDescription", "Thông tin đơn hàng không có");
            }
            
            // ============ THÔNG TIN CÔNG TY ============
            context.setVariable("companyName", companyName);
            context.setVariable("companyPhone", companyPhone);
            context.setVariable("companyEmail", companyEmail);
            context.setVariable("companyWebsite", companyWebsite);
            context.setVariable("companyAddress", companyAddress);
            
            // ============ THÔNG TIN KHÁC ============
            context.setVariable("hasPdfAttachment", 
                invoice.getPdfFilePath() != null && !invoice.getPdfFilePath().isEmpty());
            context.setVariable("currentYear", java.time.LocalDate.now().getYear());
            context.setVariable("invoiceStatusDisplay", invoice.getInvoiceStatus().getDisplayName());

            // Render template thành HTML
            return templateEngine.process("invoice-email", context);
            
        } catch (Exception e) {
            log.error("❌ Lỗi khi tạo HTML content cho email: ", e);
            
            // Fallback: Tạo email đơn giản nếu template lỗi
            return createFallbackEmailContent(invoice);
        }
    }

    /**
     * Tạo email content đơn giản khi template bị lỗi
     */
    private String createFallbackEmailContent(ElectronicInvoice invoice) {
        StringBuilder sb = new StringBuilder();
        sb.append("<html><body>");
        sb.append("<h2>Hóa đơn điện tử ").append(companyName).append("</h2>");
        sb.append("<p>Kính chào ").append(invoice.getCustomerName() != null ? 
            invoice.getCustomerName() : "Quý khách").append(",</p>");
        sb.append("<p>Chúng tôi gửi đến Quý khách hóa đơn điện tử:</p>");
        sb.append("<ul>");
        sb.append("<li><strong>Số hóa đơn:</strong> ").append(invoice.getInvoiceNumber()).append("</li>");
        sb.append("<li><strong>Tổng tiền:</strong> ").append(formatCurrency(invoice.getTotalAmount())).append("</li>");
        sb.append("</ul>");
        sb.append("<p>Trân trọng,<br>").append(companyName).append("</p>");
        sb.append("</body></html>");
        
        return sb.toString();
    }

    /**
     * Kiểm tra email service có hoạt động không
     */
    public boolean isEmailServiceAvailable() {
        return emailEnabled && mailSender != null;
    }

    /**
     * Gửi email test để kiểm tra cấu hình
     */
    public boolean sendTestEmail(String testEmail) {
        if (!emailEnabled) {
            log.warn("Email service đã bị disable");
            return false;
        }

        try {
            MimeMessage message = mailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(message, false, "UTF-8");

            helper.setFrom(fromEmail);
            helper.setTo(testEmail);
            helper.setSubject("Test Email - KTC Logistics System");
            helper.setText("Đây là email test từ hệ thống KTC Logistics. Nếu bạn nhận được email này, " +
                "cấu hình email đã hoạt động bình thường.", false);

            mailSender.send(message);
            log.info("✅ Gửi test email thành công đến {}", testEmail);
            return true;

        } catch (Exception e) {
            log.error("❌ Lỗi khi gửi test email: ", e);
            return false;
        }
    }

    // ============ HELPER METHODS ============

    /**
     * Format tiền tệ Việt Nam
     */
    private String formatCurrency(BigDecimal amount) {
        if (amount == null) return "0 VND";
        DecimalFormat formatter = new DecimalFormat("#,##0.00");
        return formatter.format(amount) + " VND";
    }

    /**
     * Format ngày tháng theo định dạng Việt Nam
     */
    private String formatDate(java.sql.Timestamp timestamp) {
        if (timestamp == null) return "";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy 'lúc' HH:mm");
        return timestamp.toLocalDateTime().format(formatter);
    }
}
