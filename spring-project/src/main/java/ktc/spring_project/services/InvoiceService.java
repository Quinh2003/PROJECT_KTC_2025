package ktc.spring_project.services;
import ktc.spring_project.exceptions.HttpException;

import ktc.spring_project.entities.*;
import ktc.spring_project.enums.InvoiceStatus;
import ktc.spring_project.repositories.*;
import jakarta.persistence.EntityNotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.File;
import java.io.IOException;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

/**
 * Service xử lý logic nghiệp vụ cho hóa đơn thanh toán
 * Bao gồm: tạo hóa đơn, kiểm tra điều kiện, quản lý trạng thái, và tích hợp với hệ thống khác
 */
@Service
@Slf4j
@Transactional
public class InvoiceService {

    @Autowired
    private ElectronicInvoiceRepository invoiceRepository;

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private DeliveryRepository deliveryRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ActivityLogService activityLogService;

    @Autowired
    private EmailService emailService;
    
    @Autowired
    private PdfGenerationService pdfGenerationService;

    // Constants cho business rules
    private static final String INVOICE_NUMBER_PREFIX = "INV";
    
    // Cấu hình từ application.properties
    @Value("${invoice.expiry.days:180}")
    private int invoiceExpiryDays;

    @Value("${invoice.email.auto.send:false}")
    private boolean autoSendEmail;
    
    @Value("${invoice.pdf.storage.path:/var/invoices/pdfs}")
    private String pdfStoragePath;

    /**
     * Kiểm tra điều kiện nghiệp vụ trước khi xuất hóa đơn
     * 
     * @param orderId ID đơn hàng
     * @return ValidationResult chứa kết quả kiểm tra
     */
    public ValidationResult validateInvoiceEligibility(Long orderId) {
        log.info("Kiểm tra điều kiện xuất hóa đơn cho order ID: {}", orderId);

        // 1. Kiểm tra đơn hàng tồn tại
        Optional<Order> orderOpt = orderRepository.findById(orderId);
        if (orderOpt.isEmpty()) {
            return new ValidationResult(false, "Đơn hàng không tồn tại");
        }

        Order order = orderOpt.get();

        // 2. Kiểm tra đơn hàng đã có hóa đơn chưa
        if (invoiceRepository.existsByOrderId(orderId)) {
            return new ValidationResult(false, "Đơn hàng đã có hóa đơn thanh toán");
        }

        // 3. Kiểm tra trạng thái đơn hàng hợp lệ
        if (order.getStatus() == null) {
            return new ValidationResult(false, "Đơn hàng chưa có trạng thái");
        }

        // Giả sử trạng thái hợp lệ là PROCESSED hoặc COMPLETED
        String statusName = order.getStatus().getName();
        if (!isValidOrderStatus(statusName)) {
            return new ValidationResult(false, 
                "Trạng thái đơn hàng không hợp lệ để xuất hóa đơn: " + statusName);
        }

        // 4. Kiểm tra có bản ghi giao hàng và đã hoàn thành
        List<Delivery> deliveries = deliveryRepository.findByOrderId(orderId);
        if (deliveries.isEmpty()) {
            return new ValidationResult(false, "Đơn hàng chưa có thông tin giao hàng");
        }

        Delivery delivery = deliveries.get(0); // Lấy delivery đầu tiên
        if (delivery.getActualDeliveryTime() == null) {
            return new ValidationResult(false, "Đơn hàng chưa hoàn thành giao hàng");
        }

        // 5. Kiểm tra thời gian xuất hóa đơn (theo cấu hình invoice.expiry.days)
        Timestamp deliveryTime = delivery.getActualDeliveryTime();
        Timestamp now = new Timestamp(System.currentTimeMillis());
        long daysDiff = ChronoUnit.DAYS.between(deliveryTime.toLocalDateTime(), now.toLocalDateTime());
        
        if (daysDiff > invoiceExpiryDays) {
            return new ValidationResult(false, 
                String.format("Đã quá hạn xuất hóa đơn (quá %d ngày từ khi hoàn thành giao hàng)", 
                invoiceExpiryDays));
        }

        log.info("Đơn hàng {} đạt điều kiện xuất hóa đơn", orderId);
        return new ValidationResult(true, "Đơn hàng đạt điều kiện xuất hóa đơn");
    }

    /**
     * Tạo hóa đơn thanh toán cho đơn hàng
     * 
     * @param orderId ID đơn hàng
     * @param createdByUserId ID người tạo hóa đơn
     * @param customerEmail Email khách hàng (tùy chọn)
     * @param customerName Tên khách hàng (tùy chọn)
     * @return ElectronicInvoice đã được tạo
     */
    @Transactional
    public ElectronicInvoice createInvoice(Long orderId, Long createdByUserId, 
                                         String customerEmail, String customerName) {
        log.info("Bắt đầu tạo hóa đơn thanh toán cho order {}", orderId);

        // 1. Kiểm tra điều kiện nghiệp vụ
        ValidationResult validation = validateInvoiceEligibility(orderId);
        if (!validation.isValid()) {
            throw new HttpException("Không thể tạo hóa đơn: " + validation.getMessage(), org.springframework.http.HttpStatus.BAD_REQUEST);
        }

        // 2. Lấy thông tin đơn hàng, delivery và người tạo
        Order order = orderRepository.findById(orderId)
            .orElseThrow(() -> new EntityNotFoundException("Đơn hàng không tồn tại"));

        List<Delivery> deliveries = deliveryRepository.findByOrderId(orderId);
        if (deliveries.isEmpty()) {
            throw new EntityNotFoundException("Không tìm thấy thông tin giao hàng cho đơn hàng này");
        }
        Delivery delivery = deliveries.get(0); // Lấy delivery đầu tiên

        User createdBy = userRepository.findById(createdByUserId)
            .orElseThrow(() -> new EntityNotFoundException("Người dùng không tồn tại"));

        // 3. Sinh số hóa đơn duy nhất
        String invoiceNumber = generateInvoiceNumber();

        // 4. Lấy số tiền từ delivery fee
        BigDecimal totalAmount = delivery.getDeliveryFee();
        if (totalAmount == null) {
            throw new HttpException("Phí giao hàng chưa được thiết lập cho đơn hàng này", org.springframework.http.HttpStatus.BAD_REQUEST);
        }

        // 5. Tạo hóa đơn
        ElectronicInvoice invoice = new ElectronicInvoice(order, delivery, invoiceNumber, totalAmount, createdBy);
        
        // Set thông tin khách hàng nếu có
        if (customerEmail != null && !customerEmail.isEmpty()) {
            invoice.setCustomerEmail(customerEmail);
        }
        if (customerName != null && !customerName.isEmpty()) {
            invoice.setCustomerName(customerName);
        }

        // 6. Tính thuế VAT
        invoice.calculateTax();

        // 7. Lưu vào database
        final ElectronicInvoice savedInvoice = invoiceRepository.save(invoice);

        // 8. TỰ ĐỘNG TẠO FILE PDF
        try {
            generateInvoicePdf(savedInvoice.getId());
            log.info("✅ Đã tự động tạo PDF cho hóa đơn {}", invoiceNumber);
        } catch (Exception e) {
            log.warn("⚠️ Không thể tạo PDF tự động cho hóa đơn {}: {}", invoiceNumber, e.getMessage());
            // Không throw exception để không ảnh hưởng đến việc tạo hóa đơn
        }

        // 9. Ghi log hoạt động
        try {
            activityLogService.logUserActivity(
                createdByUserId,
                "CREATE",
                String.format("Tạo hóa đơn thanh toán %s cho đơn hàng %d", 
                    invoiceNumber, orderId)
            );
        } catch (Exception e) {
            log.warn("Không thể ghi log cho việc tạo hóa đơn: {}", e.getMessage());
        }

        // 10. TỰ ĐỘNG GỬI EMAIL nếu được bật và có email khách hàng
        if (autoSendEmail && customerEmail != null && !customerEmail.trim().isEmpty()) {
            log.info("🔄 Tự động gửi email cho hóa đơn {} đến {}", invoiceNumber, customerEmail);
            
            // Gửi email async để không block process tạo hóa đơn
            CompletableFuture.runAsync(() -> {
                try {
                    // Đợi 1 giây để đảm bảo transaction đã commit
                    Thread.sleep(1000);
                    
                    boolean emailSent = emailService.sendInvoiceEmail(savedInvoice, customerEmail);
                    
                    if (emailSent) {
                        // Cập nhật trạng thái email đã gửi
                        savedInvoice.markAsSent(customerEmail);
                        invoiceRepository.save(savedInvoice);
                        
                        log.info("✅ Đã tự động gửi email thành công cho hóa đơn {}", invoiceNumber);
                    } else {
                        log.warn("⚠️ Tự động gửi email thất bại cho hóa đơn {}", invoiceNumber);
                    }
                } catch (Exception e) {
                    log.error("🚨 Lỗi khi tự động gửi email cho hóa đơn {}: ", invoiceNumber, e);
                }
            });
        }

        log.info("Đã tạo thành công hóa đơn {} cho đơn hàng {}", invoiceNumber, orderId);
        return savedInvoice;
    }

    /**
     * Tạo file PDF cho hóa đơn
     * 
     * @param invoiceId ID hóa đơn
     * @return đường dẫn file PDF đã tạo
     */
    @Transactional
    public String generateInvoicePdf(Long invoiceId) {
        log.info("Tạo file PDF cho hóa đơn ID: {}", invoiceId);

        ElectronicInvoice invoice = getInvoiceById(invoiceId);
        
        // Sinh tên file PDF 
        String fileName = invoice.generatePdfFileName();
        String filePath = "invoices/pdfs/" + fileName;

        // Tạo thư mục lưu trữ nếu chưa có
        File storageDir = new File(pdfStoragePath);
        if (!storageDir.exists()) {
            boolean created = storageDir.mkdirs();
            if (created) {
                log.info("Đã tạo thư mục lưu PDF: {}", pdfStoragePath);
            }
        }
        
        // Tạo file PDF thật sự bằng OpenPDF
        File pdfFile = new File(pdfStoragePath, fileName);
        try {
            pdfGenerationService.generatePdfFromInvoice(invoice, pdfFile);
            log.info("✅ Đã tạo file PDF thật sự: {}", pdfFile.getAbsolutePath());
        } catch (IOException e) {
            log.error("❌ Lỗi tạo file PDF cho hóa đơn {}: ", invoice.getInvoiceNumber(), e);
            throw new HttpException("Không thể tạo file PDF: " + e.getMessage(), org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
        
        // Cập nhật thông tin file trong database
        invoice.setPdfFileName(fileName);
        invoice.setPdfFilePath(filePath);
        invoiceRepository.save(invoice);

        log.info("Đã tạo file PDF: {} cho hóa đơn {}", filePath, invoice.getInvoiceNumber());
        return filePath;
    }

    /**
     * Gửi hóa đơn qua email (MANUAL TRIGGER từ API)
     * 
     * @param invoiceId ID hóa đơn
     * @param emailAddress địa chỉ email nhận
     */
    @Transactional
    public void sendInvoiceByEmail(Long invoiceId, String emailAddress) {
        log.info("📧 MANUAL: Gửi hóa đơn ID {} qua email: {}", invoiceId, emailAddress);

        ElectronicInvoice invoice = getInvoiceById(invoiceId);

        if (invoice.getInvoiceStatus() == InvoiceStatus.CANCELLED) {
            throw new HttpException("Không thể gửi hóa đơn đã bị hủy", org.springframework.http.HttpStatus.BAD_REQUEST);
        }

        // GỬI EMAIL THỰC SỰ thông qua EmailService
        boolean emailSent = emailService.sendInvoiceEmail(invoice, emailAddress);
        
        if (!emailSent) {
            throw new HttpException("Không thể gửi email hóa đơn. Vui lòng kiểm tra cấu hình email.", org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
        }
        
        // Cập nhật trạng thái đã gửi chỉ khi email thành công
        invoice.markAsSent(emailAddress);
        invoiceRepository.save(invoice);

        // Ghi log
        try {
            activityLogService.logUserActivity(
                invoice.getCreatedBy().getId(),
                "UPDATE",
                String.format("Gửi hóa đơn %s qua email %s", 
                    invoice.getInvoiceNumber(), emailAddress)
            );
        } catch (Exception e) {
            log.warn("Không thể ghi log cho việc gửi hóa đơn: {}", e.getMessage());
        }

        log.info("✅ MANUAL: Đã gửi thành công hóa đơn {} qua email {}", 
            invoice.getInvoiceNumber(), emailAddress);
    }

    /**
     * Hủy hóa đơn
     * 
     * @param invoiceId ID hóa đơn
     * @param cancelledByUserId ID người hủy
     * @param reason lý do hủy
     */
    @Transactional
    public void cancelInvoice(Long invoiceId, Long cancelledByUserId, String reason) {
        log.info("Hủy hóa đơn ID {} bởi user {}, lý do: {}", invoiceId, cancelledByUserId, reason);

        ElectronicInvoice invoice = getInvoiceById(invoiceId);
        User cancelledBy = userRepository.findById(cancelledByUserId)
            .orElseThrow(() -> new EntityNotFoundException("Người dùng không tồn tại"));

        invoice.cancel(cancelledBy, reason);
        invoiceRepository.save(invoice);

        // Ghi log
        try {
            activityLogService.logUserActivity(
                cancelledByUserId,
                "DELETE",
                String.format("Hủy hóa đơn %s, lý do: %s", 
                    invoice.getInvoiceNumber(), reason)
            );
        } catch (Exception e) {
            log.warn("Không thể ghi log cho việc hủy hóa đơn: {}", e.getMessage());
        }

        log.info("Đã hủy hóa đơn {}", invoice.getInvoiceNumber());
    }

    /**
     * Lấy danh sách đơn hàng cần xuất hóa đơn (job tự động)
     * 
     * @return List<Order> các đơn hàng cần xuất hóa đơn
     */
    public List<Order> getOrdersNeedingInvoice() {
        // Tìm các đơn hàng hoàn thành giao hàng > 3 ngày nhưng chưa có hóa đơn
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(3);
        Timestamp cutoffTimestamp = Timestamp.valueOf(cutoffDate);
        
        return invoiceRepository.findOrdersNeedingInvoice(cutoffTimestamp);
    }

    /**
     * Lấy hóa đơn theo ID
     */
    public ElectronicInvoice getInvoiceById(Long id) {
        return invoiceRepository.findById(id)
            .orElseThrow(() -> new EntityNotFoundException("Hóa đơn không tồn tại với ID: " + id));
    }

    /**
     * Lấy hóa đơn theo đơn hàng
     */
    public Optional<ElectronicInvoice> getInvoiceByOrderId(Long orderId) {
        return invoiceRepository.findByOrderId(orderId);
    }

    /**
     * Lấy danh sách hóa đơn theo trạng thái
     */
    public List<ElectronicInvoice> getInvoicesByStatus(InvoiceStatus status) {
        return invoiceRepository.findByStatusOrderByIssuedAtDesc(status);
    }

    // ============ PRIVATE HELPER METHODS ============

    /**
     * Kiểm tra trạng thái đơn hàng có hợp lệ để xuất hóa đơn không
     */
    private boolean isValidOrderStatus(String statusName) {
        return "COMPLETED".equalsIgnoreCase(statusName);
        // "DELIVERED".equalsIgnoreCase(statusName) || 
        //        "COMPLETED".equalsIgnoreCase(statusName) ||
        //        "SHIPPED".equalsIgnoreCase(statusName);  // Chỉ chấp nhận DELIVERED, COMPLETED, SHIPPED £201
    }

    /**
     * Sinh số hóa đơn duy nhất
     */
    private String generateInvoiceNumber() {
        // Format: INV-YYYYMMDD-HHMMSS-XXX
        LocalDateTime now = LocalDateTime.now();
        String timestamp = now.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"));
        
        // Thêm số sequence để đảm bảo duy nhất
        long count = invoiceRepository.count() + 1;
        String sequence = String.format("%03d", count % 1000);
        
        return String.format("%s-%s-%s", INVOICE_NUMBER_PREFIX, timestamp, sequence);
    }

    /**
     * Tính tổng số tiền hóa đơn từ đơn hàng (DEPRECATED: Giờ sử dụng delivery fee)
     */
    @Deprecated
    private BigDecimal calculateTotalAmount(Order order) {
        BigDecimal totalAmount = order.getTotalAmount();
        
        if (totalAmount == null || totalAmount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new HttpException("Đơn hàng không có giá trị hợp lệ", org.springframework.http.HttpStatus.BAD_REQUEST);
        }
        
        return totalAmount;
    }
    


    // ============ INNER CLASSES ============

    /**
     * Class chứa kết quả validation
     */
    public static class ValidationResult {
        private final boolean valid;
        private final String message;

        public ValidationResult(boolean valid, String message) {
            this.valid = valid;
            this.message = message;
        }

        public boolean isValid() { return valid; }
        public String getMessage() { return message; }
    }
}
