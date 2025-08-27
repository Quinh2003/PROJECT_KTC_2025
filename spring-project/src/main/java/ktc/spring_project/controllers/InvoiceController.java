package ktc.spring_project.controllers;

import ktc.spring_project.dtos.common.ApiResponse;
import ktc.spring_project.dtos.invoice.CreateInvoiceRequestDTO;
import ktc.spring_project.dtos.invoice.InvoiceResponseDTO;
import ktc.spring_project.dtos.invoice.CancelInvoiceRequestDTO;
import ktc.spring_project.entities.ElectronicInvoice;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.User;
import ktc.spring_project.enums.InvoiceStatus;
import ktc.spring_project.services.InvoiceService;
import ktc.spring_project.services.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;

import java.io.File;

import jakarta.validation.Valid;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Value;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Controller quản lý hóa đơn điện tử
 * Bao gồm các chức năng:
 * - Tạo hóa đơn điện tử từ đơn hàng đã hoàn thành giao hàng
 * - Xem danh sách và chi tiết hóa đơn
 * - Gửi hóa đơn qua email
 * - Hủy hóa đơn (với quyền hạn)
 * - Tải file PDF hóa đơn
 */
@RestController
@RequestMapping("/api/invoices")
@Slf4j
public class InvoiceController {

    @Autowired
    private InvoiceService invoiceService;

    @Autowired
    private UserService userService;
    
    @Value("${invoice.pdf.storage.path:/var/invoices/pdfs}")
    private String pdfStoragePath;

    /**
     * Kiểm tra điều kiện xuất hóa đơn cho đơn hàng
     * GET /api/invoices/check-eligibility/{orderId}
     */
    @GetMapping("/check-eligibility/{orderId}")
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> checkInvoiceEligibility(
            @PathVariable Long orderId) {
        try {
            log.info("Kiểm tra điều kiện xuất hóa đơn cho order {}", orderId);
            
            InvoiceService.ValidationResult result = invoiceService.validateInvoiceEligibility(orderId);
            
            Map<String, Object> responseData = new HashMap<>();
            responseData.put("orderId", orderId);
            responseData.put("eligible", result.isValid());
            responseData.put("message", result.getMessage());
            
            return ResponseEntity.ok(ApiResponse.success(responseData, "Kiểm tra điều kiện thành công"));
            
        } catch (Exception e) {
            log.error("Lỗi khi kiểm tra điều kiện xuất hóa đơn: ", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ApiResponse.error("Lỗi hệ thống: " + e.getMessage()));
        }
    }

    /**
     * Tạo hóa đơn điện tử mới
     * POST /api/invoices
     */
    @PostMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER', 'OPERATIONS')")
    public ResponseEntity<ApiResponse<InvoiceResponseDTO>> createInvoice(
            @Valid @RequestBody CreateInvoiceRequestDTO request,
            Authentication authentication) {
        try {
            log.info("Tạo hóa đơn điện tử cho order {}", request.getOrderId());
            
            // Làm sạch dữ liệu đầu vào
            request.sanitize();
            
            // Lấy thông tin user hiện tại
            User currentUser = userService.getCurrentUser(authentication);
            
            // Tạo hóa đơn
            ElectronicInvoice invoice = invoiceService.createInvoice(
                request.getOrderId(),
                currentUser.getId(),
                request.getCustomerEmail(),
                request.getCustomerName()
            );
            
            // Tự động tạo PDF
            try {
                invoiceService.generateInvoicePdf(invoice.getId());
            } catch (Exception e) {
                log.warn("Không thể tạo PDF cho hóa đơn {}: {}", invoice.getId(), e.getMessage());
            }
            
            InvoiceResponseDTO responseDTO = new InvoiceResponseDTO(invoice);
            
            return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.success(responseDTO, "Tạo hóa đơn điện tử thành công"));
                
        } catch (IllegalArgumentException e) {
            log.warn("Dữ liệu không hợp lệ khi tạo hóa đơn: {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                .body(ApiResponse.error(e.getMessage()));
                
        } catch (Exception e) {
            log.error("Lỗi khi tạo hóa đơn điện tử: ", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ApiResponse.error("Lỗi hệ thống: " + e.getMessage()));
        }
    }

    /**
     * Lấy danh sách tất cả hóa đơn
     * GET /api/invoices
     */
    @GetMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER', 'VIEWER')")
    public ResponseEntity<ApiResponse<List<InvoiceResponseDTO>>> getAllInvoices(
            @RequestParam(required = false) String status,
            @RequestParam(required = false) Long orderId,
            @RequestParam(required = false) String customerEmail) {
        try {
            log.info("Lấy danh sách hóa đơn với filter - status: {}, orderId: {}, email: {}", 
                status, orderId, customerEmail);
            
            List<ElectronicInvoice> invoices;
            
            // Apply filters
            if (status != null && !status.isEmpty()) {
                try {
                    InvoiceStatus invoiceStatus = InvoiceStatus.valueOf(status.toUpperCase());
                    invoices = invoiceService.getInvoicesByStatus(invoiceStatus);
                } catch (IllegalArgumentException e) {
                    return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                        .body(ApiResponse.error("Trạng thái không hợp lệ: " + status));
                }
            } else if (orderId != null) {
                Optional<ElectronicInvoice> invoice = invoiceService.getInvoiceByOrderId(orderId);
                invoices = invoice.map(List::of).orElse(List.of());
            } else {
                // TODO: Implement more sophisticated filtering
                // For now, get all invoices (this should be paginated in production)
                invoices = invoiceService.getInvoicesByStatus(InvoiceStatus.CREATED);
                invoices.addAll(invoiceService.getInvoicesByStatus(InvoiceStatus.SENT));
                invoices.addAll(invoiceService.getInvoicesByStatus(InvoiceStatus.DELIVERED));
            }
            
            List<InvoiceResponseDTO> responseDTOs = invoices.stream()
                .map(InvoiceResponseDTO::new)
                .collect(Collectors.toList());
            
            return ResponseEntity.ok(ApiResponse.success(responseDTOs, "Lấy danh sách hóa đơn thành công"));
            
        } catch (Exception e) {
            log.error("Lỗi khi lấy danh sách hóa đơn: ", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ApiResponse.error("Lỗi hệ thống: " + e.getMessage()));
        }
    }

    /**
     * Lấy chi tiết hóa đơn theo ID
     * GET /api/invoices/{id}
     */
    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER', 'VIEWER')")
    public ResponseEntity<ApiResponse<InvoiceResponseDTO>> getInvoiceById(@PathVariable Long id) {
        try {
            log.info("Lấy chi tiết hóa đơn ID: {}", id);
            
            ElectronicInvoice invoice = invoiceService.getInvoiceById(id);
            InvoiceResponseDTO responseDTO = new InvoiceResponseDTO(invoice);
            
            return ResponseEntity.ok(ApiResponse.success(responseDTO, "Lấy chi tiết hóa đơn thành công"));
            
        } catch (Exception e) {
            log.error("Lỗi khi lấy chi tiết hóa đơn {}: ", id, e);
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                .body(ApiResponse.error("Không tìm thấy hóa đơn"));
        }
    }

    /**
     * Lấy hóa đơn theo Order ID
     * GET /api/invoices/by-order/{orderId}
     */
    @GetMapping("/by-order/{orderId}")
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER', 'VIEWER')")
    public ResponseEntity<ApiResponse<InvoiceResponseDTO>> getInvoiceByOrderId(@PathVariable Long orderId) {
        try {
            log.info("Lấy hóa đơn cho order ID: {}", orderId);
            
            Optional<ElectronicInvoice> invoiceOpt = invoiceService.getInvoiceByOrderId(orderId);
            
            if (invoiceOpt.isPresent()) {
                InvoiceResponseDTO responseDTO = new InvoiceResponseDTO(invoiceOpt.get());
                return ResponseEntity.ok(ApiResponse.success(responseDTO, "Lấy hóa đơn thành công"));
            } else {
                return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error("Đơn hàng chưa có hóa đơn điện tử"));
            }
            
        } catch (Exception e) {
            log.error("Lỗi khi lấy hóa đơn cho order {}: ", orderId, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ApiResponse.error("Lỗi hệ thống: " + e.getMessage()));
        }
    }

    /**
     * Gửi hóa đơn qua email
     * POST /api/invoices/{id}/send-email
     */
    @PostMapping("/{id}/send-email")
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER', 'OPERATIONS')")
    public ResponseEntity<ApiResponse<String>> sendInvoiceByEmail(
            @PathVariable Long id,
            @RequestParam String emailAddress) {
        try {
            log.info("Gửi hóa đơn {} qua email: {}", id, emailAddress);
            
            // Validate email format
            if (emailAddress == null || emailAddress.trim().isEmpty() || 
                !emailAddress.matches("^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$")) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error("Địa chỉ email không hợp lệ"));
            }
            
            invoiceService.sendInvoiceByEmail(id, emailAddress.trim().toLowerCase());
            
            return ResponseEntity.ok(ApiResponse.success(
                "Email đã được gửi đến: " + emailAddress,
                "Gửi hóa đơn qua email thành công"
            ));
            
        } catch (IllegalStateException e) {
            log.warn("Không thể gửi hóa đơn {}: {}", id, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                .body(ApiResponse.error(e.getMessage()));
                
        } catch (Exception e) {
            log.error("Lỗi khi gửi hóa đơn {} qua email: ", id, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ApiResponse.error("Lỗi hệ thống: " + e.getMessage()));
        }
    }

    /**
     * Hủy hóa đơn
     * POST /api/invoices/{id}/cancel
     */
    @PostMapping("/{id}/cancel")
    @PreAuthorize("hasAnyRole('ADMIN')")
    public ResponseEntity<ApiResponse<String>> cancelInvoice(
            @PathVariable Long id,
            @Valid @RequestBody CancelInvoiceRequestDTO request,
            Authentication authentication) {
        try {
            log.info("Hủy hóa đơn {} với lý do: {}", id, request.getCancellationReason());
            
            // Làm sạch dữ liệu
            request.sanitize();
            
            // Lấy thông tin user hiện tại
            User currentUser = userService.getCurrentUser(authentication);
            
            invoiceService.cancelInvoice(id, currentUser.getId(), request.getCancellationReason());
            
            return ResponseEntity.ok(ApiResponse.success(
                "Hóa đơn đã được hủy với lý do: " + request.getCancellationReason(),
                "Hủy hóa đơn thành công"
            ));
            
        } catch (IllegalStateException e) {
            log.warn("Không thể hủy hóa đơn {}: {}", id, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                .body(ApiResponse.error(e.getMessage()));
                
        } catch (Exception e) {
            log.error("Lỗi khi hủy hóa đơn {}: ", id, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ApiResponse.error("Lỗi hệ thống: " + e.getMessage()));
        }
    }

    /**
     * Tạo file PDF cho hóa đơn
     * POST /api/invoices/{id}/generate-pdf
     */
    @PostMapping("/{id}/generate-pdf")
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER', 'OPERATIONS')")
    public ResponseEntity<ApiResponse<String>> generatePdf(@PathVariable Long id) {
        try {
            log.info("Tạo file PDF cho hóa đơn ID: {}", id);
            
            String pdfPath = invoiceService.generateInvoicePdf(id);
            
            return ResponseEntity.ok(ApiResponse.success(pdfPath, "Tạo file PDF thành công"));
            
        } catch (Exception e) {
            log.error("Lỗi khi tạo PDF cho hóa đơn {}: ", id, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ApiResponse.error("Lỗi hệ thống: " + e.getMessage()));
        }
    }

    /**
     * Download file PDF của hóa đơn điện tử
     * GET /api/invoices/{id}/download-pdf
     */
    @GetMapping("/{id}/download-pdf")
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER', 'USER')")
    public ResponseEntity<Resource> downloadInvoicePdf(@PathVariable Long id) {
        try {
            log.info("Yêu cầu download PDF cho hóa đơn ID: {}", id);
            
            // Lấy thông tin hóa đơn
            ElectronicInvoice invoice = invoiceService.getInvoiceById(id);
            
            // Kiểm tra xem hóa đơn có file PDF không
            if (invoice.getPdfFilePath() == null || invoice.getPdfFileName() == null) {
                log.warn("Hóa đơn {} chưa có file PDF", invoice.getInvoiceNumber());
                return ResponseEntity.notFound().build();
            }
            
            // Tạo đường dẫn đến file PDF
            File pdfFile = new File(pdfStoragePath, invoice.getPdfFileName());
            
            // Kiểm tra file có tồn tại không
            if (!pdfFile.exists()) {
                log.warn("File PDF không tồn tại: {}", pdfFile.getAbsolutePath());
                return ResponseEntity.notFound().build();
            }
            
            // Tạo Resource để serve file
            Resource resource = new FileSystemResource(pdfFile);
            
            // Xác định content type - PDF thật sự
            String contentType = "application/pdf"; 
            String downloadFileName = "HoaDon_" + invoice.getInvoiceNumber() + ".pdf";
            
            log.info("📄 Download PDF invoice cho hóa đơn {}", invoice.getInvoiceNumber());
            
            // Ghi log thành công
            log.info("✅ Download thành công cho hóa đơn {} (file: {}, type: {})", 
                invoice.getInvoiceNumber(), invoice.getPdfFileName(), contentType);
            
            return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, 
                    "attachment; filename=\"" + downloadFileName + "\"")
                .header(HttpHeaders.CONTENT_TYPE, contentType)
                .header(HttpHeaders.CONTENT_LENGTH, String.valueOf(pdfFile.length()))
                .body(resource);
                
        } catch (EntityNotFoundException e) {
            log.warn("Không tìm thấy hóa đơn ID: {}", id);
            return ResponseEntity.notFound().build();
            
        } catch (Exception e) {
            log.error("Lỗi khi download PDF cho hóa đơn ID {}: ", id, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Lấy danh sách đơn hàng cần xuất hóa đơn (cho job tự động)
     * GET /api/invoices/orders-needing-invoice
     */
    @GetMapping("/orders-needing-invoice")
    @PreAuthorize("hasAnyRole('ADMIN', 'DISPATCHER', 'OPERATIONS')")
    public ResponseEntity<ApiResponse<List<Map<String, Object>>>> getOrdersNeedingInvoice() {
        try {
            log.info("Lấy danh sách đơn hàng cần xuất hóa đơn");
            
            List<Order> orders = invoiceService.getOrdersNeedingInvoice();
            
            List<Map<String, Object>> orderDTOs = orders.stream().map(order -> {
                Map<String, Object> orderDTO = new HashMap<>();
                orderDTO.put("id", order.getId());
                orderDTO.put("description", order.getDescription());
                orderDTO.put("totalAmount", order.getTotalAmount());
                orderDTO.put("createdAt", order.getCreatedAt());
                if (order.getStatus() != null) {
                    orderDTO.put("status", order.getStatus().getName());
                }
                return orderDTO;
            }).collect(Collectors.toList());
            
            return ResponseEntity.ok(ApiResponse.success(
                orderDTOs,
                String.format("Tìm thấy %d đơn hàng cần xuất hóa đơn", orders.size())
            ));
            
        } catch (Exception e) {
            log.error("Lỗi khi lấy danh sách đơn hàng cần xuất hóa đơn: ", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ApiResponse.error("Lỗi hệ thống: " + e.getMessage()));
        }
    }
}