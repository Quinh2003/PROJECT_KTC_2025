package ktc.spring_project.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import ktc.spring_project.dtos.invoice.CreateInvoiceRequestDTO;
import ktc.spring_project.dtos.invoice.CancelInvoiceRequestDTO;
import ktc.spring_project.entities.*;
import ktc.spring_project.enums.InvoiceStatus;
import ktc.spring_project.services.InvoiceService;
import ktc.spring_project.services.UserService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Unit tests cho InvoiceController
 * Kiểm tra tất cả các API endpoints và xử lý lỗi
 */
@WebMvcTest(InvoiceController.class)
class InvoiceControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private InvoiceService invoiceService;

    @MockBean
    private UserService userService;

    @Autowired
    private ObjectMapper objectMapper;

    private ElectronicInvoice testInvoice;
    private User testUser;
    private Order testOrder;

    @BeforeEach
    void setUp() {
        testUser = new User();
        testUser.setId(1L);
        testUser.setEmail("test@example.com");
        testUser.setFullName("Test User");

        testOrder = new Order();
        testOrder.setId(1L);
        testOrder.setTotalAmount(new BigDecimal("1000.00"));
        testOrder.setDescription("Test Order");

        testInvoice = new ElectronicInvoice();
        testInvoice.setId(1L);
        testInvoice.setInvoiceNumber("INV-TEST-001");
        testInvoice.setOrder(testOrder);
        testInvoice.setCreatedBy(testUser);
        testInvoice.setInvoiceStatus(InvoiceStatus.CREATED);
        testInvoice.setTotalAmount(new BigDecimal("1000.00"));
        testInvoice.setIssuedAt(new Timestamp(System.currentTimeMillis()));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    void testCheckInvoiceEligibility_ValidOrder_ReturnsSuccess() throws Exception {
        // Given
        Long orderId = 1L;
        InvoiceService.ValidationResult validationResult = 
            new InvoiceService.ValidationResult(true, "Đơn hàng đạt điều kiện xuất hóa đơn");
        
        when(invoiceService.validateInvoiceEligibility(orderId)).thenReturn(validationResult);

        // When & Then
        mockMvc.perform(get("/api/invoices/check-eligibility/{orderId}", orderId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Kiểm tra điều kiện thành công"))
                .andExpect(jsonPath("$.data.orderId").value(orderId))
                .andExpect(jsonPath("$.data.eligible").value(true));

        verify(invoiceService).validateInvoiceEligibility(orderId);
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    void testCheckInvoiceEligibility_InvalidOrder_ReturnsError() throws Exception {
        // Given
        Long orderId = 1L;
        InvoiceService.ValidationResult validationResult = 
            new InvoiceService.ValidationResult(false, "Đơn hàng không tồn tại");
        
        when(invoiceService.validateInvoiceEligibility(orderId)).thenReturn(validationResult);

        // When & Then
        mockMvc.perform(get("/api/invoices/check-eligibility/{orderId}", orderId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data.eligible").value(false))
                .andExpect(jsonPath("$.data.message").value("Đơn hàng không tồn tại"));

        verify(invoiceService).validateInvoiceEligibility(orderId);
    }

    @Test
    @WithMockUser(roles = "VIEWER")
    void testCheckInvoiceEligibility_NoPermission_ReturnsForbidden() throws Exception {
        // When & Then
        mockMvc.perform(get("/api/invoices/check-eligibility/1"))
                .andExpect(status().isForbidden());

        verify(invoiceService, never()).validateInvoiceEligibility(any());
    }

    @Test
    @WithMockUser(roles = "ADMIN", username = "test@example.com")
    void testCreateInvoice_ValidRequest_ReturnsCreated() throws Exception {
        // Given
        CreateInvoiceRequestDTO request = new CreateInvoiceRequestDTO();
        request.setOrderId(1L);
        request.setCustomerEmail("customer@example.com");
        request.setCustomerName("Customer Name");

        when(userService.findByEmail("test@example.com")).thenReturn(testUser);
        when(invoiceService.createInvoice(anyLong(), anyLong(), anyString(), anyString()))
                .thenReturn(testInvoice);

        // When & Then
        mockMvc.perform(post("/api/invoices")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request))
                        .with(csrf()))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Tạo hóa đơn điện tử thành công"))
                .andExpect(jsonPath("$.data.id").value(1L))
                .andExpect(jsonPath("$.data.invoiceNumber").value("INV-TEST-001"));

        verify(invoiceService).createInvoice(1L, 1L, "customer@example.com", "Customer Name");
        verify(invoiceService).generateInvoicePdf(1L);
    }

    @Test
    @WithMockUser(roles = "ADMIN", username = "test@example.com")
    void testCreateInvoice_InvalidData_ReturnsBadRequest() throws Exception {
        // Given
        CreateInvoiceRequestDTO request = new CreateInvoiceRequestDTO();
        request.setOrderId(1L);

        when(userService.findByEmail("test@example.com")).thenReturn(testUser);
        when(invoiceService.createInvoice(anyLong(), anyLong(), any(), any()))
                .thenThrow(new IllegalArgumentException("Không thể tạo hóa đơn"));

        // When & Then
        mockMvc.perform(post("/api/invoices")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request))
                        .with(csrf()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.message").value("Không thể tạo hóa đơn"));

        verify(invoiceService).createInvoice(anyLong(), anyLong(), any(), any());
    }

    @Test
    @WithMockUser(roles = "VIEWER")
    void testGetAllInvoices_ValidRequest_ReturnsInvoices() throws Exception {
        // Given
        List<ElectronicInvoice> invoices = Arrays.asList(testInvoice);
        when(invoiceService.getInvoicesByStatus(InvoiceStatus.CREATED)).thenReturn(invoices);
        when(invoiceService.getInvoicesByStatus(InvoiceStatus.SENT)).thenReturn(Arrays.asList());
        when(invoiceService.getInvoicesByStatus(InvoiceStatus.DELIVERED)).thenReturn(Arrays.asList());

        // When & Then
        mockMvc.perform(get("/api/invoices"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Lấy danh sách hóa đơn thành công"))
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data[0].id").value(1L));

        verify(invoiceService).getInvoicesByStatus(InvoiceStatus.CREATED);
    }

    @Test
    @WithMockUser(roles = "VIEWER")
    void testGetAllInvoices_WithStatusFilter_ReturnsFilteredInvoices() throws Exception {
        // Given
        List<ElectronicInvoice> invoices = Arrays.asList(testInvoice);
        when(invoiceService.getInvoicesByStatus(InvoiceStatus.CREATED)).thenReturn(invoices);

        // When & Then
        mockMvc.perform(get("/api/invoices").param("status", "CREATED"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data[0].invoiceStatus").value("CREATED"));

        verify(invoiceService).getInvoicesByStatus(InvoiceStatus.CREATED);
    }

    @Test
    @WithMockUser(roles = "VIEWER")
    void testGetInvoiceById_ExistingInvoice_ReturnsInvoice() throws Exception {
        // Given
        Long invoiceId = 1L;
        when(invoiceService.getInvoiceById(invoiceId)).thenReturn(testInvoice);

        // When & Then
        mockMvc.perform(get("/api/invoices/{id}", invoiceId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Lấy chi tiết hóa đơn thành công"))
                .andExpect(jsonPath("$.data.id").value(invoiceId))
                .andExpect(jsonPath("$.data.invoiceNumber").value("INV-TEST-001"));

        verify(invoiceService).getInvoiceById(invoiceId);
    }

    @Test
    @WithMockUser(roles = "VIEWER")
    void testGetInvoiceById_NonExistingInvoice_ReturnsNotFound() throws Exception {
        // Given
        Long invoiceId = 999L;
        when(invoiceService.getInvoiceById(invoiceId))
                .thenThrow(new jakarta.persistence.EntityNotFoundException("Hóa đơn không tồn tại"));

        // When & Then
        mockMvc.perform(get("/api/invoices/{id}", invoiceId))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.message").value("Không tìm thấy hóa đơn"));

        verify(invoiceService).getInvoiceById(invoiceId);
    }

    @Test
    @WithMockUser(roles = "VIEWER")
    void testGetInvoiceByOrderId_ExistingInvoice_ReturnsInvoice() throws Exception {
        // Given
        Long orderId = 1L;
        when(invoiceService.getInvoiceByOrderId(orderId)).thenReturn(Optional.of(testInvoice));

        // When & Then
        mockMvc.perform(get("/api/invoices/by-order/{orderId}", orderId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Lấy hóa đơn thành công"))
                .andExpect(jsonPath("$.data.orderId").value(orderId));

        verify(invoiceService).getInvoiceByOrderId(orderId);
    }

    @Test
    @WithMockUser(roles = "VIEWER")
    void testGetInvoiceByOrderId_NoInvoice_ReturnsNotFound() throws Exception {
        // Given
        Long orderId = 1L;
        when(invoiceService.getInvoiceByOrderId(orderId)).thenReturn(Optional.empty());

        // When & Then
        mockMvc.perform(get("/api/invoices/by-order/{orderId}", orderId))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.message").value("Đơn hàng chưa có hóa đơn điện tử"));

        verify(invoiceService).getInvoiceByOrderId(orderId);
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    void testSendInvoiceByEmail_ValidRequest_ReturnsSuccess() throws Exception {
        // Given
        Long invoiceId = 1L;
        String emailAddress = "customer@example.com";

        doNothing().when(invoiceService).sendInvoiceByEmail(invoiceId, emailAddress);

        // When & Then
        mockMvc.perform(post("/api/invoices/{id}/send-email", invoiceId)
                        .param("emailAddress", emailAddress)
                        .with(csrf()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Gửi hóa đơn qua email thành công"));

        verify(invoiceService).sendInvoiceByEmail(invoiceId, emailAddress);
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    void testSendInvoiceByEmail_InvalidEmail_ReturnsBadRequest() throws Exception {
        // Given
        Long invoiceId = 1L;
        String invalidEmail = "invalid-email";

        // When & Then
        mockMvc.perform(post("/api/invoices/{id}/send-email", invoiceId)
                        .param("emailAddress", invalidEmail)
                        .with(csrf()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.message").value("Địa chỉ email không hợp lệ"));

        verify(invoiceService, never()).sendInvoiceByEmail(any(), any());
    }

    @Test
    @WithMockUser(roles = "ADMIN", username = "admin@example.com")
    void testCancelInvoice_ValidRequest_ReturnsSuccess() throws Exception {
        // Given
        Long invoiceId = 1L;
        CancelInvoiceRequestDTO request = new CancelInvoiceRequestDTO();
        request.setCancellationReason("Khách hàng yêu cầu hủy");

        User adminUser = new User();
        adminUser.setId(2L);
        adminUser.setEmail("admin@example.com");

        when(userService.findByEmail("admin@example.com")).thenReturn(adminUser);
        doNothing().when(invoiceService).cancelInvoice(invoiceId, 2L, "Khách hàng yêu cầu hủy");

        // When & Then
        mockMvc.perform(post("/api/invoices/{id}/cancel", invoiceId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request))
                        .with(csrf()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Hủy hóa đơn thành công"));

        verify(invoiceService).cancelInvoice(invoiceId, 2L, "Khách hàng yêu cầu hủy");
    }

    @Test
    @WithMockUser(roles = "DISPATCHER")
    void testCancelInvoice_NoPermission_ReturnsForbidden() throws Exception {
        // Given
        Long invoiceId = 1L;
        CancelInvoiceRequestDTO request = new CancelInvoiceRequestDTO();
        request.setCancellationReason("Test reason");

        // When & Then
        mockMvc.perform(post("/api/invoices/{id}/cancel", invoiceId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request))
                        .with(csrf()))
                .andExpect(status().isForbidden());

        verify(invoiceService, never()).cancelInvoice(any(), any(), any());
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    void testGeneratePdf_ValidInvoice_ReturnsSuccess() throws Exception {
        // Given
        Long invoiceId = 1L;
        String pdfPath = "invoices/pdfs/invoice_INV-TEST-001.pdf";

        when(invoiceService.generateInvoicePdf(invoiceId)).thenReturn(pdfPath);

        // When & Then
        mockMvc.perform(post("/api/invoices/{id}/generate-pdf", invoiceId)
                        .with(csrf()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Tạo file PDF thành công"))
                .andExpect(jsonPath("$.data").value(pdfPath));

        verify(invoiceService).generateInvoicePdf(invoiceId);
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    void testGetOrdersNeedingInvoice_ReturnsOrders() throws Exception {
        // Given
        List<Order> orders = Arrays.asList(testOrder);
        when(invoiceService.getOrdersNeedingInvoice()).thenReturn(orders);

        // When & Then
        mockMvc.perform(get("/api/invoices/orders-needing-invoice"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Tìm thấy 1 đơn hàng cần xuất hóa đơn"))
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data[0].id").value(1L));

        verify(invoiceService).getOrdersNeedingInvoice();
    }
}
