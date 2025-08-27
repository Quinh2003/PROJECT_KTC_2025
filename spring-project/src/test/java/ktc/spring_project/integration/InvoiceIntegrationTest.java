package ktc.spring_project.integration;

import com.fasterxml.jackson.databind.ObjectMapper;
import ktc.spring_project.dtos.invoice.CreateInvoiceRequestDTO;
import ktc.spring_project.entities.*;
import ktc.spring_project.enums.InvoiceStatus;
import ktc.spring_project.repositories.*;
import ktc.spring_project.services.InvoiceService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureWebMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests cho toàn bộ quy trình xuất hóa đơn điện tử
 * Test từ việc hoàn thành đơn hàng đến xuất hóa đơn thành công
 */
@SpringBootTest
@AutoConfigureWebMvc
@ActiveProfiles("test")
@Transactional
class InvoiceIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private InvoiceService invoiceService;

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private DeliveryRepository deliveryRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ElectronicInvoiceRepository invoiceRepository;

    @Autowired
    private StatusRepository statusRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private ObjectMapper objectMapper;

    private Order testOrder;
    private User testUser;
    private Delivery testDelivery;
    private Status completedStatus;
    private Role adminRole;

    @BeforeEach
    void setUp() {
        // Tạo role admin
        adminRole = new Role();
        adminRole.setRoleName("ADMIN");
        adminRole.setDescription("Administrator role");
        adminRole.setIsActive(true);
        adminRole = roleRepository.save(adminRole);

        // Tạo status completed
        completedStatus = new Status();
        completedStatus.setType("ORDER");
        completedStatus.setName("COMPLETED");
        completedStatus.setDescription("Order completed");
        completedStatus = statusRepository.save(completedStatus);

        // Tạo user admin
        testUser = new User();
        testUser.setUsername("admin");
        testUser.setEmail("admin@example.com");
        testUser.setPassword("password");
        testUser.setFullName("Admin User");
        testUser.setRole(adminRole);
        testUser.setStatus(completedStatus);
        testUser = userRepository.save(testUser);

        // Tạo order
        testOrder = new Order();
        testOrder.setDescription("Integration Test Order");
        testOrder.setTotalAmount(new BigDecimal("1500.00"));
        testOrder.setBenefitPerOrder(new BigDecimal("150.00"));
        testOrder.setOrderProfitPerOrder(new BigDecimal("300.00"));
        testOrder.setStatus(completedStatus);
        testOrder.setCreatedBy(testUser);
        testOrder = orderRepository.save(testOrder);

        // Tạo delivery đã hoàn thành
        testDelivery = new Delivery();
        testDelivery.setOrder(testOrder);
        testDelivery.setDeliveryFee(new BigDecimal("50.00"));
        testDelivery.setOrderDate(new Timestamp(System.currentTimeMillis() - 7 * 86400000L)); // 7 ngày trước
        testDelivery.setActualDeliveryTime(new Timestamp(System.currentTimeMillis() - 86400000L)); // 1 ngày trước
        testDelivery.setDeliveryAttempts(1);
        testDelivery.setLateDeliveryRisk(0);
        testDelivery = deliveryRepository.save(testDelivery);
    }

    @Test
    @WithMockUser(username = "admin@example.com", roles = "ADMIN")
    void testCompleteInvoiceWorkflow_Success() throws Exception {
        // Step 1: Kiểm tra điều kiện xuất hóa đơn
        mockMvc.perform(get("/api/invoices/check-eligibility/{orderId}", testOrder.getId()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data.eligible").value(true));

        // Step 2: Tạo hóa đơn điện tử
        CreateInvoiceRequestDTO createRequest = new CreateInvoiceRequestDTO();
        createRequest.setOrderId(testOrder.getId());
        createRequest.setCustomerEmail("customer@example.com");
        createRequest.setCustomerName("Test Customer");
        createRequest.setNotes("Integration test invoice");

        mockMvc.perform(post("/api/invoices")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(createRequest))
                        .with(csrf()))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Tạo hóa đơn điện tử thành công"))
                .andExpect(jsonPath("$.data.orderId").value(testOrder.getId()))
                .andExpect(jsonPath("$.data.invoiceStatus").value("CREATED"))
                .andExpect(jsonPath("$.data.customerEmail").value("customer@example.com"))
                .andExpect(jsonPath("$.data.customerName").value("Test Customer"));

        // Step 3: Kiểm tra hóa đơn đã được tạo trong database
        var invoiceOpt = invoiceRepository.findByOrderId(testOrder.getId());
        assertTrue(invoiceOpt.isPresent());
        
        ElectronicInvoice invoice = invoiceOpt.get();
        assertEquals(testOrder.getId(), invoice.getOrder().getId());
        assertEquals(InvoiceStatus.CREATED, invoice.getInvoiceStatus());
        assertEquals("customer@example.com", invoice.getCustomerEmail());
        assertEquals("Test Customer", invoice.getCustomerName());
        assertEquals(testUser.getId(), invoice.getCreatedBy().getId());
        assertNotNull(invoice.getInvoiceNumber());
        assertTrue(invoice.getInvoiceNumber().startsWith("INV-"));

        // Step 4: Lấy chi tiết hóa đơn qua API
        mockMvc.perform(get("/api/invoices/{id}", invoice.getId()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data.id").value(invoice.getId()))
                .andExpect(jsonPath("$.data.invoiceNumber").value(invoice.getInvoiceNumber()));

        // Step 5: Lấy hóa đơn theo order ID
        mockMvc.perform(get("/api/invoices/by-order/{orderId}", testOrder.getId()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data.orderId").value(testOrder.getId()));

        // Step 6: Gửi hóa đơn qua email
        mockMvc.perform(post("/api/invoices/{id}/send-email", invoice.getId())
                        .param("emailAddress", "customer@example.com")
                        .with(csrf()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Gửi hóa đơn qua email thành công"));

        // Step 7: Kiểm tra trạng thái hóa đơn đã được cập nhật
        invoice = invoiceRepository.findById(invoice.getId()).orElseThrow();
        assertEquals(InvoiceStatus.SENT, invoice.getInvoiceStatus());
        assertNotNull(invoice.getEmailSentAt());
        assertEquals("customer@example.com", invoice.getCustomerEmail());

        // Step 8: Tạo PDF cho hóa đơn
        mockMvc.perform(post("/api/invoices/{id}/generate-pdf", invoice.getId())
                        .with(csrf()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Tạo file PDF thành công"));

        // Step 9: Kiểm tra PDF đã được tạo
        invoice = invoiceRepository.findById(invoice.getId()).orElseThrow();
        assertNotNull(invoice.getPdfFilePath());
        assertNotNull(invoice.getPdfFileName());
        assertTrue(invoice.getPdfFilePath().startsWith("invoices/pdfs/"));
    }

    @Test
    @WithMockUser(username = "admin@example.com", roles = "ADMIN")
    void testInvoiceWorkflow_OrderAlreadyHasInvoice() throws Exception {
        // Tạo hóa đơn trước
        invoiceService.createInvoice(
            testOrder.getId(),
            testUser.getId(),
            "first@example.com",
            "First Customer"
        );

        // Kiểm tra điều kiện - không thể tạo hóa đơn thứ 2
        mockMvc.perform(get("/api/invoices/check-eligibility/{orderId}", testOrder.getId()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data.eligible").value(false))
                .andExpect(jsonPath("$.data.message").value("Đơn hàng đã có hóa đơn điện tử"));

        // Thử tạo hóa đơn thứ 2 sẽ thất bại
        CreateInvoiceRequestDTO createRequest = new CreateInvoiceRequestDTO();
        createRequest.setOrderId(testOrder.getId());

        mockMvc.perform(post("/api/invoices")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(createRequest))
                        .with(csrf()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false));
    }

    @Test
    @WithMockUser(username = "admin@example.com", roles = "ADMIN")
    void testInvoiceWorkflow_DeliveryNotCompleted() throws Exception {
        // Cập nhật delivery chưa hoàn thành
        testDelivery.setActualDeliveryTime(null);
        deliveryRepository.save(testDelivery);

        // Kiểm tra điều kiện - không thể tạo hóa đơn
        mockMvc.perform(get("/api/invoices/check-eligibility/{orderId}", testOrder.getId()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data.eligible").value(false))
                .andExpect(jsonPath("$.data.message").value("Đơn hàng chưa hoàn thành giao hàng"));

        // Thử tạo hóa đơn sẽ thất bại
        CreateInvoiceRequestDTO createRequest = new CreateInvoiceRequestDTO();
        createRequest.setOrderId(testOrder.getId());

        mockMvc.perform(post("/api/invoices")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(createRequest))
                        .with(csrf()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false));
    }

    @Test
    @WithMockUser(username = "admin@example.com", roles = "ADMIN")
    void testInvoiceWorkflow_ExpiredDelivery() throws Exception {
        // Cập nhật delivery đã hoàn thành quá 5 ngày
        testDelivery.setActualDeliveryTime(new Timestamp(System.currentTimeMillis() - 6 * 86400000L)); // 6 ngày trước
        deliveryRepository.save(testDelivery);

        // Kiểm tra điều kiện - không thể tạo hóa đơn
        mockMvc.perform(get("/api/invoices/check-eligibility/{orderId}", testOrder.getId()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data.eligible").value(false))
                .andExpect(jsonPath("$.data.message").contains("Đã quá hạn xuất hóa đơn"));
    }

    @Test
    @WithMockUser(username = "admin@example.com", roles = "ADMIN")
    void testCancelInvoiceWorkflow() throws Exception {
        // Tạo hóa đơn
        ElectronicInvoice invoice = invoiceService.createInvoice(
            testOrder.getId(),
            testUser.getId(),
            "customer@example.com",
            "Customer Name"
        );

        // Hủy hóa đơn
        var cancelRequest = new ktc.spring_project.dtos.invoice.CancelInvoiceRequestDTO();
        cancelRequest.setCancellationReason("Customer requested cancellation");

        mockMvc.perform(post("/api/invoices/{id}/cancel", invoice.getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(cancelRequest))
                        .with(csrf()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.message").value("Hủy hóa đơn thành công"));

        // Kiểm tra hóa đơn đã bị hủy
        invoice = invoiceRepository.findById(invoice.getId()).orElseThrow();
        assertEquals(InvoiceStatus.CANCELLED, invoice.getInvoiceStatus());
        assertNotNull(invoice.getCancelledAt());
        assertEquals(testUser.getId(), invoice.getCancelledBy().getId());
        assertEquals("Customer requested cancellation", invoice.getCancellationReason());

        // Thử gửi email hóa đơn đã hủy sẽ thất bại
        mockMvc.perform(post("/api/invoices/{id}/send-email", invoice.getId())
                        .param("emailAddress", "customer@example.com")
                        .with(csrf()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.success").value(false));
    }

    @Test
    @WithMockUser(username = "admin@example.com", roles = "ADMIN")
    void testGetOrdersNeedingInvoice() throws Exception {
        // Tạo thêm một order khác cần xuất hóa đơn
        Order anotherOrder = new Order();
        anotherOrder.setDescription("Another Order");
        anotherOrder.setTotalAmount(new BigDecimal("2000.00"));
        anotherOrder.setStatus(completedStatus);
        anotherOrder.setCreatedBy(testUser);
        anotherOrder = orderRepository.save(anotherOrder);

        Delivery anotherDelivery = new Delivery();
        anotherDelivery.setOrder(anotherOrder);
        anotherDelivery.setOrderDate(new Timestamp(System.currentTimeMillis() - 5 * 86400000L));
        anotherDelivery.setActualDeliveryTime(new Timestamp(System.currentTimeMillis() - 4 * 86400000L)); // 4 ngày trước
        anotherDelivery.setDeliveryAttempts(1);
        anotherDelivery.setLateDeliveryRisk(0);
        deliveryRepository.save(anotherDelivery);

        // Lấy danh sách orders cần xuất hóa đơn
        mockMvc.perform(get("/api/invoices/orders-needing-invoice"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(2)); // Có 2 orders cần xuất hóa đơn

        // Tạo hóa đơn cho 1 order
        invoiceService.createInvoice(testOrder.getId(), testUser.getId(), null, null);

        // Kiểm tra lại danh sách - chỉ còn 1 order
        mockMvc.perform(get("/api/invoices/orders-needing-invoice"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data.length()").value(1));
    }

    @Test
    @WithMockUser(username = "admin@example.com", roles = "ADMIN")
    void testInvoiceListingAndFiltering() throws Exception {
        // Tạo một số hóa đơn với trạng thái khác nhau
        ElectronicInvoice invoice1 = invoiceService.createInvoice(
            testOrder.getId(), testUser.getId(), "customer1@example.com", "Customer 1");
        
        // Tạo order và delivery khác
        Order order2 = new Order();
        order2.setDescription("Order 2");
        order2.setTotalAmount(new BigDecimal("1000.00"));
        order2.setStatus(completedStatus);
        order2.setCreatedBy(testUser);
        order2 = orderRepository.save(order2);

        Delivery delivery2 = new Delivery();
        delivery2.setOrder(order2);
        delivery2.setOrderDate(new Timestamp(System.currentTimeMillis() - 86400000L));
        delivery2.setActualDeliveryTime(new Timestamp(System.currentTimeMillis() - 43200000L)); // 12 giờ trước
        delivery2.setDeliveryAttempts(1);
        delivery2.setLateDeliveryRisk(0);
        deliveryRepository.save(delivery2);

        ElectronicInvoice invoice2 = invoiceService.createInvoice(
            order2.getId(), testUser.getId(), "customer2@example.com", "Customer 2");

        // Gửi email cho invoice2
        invoiceService.sendInvoiceByEmail(invoice2.getId(), "customer2@example.com");

        // Test lấy tất cả hóa đơn
        mockMvc.perform(get("/api/invoices"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data").isArray());

        // Test filter theo trạng thái CREATED
        mockMvc.perform(get("/api/invoices").param("status", "CREATED"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data").isArray());

        // Test filter theo trạng thái SENT
        mockMvc.perform(get("/api/invoices").param("status", "SENT"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data").isArray());

        // Test filter theo order ID
        mockMvc.perform(get("/api/invoices").param("orderId", testOrder.getId().toString()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true))
                .andExpect(jsonPath("$.data").isArray());
    }
}
