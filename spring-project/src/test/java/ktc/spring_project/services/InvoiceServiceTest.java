package ktc.spring_project.services;

import ktc.spring_project.entities.*;
import ktc.spring_project.enums.InvoiceStatus;
import ktc.spring_project.repositories.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests cho InvoiceService
 * Kiểm tra tất cả các chức năng nghiệp vụ quan trọng
 */
@ExtendWith(MockitoExtension.class)
class InvoiceServiceTest {

    @Mock
    private ElectronicInvoiceRepository invoiceRepository;

    @Mock
    private OrderRepository orderRepository;

    @Mock
    private DeliveryRepository deliveryRepository;

    @Mock
    private UserRepository userRepository;

    @Mock
    private ActivityLogService activityLogService;

    @InjectMocks
    private InvoiceService invoiceService;

    private Order testOrder;
    private User testUser;
    private Delivery testDelivery;
    private Status testStatus;

    @BeforeEach
    void setUp() {
        // Thiết lập dữ liệu test
        testStatus = new Status();
        testStatus.setId(1L);
        testStatus.setName("COMPLETED");

        testOrder = new Order();
        testOrder.setId(1L);
        testOrder.setTotalAmount(new BigDecimal("1000.00"));
        testOrder.setDescription("Test Order");
        testOrder.setStatus(testStatus);

        testUser = new User();
        testUser.setId(1L);
        testUser.setEmail("test@example.com");
        testUser.setFullName("Test User");

        testDelivery = new Delivery();
        testDelivery.setId(1L);
        testDelivery.setOrder(testOrder);
        testDelivery.setActualDeliveryTime(new Timestamp(System.currentTimeMillis() - 86400000)); // 1 ngày trước
    }

    @Test
    void testValidateInvoiceEligibility_ValidOrder_ReturnsTrue() {
        // Given
        Long orderId = 1L;
        when(orderRepository.findById(orderId)).thenReturn(Optional.of(testOrder));
        when(invoiceRepository.existsByOrderId(orderId)).thenReturn(false);
        when(deliveryRepository.findByOrderId(orderId)).thenReturn(Arrays.asList(testDelivery));

        // When
        InvoiceService.ValidationResult result = invoiceService.validateInvoiceEligibility(orderId);

        // Then
        assertTrue(result.isValid());
        assertEquals("Đơn hàng đạt điều kiện xuất hóa đơn", result.getMessage());
    }

    @Test
    void testValidateInvoiceEligibility_OrderNotFound_ReturnsFalse() {
        // Given
        Long orderId = 999L;
        when(orderRepository.findById(orderId)).thenReturn(Optional.empty());

        // When
        InvoiceService.ValidationResult result = invoiceService.validateInvoiceEligibility(orderId);

        // Then
        assertFalse(result.isValid());
        assertEquals("Đơn hàng không tồn tại", result.getMessage());
    }

    @Test
    void testValidateInvoiceEligibility_AlreadyHasInvoice_ReturnsFalse() {
        // Given
        Long orderId = 1L;
        when(orderRepository.findById(orderId)).thenReturn(Optional.of(testOrder));
        when(invoiceRepository.existsByOrderId(orderId)).thenReturn(true);

        // When
        InvoiceService.ValidationResult result = invoiceService.validateInvoiceEligibility(orderId);

        // Then
        assertFalse(result.isValid());
        assertEquals("Đơn hàng đã có hóa đơn điện tử", result.getMessage());
    }

    @Test
    void testValidateInvoiceEligibility_DeliveryNotCompleted_ReturnsFalse() {
        // Given
        Long orderId = 1L;
        testDelivery.setActualDeliveryTime(null); // Chưa hoàn thành giao hàng
        
        when(orderRepository.findById(orderId)).thenReturn(Optional.of(testOrder));
        when(invoiceRepository.existsByOrderId(orderId)).thenReturn(false);
        when(deliveryRepository.findByOrderId(orderId)).thenReturn(Arrays.asList(testDelivery));

        // When
        InvoiceService.ValidationResult result = invoiceService.validateInvoiceEligibility(orderId);

        // Then
        assertFalse(result.isValid());
        assertEquals("Đơn hàng chưa hoàn thành giao hàng", result.getMessage());
    }

    @Test
    void testValidateInvoiceEligibility_DeliveryExpired_ReturnsFalse() {
        // Given
        Long orderId = 1L;
        // Set delivery time 6 ngày trước (quá hạn 5 ngày)
        testDelivery.setActualDeliveryTime(new Timestamp(System.currentTimeMillis() - 6 * 86400000L));
        
        when(orderRepository.findById(orderId)).thenReturn(Optional.of(testOrder));
        when(invoiceRepository.existsByOrderId(orderId)).thenReturn(false);
        when(deliveryRepository.findByOrderId(orderId)).thenReturn(Arrays.asList(testDelivery));

        // When
        InvoiceService.ValidationResult result = invoiceService.validateInvoiceEligibility(orderId);

        // Then
        assertFalse(result.isValid());
        assertTrue(result.getMessage().contains("Đã quá hạn xuất hóa đơn"));
    }

    @Test
    void testCreateInvoice_ValidInput_Success() {
        // Given
        Long orderId = 1L;
        Long userId = 1L;
        String customerEmail = "customer@example.com";
        String customerName = "Customer Name";

        when(orderRepository.findById(orderId)).thenReturn(Optional.of(testOrder));
        when(invoiceRepository.existsByOrderId(orderId)).thenReturn(false);
        when(deliveryRepository.findByOrderId(orderId)).thenReturn(Arrays.asList(testDelivery));
        when(userRepository.findById(userId)).thenReturn(Optional.of(testUser));
        when(invoiceRepository.save(any(ElectronicInvoice.class))).thenAnswer(invocation -> {
            ElectronicInvoice invoice = invocation.getArgument(0);
            invoice.setId(1L);
            return invoice;
        });

        // When
        ElectronicInvoice result = invoiceService.createInvoice(orderId, userId, customerEmail, customerName);

        // Then
        assertNotNull(result);
        assertEquals(testOrder, result.getOrder());
        assertEquals(testUser, result.getCreatedBy());
        assertEquals(customerEmail, result.getCustomerEmail());
        assertEquals(customerName, result.getCustomerName());
        assertEquals(InvoiceStatus.CREATED, result.getInvoiceStatus());
        assertNotNull(result.getInvoiceNumber());
        assertTrue(result.getInvoiceNumber().startsWith("INV-"));
        
        verify(invoiceRepository).save(any(ElectronicInvoice.class));
        verify(activityLogService).logUserActivity(eq(userId), eq("CREATE"), anyString());
    }

    @Test
    void testCreateInvoice_InvalidOrder_ThrowsException() {
        // Given
        Long orderId = 1L;
        Long userId = 1L;
        
        when(orderRepository.findById(orderId)).thenReturn(Optional.of(testOrder));
        when(invoiceRepository.existsByOrderId(orderId)).thenReturn(true); // Đã có hóa đơn

        // When & Then
        IllegalArgumentException exception = assertThrows(
            IllegalArgumentException.class,
            () -> invoiceService.createInvoice(orderId, userId, null, null)
        );
        
        assertTrue(exception.getMessage().contains("Không thể tạo hóa đơn"));
        verify(invoiceRepository, never()).save(any());
    }

    @Test
    void testSendInvoiceByEmail_ValidInvoice_Success() {
        // Given
        Long invoiceId = 1L;
        String emailAddress = "customer@example.com";
        
        ElectronicInvoice testInvoice = new ElectronicInvoice();
        testInvoice.setId(invoiceId);
        testInvoice.setInvoiceNumber("INV-TEST-001");
        testInvoice.setInvoiceStatus(InvoiceStatus.CREATED);
        testInvoice.setCreatedBy(testUser);

        when(invoiceRepository.findById(invoiceId)).thenReturn(Optional.of(testInvoice));
        when(invoiceRepository.save(any(ElectronicInvoice.class))).thenReturn(testInvoice);

        // When
        invoiceService.sendInvoiceByEmail(invoiceId, emailAddress);

        // Then
        verify(invoiceRepository).save(testInvoice);
        assertEquals(InvoiceStatus.SENT, testInvoice.getInvoiceStatus());
        assertEquals(emailAddress, testInvoice.getCustomerEmail());
        assertNotNull(testInvoice.getEmailSentAt());
        verify(activityLogService).logUserActivity(eq(testUser.getId()), eq("UPDATE"), anyString());
    }

    @Test
    void testSendInvoiceByEmail_CancelledInvoice_ThrowsException() {
        // Given
        Long invoiceId = 1L;
        String emailAddress = "customer@example.com";
        
        ElectronicInvoice testInvoice = new ElectronicInvoice();
        testInvoice.setId(invoiceId);
        testInvoice.setInvoiceStatus(InvoiceStatus.CANCELLED);

        when(invoiceRepository.findById(invoiceId)).thenReturn(Optional.of(testInvoice));

        // When & Then
        IllegalStateException exception = assertThrows(
            IllegalStateException.class,
            () -> invoiceService.sendInvoiceByEmail(invoiceId, emailAddress)
        );
        
        assertEquals("Không thể gửi hóa đơn đã bị hủy", exception.getMessage());
        verify(invoiceRepository, never()).save(any());
    }

    @Test
    void testCancelInvoice_ValidInvoice_Success() {
        // Given
        Long invoiceId = 1L;
        Long cancelledByUserId = 2L;
        String reason = "Khách hàng yêu cầu hủy";
        
        User cancelledByUser = new User();
        cancelledByUser.setId(cancelledByUserId);
        cancelledByUser.setEmail("admin@example.com");
        
        ElectronicInvoice testInvoice = new ElectronicInvoice();
        testInvoice.setId(invoiceId);
        testInvoice.setInvoiceNumber("INV-TEST-001");
        testInvoice.setInvoiceStatus(InvoiceStatus.CREATED);

        when(invoiceRepository.findById(invoiceId)).thenReturn(Optional.of(testInvoice));
        when(userRepository.findById(cancelledByUserId)).thenReturn(Optional.of(cancelledByUser));
        when(invoiceRepository.save(any(ElectronicInvoice.class))).thenReturn(testInvoice);

        // When
        invoiceService.cancelInvoice(invoiceId, cancelledByUserId, reason);

        // Then
        assertEquals(InvoiceStatus.CANCELLED, testInvoice.getInvoiceStatus());
        assertEquals(cancelledByUser, testInvoice.getCancelledBy());
        assertEquals(reason, testInvoice.getCancellationReason());
        assertNotNull(testInvoice.getCancelledAt());
        
        verify(invoiceRepository).save(testInvoice);
        verify(activityLogService).logUserActivity(eq(cancelledByUserId), eq("DELETE"), anyString());
    }

    @Test
    void testGetInvoiceById_ExistingInvoice_ReturnsInvoice() {
        // Given
        Long invoiceId = 1L;
        ElectronicInvoice testInvoice = new ElectronicInvoice();
        testInvoice.setId(invoiceId);
        
        when(invoiceRepository.findById(invoiceId)).thenReturn(Optional.of(testInvoice));

        // When
        ElectronicInvoice result = invoiceService.getInvoiceById(invoiceId);

        // Then
        assertNotNull(result);
        assertEquals(invoiceId, result.getId());
    }

    @Test
    void testGetInvoiceById_NonExistingInvoice_ThrowsException() {
        // Given
        Long invoiceId = 999L;
        when(invoiceRepository.findById(invoiceId)).thenReturn(Optional.empty());

        // When & Then
        jakarta.persistence.EntityNotFoundException exception = assertThrows(
            jakarta.persistence.EntityNotFoundException.class,
            () -> invoiceService.getInvoiceById(invoiceId)
        );
        
        assertTrue(exception.getMessage().contains("Hóa đơn không tồn tại"));
    }

    @Test
    void testGetInvoicesByStatus_ValidStatus_ReturnsInvoices() {
        // Given
        InvoiceStatus status = InvoiceStatus.CREATED;
        List<ElectronicInvoice> mockInvoices = Arrays.asList(
            new ElectronicInvoice(),
            new ElectronicInvoice()
        );
        
        when(invoiceRepository.findByStatusOrderByIssuedAtDesc(status)).thenReturn(mockInvoices);

        // When
        List<ElectronicInvoice> result = invoiceService.getInvoicesByStatus(status);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        verify(invoiceRepository).findByStatusOrderByIssuedAtDesc(status);
    }

    @Test
    void testGenerateInvoicePdf_ValidInvoice_ReturnsPath() {
        // Given
        Long invoiceId = 1L;
        ElectronicInvoice testInvoice = new ElectronicInvoice();
        testInvoice.setId(invoiceId);
        testInvoice.setInvoiceNumber("INV-TEST-001");
        
        when(invoiceRepository.findById(invoiceId)).thenReturn(Optional.of(testInvoice));
        when(invoiceRepository.save(any(ElectronicInvoice.class))).thenReturn(testInvoice);

        // When
        String result = invoiceService.generateInvoicePdf(invoiceId);

        // Then
        assertNotNull(result);
        assertTrue(result.startsWith("invoices/pdfs/"));
        assertTrue(result.contains("INV-TEST-001"));
        assertNotNull(testInvoice.getPdfFileName());
        assertNotNull(testInvoice.getPdfFilePath());
        verify(invoiceRepository).save(testInvoice);
    }

    @Test
    void testGetOrdersNeedingInvoice_ReturnsOrders() {
        // Given
        List<Order> mockOrders = Arrays.asList(testOrder);
        when(invoiceRepository.findOrdersNeedingInvoice(any(Timestamp.class))).thenReturn(mockOrders);

        // When
        List<Order> result = invoiceService.getOrdersNeedingInvoice();

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(testOrder, result.get(0));
        verify(invoiceRepository).findOrdersNeedingInvoice(any(Timestamp.class));
    }
}

