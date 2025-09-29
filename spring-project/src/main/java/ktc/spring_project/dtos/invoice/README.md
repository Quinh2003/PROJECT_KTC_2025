# Electronic Invoice System - DTO Documentation

## Overview
This directory contains Data Transfer Objects (DTOs) for the Electronic Invoice functionality in the KTC Logistics Management System.

## DTOs Structure

### 📝 Request DTOs

#### `CreateInvoiceRequestDTO`
Used for creating new electronic invoices.

**Fields:**
- `orderId` (Long, required): ID of the order to create invoice for
- `customerEmail` (String, optional): Customer email address
- `customerName` (String, optional): Customer full name  
- `notes` (String, optional): Additional notes for the invoice

**Validation:**
- Order ID must not be null
- Email must be valid format if provided
- Customer name max 255 characters
- Notes max 1000 characters

**Example:**
```json
{
  "orderId": 123,
  "customerEmail": "customer@example.com",
  "customerName": "Nguyễn Văn A",
  "notes": "Special delivery invoice"
}
```

#### `CancelInvoiceRequestDTO`
Used for cancelling existing invoices.

**Fields:**
- `cancellationReason` (String, required): Reason for cancellation

**Validation:**
- Cancellation reason must not be blank
- Max 1000 characters

**Example:**
```json
{
  "cancellationReason": "Customer requested cancellation due to incorrect information"
}
```

### 📤 Response DTOs

#### `InvoiceResponseDTO`
Comprehensive response containing all invoice information.

**Key Fields:**
- `id`: Invoice ID
- `invoiceNumber`: Unique invoice number (format: INV-YYYYMMDD-HHMMSS-XXX)
- `invoiceStatus`: Current status (CREATED, SENT, DELIVERED, CANCELLED, EXPIRED)
- `orderId`: Associated order ID
- `totalAmount`: Total invoice amount
- `taxAmount`: VAT amount (10%)
- `netAmount`: Amount without tax
- `customerEmail`: Customer email
- `customerName`: Customer name
- `issuedAt`: Invoice creation timestamp
- `pdfFilePath`: Path to PDF file
- `emailSentAt`: Email sent timestamp
- `createdByName`: User who created the invoice

**Business Logic Methods:**
- `isCancellable()`: Check if invoice can be cancelled
- `isCompleted()`: Check if invoice is completed
- `hasPdfFile()`: Check if PDF file exists
- `isEmailSent()`: Check if email was sent
- `getDisplayName()`: Get formatted display name
- `getSummary()`: Get invoice summary

**Example:**
```json
{
  "id": 456,
  "invoiceNumber": "INV-20250108-143022-001",
  "invoiceStatus": "CREATED",
  "invoiceStatusDisplay": "Đã tạo",
  "orderId": 123,
  "totalAmount": 1500000,
  "totalAmountFormatted": "1,500,000.00 VND",
  "taxAmount": 150000,
  "netAmount": 1350000,
  "customerEmail": "customer@example.com",
  "customerName": "Nguyễn Văn A",
  "issuedAt": "2025-01-08T14:30:22",
  "issuedAtFormatted": "08/01/2025 14:30",
  "createdByName": "Admin User",
  "createdByEmail": "admin@example.com",
  "pdfFilePath": "invoices/pdfs/invoice_INV-20250108-143022-001.pdf",
  "notes": "Special delivery invoice"
}
```

## Usage Examples

### Creating an Invoice
```java
CreateInvoiceRequestDTO request = new CreateInvoiceRequestDTO();
request.setOrderId(123L);
request.setCustomerEmail("customer@example.com");
request.setCustomerName("Nguyễn Văn A");
request.sanitize(); // Clean input data

if (request.isValid()) {
    // Process the request
}
```

### Handling Invoice Response
```java
InvoiceResponseDTO invoice = invoiceService.getInvoiceById(456L);

// Business logic checks
if (invoice.isCancellable()) {
    // Allow cancellation
}

if (invoice.isEmailSent()) {
    // Show email sent status
}

// Display information
String summary = invoice.getSummary();
String displayName = invoice.getDisplayName();
```

### Cancelling an Invoice
```java
CancelInvoiceRequestDTO request = new CancelInvoiceRequestDTO();
request.setCancellationReason("Customer requested cancellation");
request.sanitize();

if (request.isValid()) {
    // Process cancellation
}
```

## Validation Rules

### Email Validation
- Must follow standard email format: `example@domain.com`
- Case insensitive
- Automatically converted to lowercase

### Data Sanitization
All DTOs include `sanitize()` method that:
- Trims whitespace
- Converts email to lowercase
- Removes unnecessary characters

### Business Rules
- Each order can only have one invoice
- Invoice must be created within 5 days of delivery completion
- Only CREATED and EXPIRED invoices can be cancelled
- Email can only be sent to non-cancelled invoices

## Error Handling

### Common Validation Errors
- `orderId is required`
- `Invalid email format`
- `Customer name must not exceed 255 characters`
- `Cancellation reason is required`

### Business Logic Errors
- `Order already has an electronic invoice`
- `Order delivery not completed`
- `Invoice creation deadline exceeded`
- `Cannot cancel invoice in current status`

## Integration with API

These DTOs are used in the following API endpoints:

### POST `/api/invoices`
- Request: `CreateInvoiceRequestDTO`
- Response: `ApiResponse<InvoiceResponseDTO>`

### POST `/api/invoices/{id}/cancel`
- Request: `CancelInvoiceRequestDTO`
- Response: `ApiResponse<String>`

### GET `/api/invoices/{id}`
- Response: `ApiResponse<InvoiceResponseDTO>`

### GET `/api/invoices`
- Response: `ApiResponse<List<InvoiceResponseDTO>>`

## Testing

Example test cases for DTOs:

```java
@Test
void testCreateInvoiceRequestDTO_Valid() {
    CreateInvoiceRequestDTO dto = new CreateInvoiceRequestDTO();
    dto.setOrderId(123L);
    dto.setCustomerEmail("test@example.com");
    
    assertTrue(dto.isValid());
    assertTrue(dto.hasCustomerInfo());
}

@Test
void testInvoiceResponseDTO_BusinessLogic() {
    InvoiceResponseDTO dto = new InvoiceResponseDTO(invoice);
    
    assertTrue(dto.isCancellable());
    assertFalse(dto.isEmailSent());
    assertEquals("INV-123 - Customer Name", dto.getDisplayName());
}
```

## Best Practices

1. **Always validate DTOs** before processing
2. **Use sanitize()** method to clean input data
3. **Check business logic methods** before performing operations
4. **Handle validation errors** gracefully
5. **Use formatted fields** for display purposes
6. **Leverage utility methods** for business decisions

---

*Last updated: January 8, 2025*
*Version: 1.0*

