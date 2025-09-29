# Data Transfer Objects (DTOs) Documentation

## Overview
This directory contains comprehensive DTOs for the KTC Logistics Management System, covering all major entities and operations for supply chain and delivery management.

## Completed DTO Packages

### 1. Address Management (`/address/`)
- `CreateAddressRequestDTO` - For creating delivery/pickup addresses
- `AddressResponseDTO` - Complete address information with GPS coordinates
- `UpdateAddressRequestDTO` - For updating existing addresses

### 2. Category Management (`/category/`)
- `CreateCategoryRequestDTO` - For creating product categories
- `CategoryResponseDTO` - Category information with hierarchy and product counts
- `UpdateCategoryRequestDTO` - For updating category information

### 3. Store Management (`/store/`)
- `CreateStoreRequestDTO` - For creating new store locations
- `StoreResponseDTO` - Store information with business metrics
- `UpdateStoreRequestDTO` - For updating store details

### 4. Warehouse Management (`/warehouse/`)
- `CreateWarehouseRequestDTO` - For creating warehouse facilities
- `WarehouseResponseDTO` - Warehouse information with capacity and utilization
- `UpdateWarehouseRequestDTO` - For updating warehouse details

### 5. Delivery Management (`/delivery/`)
- `CreateDeliveryRequestDTO` - For scheduling new deliveries
- `DeliveryResponseDTO` - Complete delivery information with tracking
- `UpdateDeliveryRequestDTO` - For updating delivery status and details

### 6. Order Item Management (`/orderitem/`)
- `CreateOrderItemRequestDTO` - For adding items to orders
- `OrderItemResponseDTO` - Order item details with product information
- `UpdateOrderItemRequestDTO` - For modifying order items

### 7. Role Management (`/role/`)
- `CreateRoleRequestDTO` - For creating user roles
- `RoleResponseDTO` - Role information with permissions and user counts
- `UpdateRoleRequestDTO` - For updating role configurations

### 8. Delivery Proof Management (`/deliveryproof/`)
- `CreateDeliveryProofRequestDTO` - For uploading delivery evidence
- `DeliveryProofResponseDTO` - Proof details with file information

### 9. Warehouse Transaction Management (`/warehousetransaction/`)
- `CreateWarehouseTransactionRequestDTO` - For inventory movements
- `WarehouseTransactionResponseDTO` - Transaction details with cost calculations

## Key Features

### Validation Methods
All DTOs include comprehensive validation methods:
- `isValid()` - Basic validation
- Type-specific validations (e.g., `hasCoordinates()`, `isFragileItem()`)
- Business logic validations

### Utility Methods
Rich utility methods for:
- Display formatting (e.g., `getDisplayName()`, `getFormattedPrice()`)
- Status checking (e.g., `isActiveStore()`, `isCompleted()`)
- Data transformations

### Comprehensive Information
Response DTOs include:
- Entity relationships (flattened for easy access)
- Calculated fields (totals, summaries)
- Display-ready formatted data
- Business metrics and statistics

## Design Patterns

### Request DTOs
- Include only fields needed for creation/updates
- Comprehensive validation methods
- Business logic helper methods

### Response DTOs
- Include full entity data plus related entity information
- Rich utility methods for display and business logic
- Formatted data for UI consumption

### Common Patterns
- Constructor overloads for common use cases
- Null-safe utility methods
- Consistent naming conventions
- Type-safe enum handling

## Usage Examples

### Creating a New Delivery
```java
CreateDeliveryRequestDTO request = new CreateDeliveryRequestDTO(
    orderId, vehicleId, scheduleTime);
request.setServiceType(ServiceType.EXPRESS);
request.setDeliveryNotes("Handle with care");
```

### Displaying Order Items
```java
OrderItemResponseDTO item = orderService.getOrderItem(id);
System.out.println(item.getDisplayName()); // "SKU123 - Product Name"
System.out.println(item.getFormattedTotal()); // "$99.99"
System.out.println(item.getFragileIndicator()); // "⚠️ FRAGILE" or ""
```

## Integration with Entities

All DTOs are designed to work seamlessly with the corresponding JPA entities:
- Field names match entity properties
- Enum types are consistent
- Relationship mapping is preserved
- Database constraints are reflected in validation

## Future Enhancements

The DTO structure supports:
- JSON serialization/deserialization
- Spring Boot validation annotations
- MapStruct mapping integration
- API documentation generation
- Frontend framework integration

## Notes

- All monetary values use `BigDecimal` for precision
- Timestamps use `java.sql.Timestamp` for database compatibility
- Enums are used for type safety and validation
- Utility methods provide display-ready formatted data
- Validation methods support both client and server-side validation