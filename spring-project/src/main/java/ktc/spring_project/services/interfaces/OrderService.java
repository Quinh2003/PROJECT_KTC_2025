package ktc.spring_project.services.interfaces;

import ktc.spring_project.dtos.order.CreateOrderRequestDTO;
import ktc.spring_project.dtos.order.OrderResponseDTO;
import ktc.spring_project.dtos.common.PagedResponse;
import ktc.spring_project.entities.Order;
import ktc.spring_project.enums.TransportMode;
import org.springframework.data.domain.Pageable;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Service interface for Order management operations
 * Handles order lifecycle, assignment, and business logic
 */
public interface OrderService {
    
    // ===== ORDER LIFECYCLE =====
    /**
     * Create a new order with items and calculate shipping fees
     * @param createOrderDTO Order creation data
     * @param customerId Customer who places the order
     * @return Created order response
     */
    OrderResponseDTO createOrder(CreateOrderRequestDTO createOrderDTO, Long customerId);
    
    /**
     * Update order details (before assignment)
     * @param orderId Order ID
     * @param updateData Updated order data
     * @return Updated order response
     */
    OrderResponseDTO updateOrder(Long orderId, CreateOrderRequestDTO updateData);
    
    /**
     * Get order by ID
     * @param orderId Order ID
     * @return Order response or empty if not found
     */
    Optional<OrderResponseDTO> getOrderById(Long orderId);
    
    /**
     * Get order entity by ID (for internal service use)
     * @param orderId Order ID
     * @return Order entity or empty if not found
     */
    Optional<Order> getOrderEntityById(Long orderId);
    
    /**
     * Get order by order code
     * @param orderCode Unique order code
     * @return Order response or empty if not found
     */
    Optional<OrderResponseDTO> getOrderByCode(String orderCode);
    
    // ===== ORDER STATUS MANAGEMENT =====
    /**
     * Update order status
     * @param orderId Order ID
     * @param statusCode New status code (PENDING, READY, ON_DELIVERY, DELIVERED, CANCELLED)
     * @param updatedBy User who updates the status
     * @return Updated order response
     */
    OrderResponseDTO updateOrderStatus(Long orderId, String statusCode, Long updatedBy);
    
    /**
     * Cancel order (if cancellable)
     * @param orderId Order ID
     * @param cancelledBy User who cancels the order
     * @param reason Cancellation reason
     * @return Updated order response
     */
    OrderResponseDTO cancelOrder(Long orderId, Long cancelledBy, String reason);
    
    /**
     * Mark order as delivered
     * @param orderId Order ID
     * @param deliveredBy Driver who delivered the order
     * @param actualDeliveryTime Actual delivery timestamp
     * @return Updated order response
     */
    OrderResponseDTO markAsDelivered(Long orderId, Long deliveredBy, LocalDateTime actualDeliveryTime);
    
    // ===== ORDER ASSIGNMENT =====
    /**
     * Assign driver and vehicle to order
     * @param orderId Order ID
     * @param driverId Driver ID
     * @param vehicleId Vehicle ID
     * @param assignedBy User who makes the assignment
     * @return Updated order response
     */
    OrderResponseDTO assignDriverAndVehicle(Long orderId, Long driverId, Long vehicleId, Long assignedBy);
    
    /**
     * Assign route to order
     * @param orderId Order ID
     * @param routeId Route ID
     * @param assignedBy User who makes the assignment
     * @return Updated order response
     */
    OrderResponseDTO assignRoute(Long orderId, Long routeId, Long assignedBy);
    
    /**
     * Unassign driver and vehicle from order
     * @param orderId Order ID
     * @param unassignedBy User who removes the assignment
     * @return Updated order response
     */
    OrderResponseDTO unassignDriverAndVehicle(Long orderId, Long unassignedBy);
    
    // ===== ORDER QUERIES =====
    /**
     * Get all orders with pagination
     * @param pageable Pagination parameters
     * @return Paged order responses
     */
    PagedResponse<OrderResponseDTO> getAllOrders(Pageable pageable);
    
    /**
     * Get orders by customer
     * @param customerId Customer ID
     * @param pageable Pagination parameters
     * @return Paged customer orders
     */
    PagedResponse<OrderResponseDTO> getOrdersByCustomer(Long customerId, Pageable pageable);
    
    /**
     * Get orders by driver
     * @param driverId Driver ID
     * @param pageable Pagination parameters
     * @return Paged driver orders
     */
    PagedResponse<OrderResponseDTO> getOrdersByDriver(Long driverId, Pageable pageable);
    
    /**
     * Get orders by vehicle
     * @param vehicleId Vehicle ID
     * @param pageable Pagination parameters
     * @return Paged vehicle orders
     */
    PagedResponse<OrderResponseDTO> getOrdersByVehicle(Long vehicleId, Pageable pageable);
    
    /**
     * Get orders by status
     * @param statusCode Status code
     * @param pageable Pagination parameters
     * @return Paged orders with specified status
     */
    PagedResponse<OrderResponseDTO> getOrdersByStatus(String statusCode, Pageable pageable);
    
    /**
     * Get orders by transport mode
     * @param transportMode Transport mode
     * @param pageable Pagination parameters
     * @return Paged orders with specified transport mode
     */
    PagedResponse<OrderResponseDTO> getOrdersByTransportMode(TransportMode transportMode, Pageable pageable);
    
    // ===== BUSINESS QUERIES =====
    /**
     * Get unassigned orders (no driver/vehicle assigned)
     * @param pageable Pagination parameters
     * @return Paged unassigned orders
     */
    PagedResponse<OrderResponseDTO> getUnassignedOrders(Pageable pageable);
    
    /**
     * Get overdue orders (past scheduled time but not delivered)
     * @param pageable Pagination parameters
     * @return Paged overdue orders
     */
    PagedResponse<OrderResponseDTO> getOverdueOrders(Pageable pageable);
    
    /**
     * Get orders scheduled for today
     * @param pageable Pagination parameters
     * @return Paged today's orders
     */
    PagedResponse<OrderResponseDTO> getTodaysOrders(Pageable pageable);
    
    /**
     * Get orders within date range
     * @param startDate Start date
     * @param endDate End date
     * @param pageable Pagination parameters
     * @return Paged orders within date range
     */
    PagedResponse<OrderResponseDTO> getOrdersByDateRange(LocalDateTime startDate, LocalDateTime endDate, Pageable pageable);
    
    // ===== SEARCH AND ANALYTICS =====
    /**
     * Search orders by order code, customer name, or delivery address
     * @param searchTerm Search term
     * @param pageable Pagination parameters
     * @return Paged search results
     */
    PagedResponse<OrderResponseDTO> searchOrders(String searchTerm, Pageable pageable);
    
    /**
     * Calculate total revenue for date range
     * @param startDate Start date
     * @param endDate End date
     * @return Total revenue amount
     */
    BigDecimal calculateRevenue(LocalDateTime startDate, LocalDateTime endDate);
    
    /**
     * Get order statistics
     * @return Map of order statistics (total, by status, revenue, etc.)
     */
    java.util.Map<String, Object> getOrderStatistics();
    
    /**
     * Get top customers by order count
     * @param limit Number of top customers to return
     * @return List of customer statistics
     */
    List<java.util.Map<String, Object>> getTopCustomers(int limit);
}