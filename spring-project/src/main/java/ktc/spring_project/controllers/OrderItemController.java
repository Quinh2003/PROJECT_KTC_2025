

package ktc.spring_project.controllers;

import ktc.spring_project.dtos.orderitem.CreateOrderItemRequestDTO;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.OrderItem;
import ktc.spring_project.entities.Product;
import ktc.spring_project.repositories.OrderRepository;
import ktc.spring_project.repositories.ProductRepository;
import ktc.spring_project.services.OrderItemService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import jakarta.validation.Valid;

import java.net.URI;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/order-items")
public class OrderItemController {
    // Lấy tất cả order item (có phân trang)
    @GetMapping
    public ResponseEntity<?> getAllOrderItemsPaged(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size
    ) {
        org.springframework.data.domain.Pageable pageable = org.springframework.data.domain.PageRequest.of(page, size);
        org.springframework.data.domain.Page<OrderItem> pageResult = orderItemService.findAllPaged(pageable);
        return ResponseEntity.ok(pageResult);
    }
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private OrderRepository orderRepository;
    @Autowired
    private ProductRepository productRepository;

    // Tạo mới order item
    @PostMapping
    public ResponseEntity<?> createOrderItem(@Valid @RequestBody CreateOrderItemRequestDTO dto) {
        Optional<Order> orderOpt = orderRepository.findById(dto.getOrderId());
        Optional<Product> productOpt = productRepository.findById(dto.getProductId());
        if (orderOpt.isEmpty() || productOpt.isEmpty()) {
            return ResponseEntity.badRequest().body("Order or Product not found");
        }
        OrderItem orderItem = new OrderItem();
        orderItem.setOrder(orderOpt.get());
        orderItem.setProduct(productOpt.get());
        orderItem.setQuantity(dto.getQuantity());
        orderItem.setNotes(dto.getNotes());
        
        // SỬ DỤNG SHIPPING FEE TỪ FRONTEND THAY VÌ TÍNH LẠI
        if (dto.getShippingFee() != null) {
            orderItem.setShippingFee(dto.getShippingFee());
            OrderItem saved = orderItemService.save(orderItem); // Dùng save() thường, không tính lại
            return ResponseEntity.created(URI.create("/api/order-items/" + saved.getId())).body(saved);
        } else {
            // Fallback: chỉ tính lại nếu frontend không gửi shippingFee
            OrderItem saved = orderItemService.saveWithShippingCalculation(orderItem);
            return ResponseEntity.created(URI.create("/api/order-items/" + saved.getId())).body(saved);
        }
    }

    // Lấy order item theo id
    @GetMapping("/{id}")
    public ResponseEntity<?> getOrderItem(@PathVariable Long id) {
        Optional<OrderItem> orderItemOpt = orderItemService.findById(id);
        return orderItemOpt.map(ResponseEntity::ok)
                .orElseGet(() -> ResponseEntity.notFound().build());
    }

    // Xoá order item
    @DeleteMapping("/{id}")
    public ResponseEntity<?> deleteOrderItem(@PathVariable Long id) {
        orderItemService.delete(id);
        return ResponseEntity.noContent().build();
    }


    // Lấy tất cả order item của 1 order (có phân trang, mặc định size=5)
    @GetMapping("/order/{orderId}")
    public ResponseEntity<?> getOrderItemsByOrderIdPagedDefault(
            @PathVariable Long orderId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size
    ) {
        org.springframework.data.domain.Pageable pageable = org.springframework.data.domain.PageRequest.of(page, size);
        org.springframework.data.domain.Page<OrderItem> pageResult = orderItemService.findByOrderIdPaged(orderId, pageable);
        return ResponseEntity.ok(pageResult);
    }

    // Lấy order item của 1 order có phân trang
    @GetMapping("/order/{orderId}/paged")
    public ResponseEntity<?> getOrderItemsByOrderIdPaged(
            @PathVariable Long orderId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size
    ) {
        org.springframework.data.domain.Pageable pageable = org.springframework.data.domain.PageRequest.of(page, size);
        org.springframework.data.domain.Page<OrderItem> pageResult = orderItemService.findByOrderIdPaged(orderId, pageable);
        return ResponseEntity.ok(pageResult);
    }


    // Lấy tổng số lượng sản phẩm của 1 order (tối ưu, không load toàn bộ list)
    @GetMapping("/order/{orderId}/total-quantity")
    public ResponseEntity<Long> getTotalQuantityByOrderId(@PathVariable Long orderId) {
        long total = orderItemService.getTotalQuantityByOrderId(orderId);
        return ResponseEntity.ok(total);
    }

    // Đếm số order item của 1 order (tối ưu)
    @GetMapping("/order/{orderId}/count")
    public ResponseEntity<Long> countOrderItemsByOrderId(@PathVariable Long orderId) {
        long count = orderItemService.countByOrderId(orderId);
        return ResponseEntity.ok(count);
    }

    // Batch endpoint: lấy tổng quantity cho nhiều orderId
    @PostMapping("/orders/total-quantity")
    public ResponseEntity<?> getTotalQuantityByOrderIds(@RequestBody List<Long> orderIds) {
        return ResponseEntity.ok(orderItemService.getTotalQuantityByOrderIds(orderIds));
    }
}

