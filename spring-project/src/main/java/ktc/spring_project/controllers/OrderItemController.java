
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

import java.net.URI;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/order-items")
public class OrderItemController {
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private OrderRepository orderRepository;
    @Autowired
    private ProductRepository productRepository;

    // Tạo mới order item
    @PostMapping
    public ResponseEntity<?> createOrderItem(@RequestBody CreateOrderItemRequestDTO dto) {
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
        OrderItem saved = orderItemService.saveWithShippingCalculation(orderItem);
        return ResponseEntity.created(URI.create("/api/order-items/" + saved.getId())).body(saved);
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

    // Lấy tất cả order item của 1 order
    @GetMapping("/order/{orderId}")
    public ResponseEntity<List<OrderItem>> getOrderItemsByOrderId(@PathVariable Long orderId) {
        List<OrderItem> items = orderItemService.findByOrderId(orderId);
        return ResponseEntity.ok(items);
    }

    // Lấy tổng số lượng sản phẩm của 1 order
    @GetMapping("/order/{orderId}/total-quantity")
    public ResponseEntity<Integer> getTotalQuantityByOrderId(@PathVariable Long orderId) {
        List<OrderItem> items = orderItemService.findByOrderId(orderId);
        int total = items.stream().mapToInt(OrderItem::getQuantity).sum();
        return ResponseEntity.ok(total);
    }

    // Đếm số order item của 1 order
    @GetMapping("/order/{orderId}/count")
    public ResponseEntity<Long> countOrderItemsByOrderId(@PathVariable Long orderId) {
        long count = orderItemService.countByOrderId(orderId);
        return ResponseEntity.ok(count);
    }
}

