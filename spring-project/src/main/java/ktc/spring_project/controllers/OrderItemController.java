
package ktc.spring_project.controllers;

import ktc.spring_project.entities.OrderItem;
import ktc.spring_project.services.OrderItemService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/order-items")
public class OrderItemController {
    @Autowired
    private OrderItemService orderItemService;

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

    // Lấy order item theo id
    @GetMapping("/{id}")
    public ResponseEntity<OrderItem> getOrderItemById(@PathVariable Long id) {
        return orderItemService.findById(id)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }
}

