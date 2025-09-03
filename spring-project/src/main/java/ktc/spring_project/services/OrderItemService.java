    package ktc.spring_project.services;

    import ktc.spring_project.entities.OrderItem;
    import ktc.spring_project.enums.ServiceType;
    import ktc.spring_project.repositories.OrderItemRepository;
    import org.springframework.beans.factory.annotation.Autowired;
    import org.springframework.stereotype.Service;

    import java.util.List;
    import java.util.Optional;

    @Service
    public class OrderItemService {
        @Autowired
        private OrderItemRepository orderItemRepository;

        @Autowired
        private OrderItemShippingService orderItemShippingService;

        public List<OrderItem> findAll() {
            return orderItemRepository.findAll();
        }

        public Optional<OrderItem> findById(Long id) {
            return orderItemRepository.findById(id);
        }

        public OrderItem save(OrderItem orderItem) {
            // Kiểm tra trùng lặp productCode
            if (orderItem.getProductCode() != null && orderItemRepository.findByProductCode(orderItem.getProductCode()).isPresent()) {
                throw new ktc.spring_project.exceptions.EntityDuplicateException("OrderItem productCode");
            }
            return orderItemRepository.save(orderItem);
        }

        /**
         * Lưu OrderItem và tự động tính shipping fee
         */
        public OrderItem saveWithShippingCalculation(OrderItem orderItem, ServiceType serviceType) {
            // Lưu OrderItem trước
            OrderItem savedOrderItem = orderItemRepository.save(orderItem);
            
            try {
                // Tính và cập nhật shipping fee
                orderItemShippingService.calculateAndUpdateShippingFee(savedOrderItem, serviceType);
            } catch (Exception e) {
                // Log lỗi nhưng không fail toàn bộ quá trình
                System.err.println("Không thể tính shipping fee cho OrderItem ID: " + 
                    savedOrderItem.getId() + " - " + e.getMessage());
            }
            
            return savedOrderItem;
        }

        /**
         * Lưu OrderItem với service type mặc định là STANDARD
         */
        public OrderItem saveWithShippingCalculation(OrderItem orderItem) {
            return saveWithShippingCalculation(orderItem, ServiceType.STANDARD);
        }

        public void delete(Long id) {
            orderItemRepository.deleteById(id);
        }

        public List<OrderItem> findByOrderId(Long orderId) {
            return orderItemRepository.findByOrderId(orderId);
        }

        public long countByOrderId(Long orderId) {
            return orderItemRepository.countItemsByOrderId(orderId);
        }

        /**
         * Tính lại shipping fee cho tất cả OrderItem trong một Order
         */
        public void recalculateShippingFeeForOrder(Long orderId, ServiceType serviceType) {
            List<OrderItem> orderItems = findByOrderId(orderId);
            orderItemShippingService.calculateShippingFeeForOrderItems(orderItems, serviceType);
        }

        /**
         * Tính lại shipping fee cho một OrderItem cụ thể
         */
        public void recalculateShippingFee(Long orderItemId, ServiceType serviceType) {
            Optional<OrderItem> orderItemOpt = findById(orderItemId);
            if (orderItemOpt.isPresent()) {
                orderItemShippingService.calculateAndUpdateShippingFee(orderItemOpt.get(), serviceType);
            }
        }
    }