package ktc.spring_project.repositories;

import ktc.spring_project.entities.ElectronicInvoice;
import ktc.spring_project.entities.Order;
import ktc.spring_project.entities.User;
import ktc.spring_project.enums.InvoiceStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

/**
 * Repository cho ElectronicInvoice entity
 * Cung cấp các phương thức truy vấn dữ liệu hóa đơn thanh toán
 */
@Repository
public interface ElectronicInvoiceRepository extends JpaRepository<ElectronicInvoice, Long> {

    /**
     * Tìm hóa đơn theo đơn hàng
     */
    Optional<ElectronicInvoice> findByOrder(Order order);

    /**
     * Tìm hóa đơn theo ID đơn hàng
     */
    Optional<ElectronicInvoice> findByOrderId(Long orderId);

    /**
     * Tìm hóa đơn theo số hóa đơn
     */
    Optional<ElectronicInvoice> findByInvoiceNumber(String invoiceNumber);

    /**
     * Tìm tất cả hóa đơn theo trạng thái
     */
    List<ElectronicInvoice> findByInvoiceStatus(InvoiceStatus status);

    /**
     * Tìm hóa đơn theo người tạo
     */
    List<ElectronicInvoice> findByCreatedBy(User createdBy);

    /**
     * Tìm hóa đơn theo email khách hàng
     */
    List<ElectronicInvoice> findByCustomerEmailContainingIgnoreCase(String email);

    /**
     * Tìm hóa đơn được tạo trong khoảng thời gian
     */
    @Query("SELECT ei FROM ElectronicInvoice ei WHERE ei.issuedAt BETWEEN :startDate AND :endDate ORDER BY ei.issuedAt DESC")
    List<ElectronicInvoice> findInvoicesIssuedBetween(@Param("startDate") Timestamp startDate,
                                                     @Param("endDate") Timestamp endDate);

    /**
     * Tìm hóa đơn theo trạng thái và sắp xếp theo thời gian tạo
     */
    @Query("SELECT ei FROM ElectronicInvoice ei WHERE ei.invoiceStatus = :status ORDER BY ei.issuedAt DESC")
    List<ElectronicInvoice> findByStatusOrderByIssuedAtDesc(@Param("status") InvoiceStatus status);

    /**
     * Kiểm tra xem đơn hàng đã có hóa đơn chưa
     */
    boolean existsByOrderId(Long orderId);

    /**
     * Kiểm tra xem số hóa đơn đã tồn tại chưa
     */
    boolean existsByInvoiceNumber(String invoiceNumber);

    /**
     * Đếm số hóa đơn theo trạng thái
     */
    @Query("SELECT COUNT(ei) FROM ElectronicInvoice ei WHERE ei.invoiceStatus = :status")
    long countByStatus(@Param("status") InvoiceStatus status);

    /**
     * Tìm hóa đơn có thể bị hết hạn (CREATED > 30 ngày)
     */
    @Query("SELECT ei FROM ElectronicInvoice ei WHERE ei.invoiceStatus = 'CREATED' " +
           "AND ei.issuedAt < :expiryDate")
    List<ElectronicInvoice> findExpiredInvoices(@Param("expiryDate") Timestamp expiryDate);

    /**
     * Tìm hóa đơn chưa gửi email sau một khoảng thời gian
     */
    @Query("SELECT ei FROM ElectronicInvoice ei WHERE ei.invoiceStatus IN ('CREATED', 'SENT') " +
           "AND ei.emailSentAt IS NULL AND ei.issuedAt < :cutoffDate")
    List<ElectronicInvoice> findUnsentInvoicesOlderThan(@Param("cutoffDate") Timestamp cutoffDate);

    /**
     * Tìm hóa đơn theo tên khách hàng (tìm kiếm mờ)
     */
    @Query("SELECT ei FROM ElectronicInvoice ei WHERE LOWER(ei.customerName) LIKE LOWER(CONCAT('%', :name, '%'))")
    List<ElectronicInvoice> findByCustomerNameContainingIgnoreCase(@Param("name") String name);

    /**
     * Thống kê số lượng hóa đơn được tạo trong tháng
     */
    @Query("SELECT COUNT(ei) FROM ElectronicInvoice ei WHERE " +
           "YEAR(ei.issuedAt) = :year AND MONTH(ei.issuedAt) = :month")
    long countInvoicesInMonth(@Param("year") int year, @Param("month") int month);

    /**
     * Tìm hóa đơn theo đơn hàng và trạng thái
     */
    @Query("SELECT ei FROM ElectronicInvoice ei WHERE ei.order.id = :orderId AND ei.invoiceStatus = :status")
    Optional<ElectronicInvoice> findByOrderIdAndStatus(@Param("orderId") Long orderId, 
                                                      @Param("status") InvoiceStatus status);

    /**
     * Tìm các đơn hàng đã hoàn thành giao hàng nhưng chưa có hóa đơn
     * (để hỗ trợ job tự động kiểm tra)
     */
    @Query("SELECT o FROM Order o " +
           "JOIN Delivery d ON d.order.id = o.id " +
           "WHERE d.actualDeliveryTime IS NOT NULL " +
           "AND o.id NOT IN (SELECT ei.order.id FROM ElectronicInvoice ei) " +
           "AND d.actualDeliveryTime < :cutoffDate")
    List<Order> findOrdersNeedingInvoice(@Param("cutoffDate") Timestamp cutoffDate);

    /**
     * Lấy hóa đơn mới nhất của người dùng
     */
    @Query("SELECT ei FROM ElectronicInvoice ei WHERE ei.createdBy.id = :userId " +
           "ORDER BY ei.issuedAt DESC")
    List<ElectronicInvoice> findLatestInvoicesByUser(@Param("userId") Long userId);
}
