// order_status_update.dart
// Model cho cập nhật trạng thái đơn hàng

// Status constants for order status - matched with database IDs
class OrderStatus {
  static const String PENDING = 'PENDING';           // Awaiting processing - ID: 1
  static const String PROCESSING = 'PROCESSING';     // Order being processed - ID: 3
  static const String SHIPPING = 'SHIPPING';         // Order shipped - ID: 5
  static const String DELIVERED = 'DELIVERED';       // Order delivered but not fully paid - ID: 6
  static const String COMPLETED = 'COMPLETED';       // Order completed - ID: 2
  static const String CANCELLED = 'CANCELLED';       // Order cancelled - ID: 4
}

class OrderStatusUpdate {
  final int statusId;
  final String? notes;

  OrderStatusUpdate({
    required this.statusId,
    this.notes,
  });

  Map<String, dynamic> toJson() {
    final Map<String, dynamic> data = {
      'statusId': statusId,
    };

    if (notes != null) data['notes'] = notes;
    
    return data;
  }
  
  factory OrderStatusUpdate.fromJson(Map<String, dynamic> json) {
    return OrderStatusUpdate(
      statusId: json['statusId'],
      notes: json['notes'],
    );
  }
}

// Mapping statusId cho đơn hàng - matched with database IDs from table
class OrderStatusId {
  // Status IDs matching database table values - chỉ dùng đúng 6 trạng thái trong bảng
  static const int PENDING = 1;         // Awaiting processing - ID: 1
  static const int COMPLETED = 2;       // Order completed - ID: 2
  static const int CANCELLED = 3;       // Order cancelled - ID: 3
  static const int PROCESSING = 4;      // Order being processed - ID: 4
  static const int SHIPPING = 5;        // Order shipped - ID: 5
  static const int DELIVERED = 6;       // Order delivered - ID: 6
  
  // Chuyển đổi từ tên trạng thái sang id
  static int fromStatusName(String statusName) {
    switch (statusName.toUpperCase()) {
      case 'PENDING':
        return PENDING;
      case 'COMPLETED':
        return COMPLETED;
      case 'CANCELLED':
        return CANCELLED;
      case 'PROCESSING':
        return PROCESSING;
      case 'SHIPPING':
        return SHIPPING;
      case 'DELIVERED':
        return DELIVERED;
      default:
        return PENDING; // Default to PENDING
    }
  }
}
