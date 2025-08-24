// order_response.dart
// Model response cho API danh sách đơn hàng

import '../order_model.dart';

class OrdersResponse {
  final bool success;      // Trạng thái thành công
  final String message;    // Thông báo
  final List<Order> orders; // Danh sách đơn hàng
  final Pagination pagination; // Thông tin phân trang

  // Constructor với các tham số bắt buộc
  OrdersResponse({
    required this.success,
    required this.message,
    required this.orders,
    required this.pagination,
  });

  // Factory constructor để parse JSON thành object
  factory OrdersResponse.fromJson(Map<String, dynamic> json) {
    // Parse danh sách đơn hàng
    List<Order> ordersList = [];
    if (json['orders'] != null) {
      ordersList = List<Order>.from(
        json['orders'].map((order) => Order.fromJson(order))
      );
    }

    return OrdersResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      orders: ordersList,
      pagination: Pagination.fromJson(json['pagination'] ?? {}),
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'success': success,
      'message': message,
      'orders': orders.map((order) => order.toJson()).toList(),
      'pagination': pagination.toJson(),
    };
  }
}

// Model cho thông tin chi tiết một đơn hàng
class OrderDetailResponse {
  final bool success;   // Trạng thái thành công
  final String message; // Thông báo
  final Order order;    // Thông tin đơn hàng

  // Constructor với các tham số bắt buộc
  OrderDetailResponse({
    required this.success,
    required this.message,
    required this.order,
  });

  // Factory constructor để parse JSON thành object
  factory OrderDetailResponse.fromJson(Map<String, dynamic> json) {
    return OrderDetailResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      order: Order.fromJson(json['order'] ?? {}),
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'success': success,
      'message': message,
      'order': order.toJson(),
    };
  }
}

// Model cho thông tin phân trang
class Pagination {
  final int total;      // Tổng số đơn hàng
  final int perPage;    // Số đơn hàng mỗi trang
  final int currentPage; // Trang hiện tại
  final int lastPage;   // Trang cuối cùng

  // Constructor với các tham số bắt buộc
  Pagination({
    required this.total,
    required this.perPage,
    required this.currentPage,
    required this.lastPage,
  });

  // Factory constructor để parse JSON thành object
  factory Pagination.fromJson(Map<String, dynamic> json) {
    return Pagination(
      total: json['total'] ?? 0,
      perPage: json['perPage'] ?? 10,
      currentPage: json['currentPage'] ?? 1,
      lastPage: json['lastPage'] ?? 1,
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'total': total,
      'perPage': perPage,
      'currentPage': currentPage,
      'lastPage': lastPage,
    };
  }
}


