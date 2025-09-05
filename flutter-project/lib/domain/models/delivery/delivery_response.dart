// delivery_response.dart
// Model response cho các API liên quan đến giao hàng

class DeliveryResponse {
  final bool success;          // Trạng thái thành công
  final String message;        // Thông báo
  final Map<String, dynamic>? data; // Dữ liệu phản hồi

  // Constructor với các tham số bắt buộc
  DeliveryResponse({
    required this.success,
    required this.message,
    this.data,
  });

  // Factory constructor để parse JSON thành object
  factory DeliveryResponse.fromJson(Map<String, dynamic> json) {
    return DeliveryResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      data: json['data'],
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'success': success,
      'message': message,
      'data': data,
    };
  }
}

// Model response cho API cập nhật trạng thái đơn hàng
class OrderStatusUpdateResponse {
  final bool success;     // Trạng thái thành công
  final String message;   // Thông báo
  final String orderId;   // ID đơn hàng
  final String status;    // Trạng thái mới
  final DateTime updatedAt; // Thời gian cập nhật

  // Constructor với các tham số bắt buộc
  OrderStatusUpdateResponse({
    required this.success,
    required this.message,
    required this.orderId,
    required this.status,
    required this.updatedAt,
  });

  // Factory constructor để parse JSON thành object
  factory OrderStatusUpdateResponse.fromJson(Map<String, dynamic> json) {
    return OrderStatusUpdateResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      orderId: json['orderId'] ?? '',
      status: json['status'] ?? '',
      updatedAt: json['updatedAt'] != null 
          ? DateTime.parse(json['updatedAt']) 
          : DateTime.now(),
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'success': success,
      'message': message,
      'orderId': orderId,
      'status': status,
      'updatedAt': updatedAt.toIso8601String(),
    };
  }
}

// Model response cho API cập nhật vị trí tài xế
class LocationUpdateResponse {
  final bool success;    // Trạng thái thành công
  final String message;  // Thông báo
  final double latitude;  // Vĩ độ mới
  final double longitude; // Kinh độ mới
  final DateTime timestamp; // Thời gian cập nhật

  // Constructor với các tham số bắt buộc
  LocationUpdateResponse({
    required this.success,
    required this.message,
    required this.latitude,
    required this.longitude,
    required this.timestamp,
  });

  // Factory constructor để parse JSON thành object
  factory LocationUpdateResponse.fromJson(Map<String, dynamic> json) {
    return LocationUpdateResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      latitude: json['latitude']?.toDouble() ?? 0.0,
      longitude: json['longitude']?.toDouble() ?? 0.0,
      timestamp: json['timestamp'] != null 
          ? DateTime.parse(json['timestamp']) 
          : DateTime.now(),
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'success': success,
      'message': message,
      'latitude': latitude,
      'longitude': longitude,
      'timestamp': timestamp.toIso8601String(),
    };
  }
}


