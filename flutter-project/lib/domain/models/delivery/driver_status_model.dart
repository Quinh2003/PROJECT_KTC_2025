// driver_status_model.dart
// Model cho trạng thái của tài xế

class DriverStatus {
  final String driverId;    // ID tài xế
  final bool isOnline;      // Đang hoạt động hay không
  final bool isAvailable;   // Có sẵn sàng nhận đơn không
  final String status;      // Trạng thái hiện tại (IDLE, DELIVERING, RESTING...)
  final double latitude;    // Vĩ độ hiện tại
  final double longitude;   // Kinh độ hiện tại
  final DateTime updatedAt; // Thời gian cập nhật
  final String? currentOrderId; // ID đơn hàng đang giao (nếu có)

  // Constructor với các tham số bắt buộc
  DriverStatus({
    required this.driverId,
    required this.isOnline,
    required this.isAvailable,
    required this.status,
    required this.latitude,
    required this.longitude,
    required this.updatedAt,
    this.currentOrderId,
  });

  // Factory constructor để parse JSON thành object
  factory DriverStatus.fromJson(Map<String, dynamic> json) {
    return DriverStatus(
      driverId: json['driverId'] ?? '',
      isOnline: json['isOnline'] ?? false,
      isAvailable: json['isAvailable'] ?? false,
      status: json['status'] ?? 'OFFLINE',
      latitude: json['latitude']?.toDouble() ?? 0.0,
      longitude: json['longitude']?.toDouble() ?? 0.0,
      updatedAt: json['updatedAt'] != null 
          ? DateTime.parse(json['updatedAt']) 
          : DateTime.now(),
      currentOrderId: json['currentOrderId'],
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'driverId': driverId,
      'isOnline': isOnline,
      'isAvailable': isAvailable,
      'status': status,
      'latitude': latitude,
      'longitude': longitude,
      'updatedAt': updatedAt.toIso8601String(),
      'currentOrderId': currentOrderId,
    };
  }

  // Tạo một bản sao của đối tượng với các thuộc tính được cập nhật
  DriverStatus copyWith({
    bool? isOnline,
    bool? isAvailable,
    String? status,
    double? latitude,
    double? longitude,
    String? currentOrderId,
  }) {
    return DriverStatus(
      driverId: driverId,
      isOnline: isOnline ?? this.isOnline,
      isAvailable: isAvailable ?? this.isAvailable,
      status: status ?? this.status,
      latitude: latitude ?? this.latitude,
      longitude: longitude ?? this.longitude,
      updatedAt: DateTime.now(),
      currentOrderId: currentOrderId ?? this.currentOrderId,
    );
  }
}


