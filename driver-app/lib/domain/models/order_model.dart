// order_model.dart
// Model cho thông tin đơn hàng

import 'package:google_maps_flutter/google_maps_flutter.dart';

class Order {
  final String id;               // ID đơn hàng
  final String status;           // Trạng thái đơn hàng
  final DateTime createdAt;      // Thời gian tạo đơn
  final DateTime? deliveredAt;   // Thời gian giao hàng
  final Customer customer;       // Thông tin khách hàng
  final PickupLocation pickup;   // Địa điểm lấy hàng
  final DeliveryLocation delivery; // Địa điểm giao hàng
  final List<OrderItem> items;   // Danh sách sản phẩm
  final double totalAmount;      // Tổng tiền
  final String note;             // Ghi chú
  final String paymentMethod;    // Phương thức thanh toán
  final bool isPaid;             // Đã thanh toán chưa
  final double distance;         // Khoảng cách giao hàng (km)
  final int estimatedTime;       // Thời gian dự kiến (phút)
  
  // Constructor với các tham số bắt buộc
  Order({
    required this.id,
    required this.status,
    required this.createdAt,
    this.deliveredAt,
    required this.customer,
    required this.pickup,
    required this.delivery,
    required this.items,
    required this.totalAmount,
    required this.note,
    required this.paymentMethod,
    required this.isPaid,
    required this.distance,
    required this.estimatedTime,
  });

  // Factory constructor để parse JSON thành object
  factory Order.fromJson(Map<String, dynamic> json) {
    // Parse danh sách sản phẩm
    List<OrderItem> itemsList = [];
    if (json['items'] != null) {
      itemsList = List<OrderItem>.from(
        json['items'].map((item) => OrderItem.fromJson(item))
      );
    }

    return Order(
      id: json['id'] ?? '',
      status: json['status'] ?? '',
      createdAt: json['createdAt'] != null 
          ? DateTime.parse(json['createdAt']) 
          : DateTime.now(),
      deliveredAt: json['deliveredAt'] != null 
          ? DateTime.parse(json['deliveredAt']) 
          : null,
      customer: Customer.fromJson(json['customer']),
      pickup: PickupLocation.fromJson(json['pickup']),
      delivery: DeliveryLocation.fromJson(json['delivery']),
      items: itemsList,
      totalAmount: json['totalAmount']?.toDouble() ?? 0.0,
      note: json['note'] ?? '',
      paymentMethod: json['paymentMethod'] ?? '',
      isPaid: json['isPaid'] ?? false,
      distance: json['distance']?.toDouble() ?? 0.0,
      estimatedTime: json['estimatedTime'] ?? 0,
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'status': status,
      'createdAt': createdAt.toIso8601String(),
      'deliveredAt': deliveredAt?.toIso8601String(),
      'customer': customer.toJson(),
      'pickup': pickup.toJson(),
      'delivery': delivery.toJson(),
      'items': items.map((item) => item.toJson()).toList(),
      'totalAmount': totalAmount,
      'note': note,
      'paymentMethod': paymentMethod,
      'isPaid': isPaid,
      'distance': distance,
      'estimatedTime': estimatedTime,
    };
  }
}

// Model cho thông tin khách hàng
class Customer {
  final String id;       // ID khách hàng
  final String name;     // Tên khách hàng
  final String phone;    // Số điện thoại
  final String email;    // Email
  final String avatar;   // URL ảnh đại diện

  // Constructor với các tham số bắt buộc
  Customer({
    required this.id,
    required this.name,
    required this.phone,
    required this.email,
    required this.avatar,
  });

  // Factory constructor để parse JSON thành object
  factory Customer.fromJson(Map<String, dynamic> json) {
    return Customer(
      id: json['id'] ?? '',
      name: json['name'] ?? '',
      phone: json['phone'] ?? '',
      email: json['email'] ?? '',
      avatar: json['avatar'] ?? '',
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'name': name,
      'phone': phone,
      'email': email,
      'avatar': avatar,
    };
  }
}

// Model cho địa điểm lấy hàng
class PickupLocation {
  final String address;  // Địa chỉ
  final double latitude;  // Vĩ độ
  final double longitude; // Kinh độ

  // Constructor với các tham số bắt buộc
  PickupLocation({
    required this.address,
    required this.latitude,
    required this.longitude,
  });

  // Chuyển đổi thành LatLng cho Google Maps
  LatLng toLatLng() {
    return LatLng(latitude, longitude);
  }

  // Factory constructor để parse JSON thành object
  factory PickupLocation.fromJson(Map<String, dynamic> json) {
    return PickupLocation(
      address: json['address'] ?? '',
      latitude: json['latitude']?.toDouble() ?? 0.0,
      longitude: json['longitude']?.toDouble() ?? 0.0,
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'address': address,
      'latitude': latitude,
      'longitude': longitude,
    };
  }
}

// Model cho địa điểm giao hàng
class DeliveryLocation {
  final String address;  // Địa chỉ
  final double latitude;  // Vĩ độ
  final double longitude; // Kinh độ

  // Constructor với các tham số bắt buộc
  DeliveryLocation({
    required this.address,
    required this.latitude,
    required this.longitude,
  });

  // Chuyển đổi thành LatLng cho Google Maps
  LatLng toLatLng() {
    return LatLng(latitude, longitude);
  }

  // Factory constructor để parse JSON thành object
  factory DeliveryLocation.fromJson(Map<String, dynamic> json) {
    return DeliveryLocation(
      address: json['address'] ?? '',
      latitude: json['latitude']?.toDouble() ?? 0.0,
      longitude: json['longitude']?.toDouble() ?? 0.0,
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'address': address,
      'latitude': latitude,
      'longitude': longitude,
    };
  }
}

// Model cho sản phẩm trong đơn hàng
class OrderItem {
  final String id;         // ID sản phẩm
  final String name;       // Tên sản phẩm
  final String image;      // URL ảnh sản phẩm
  final int quantity;      // Số lượng
  final double price;      // Giá tiền
  final double totalPrice; // Tổng tiền
  
  // Constructor với các tham số bắt buộc
  OrderItem({
    required this.id,
    required this.name,
    required this.image,
    required this.quantity,
    required this.price,
    required this.totalPrice,
  });

  // Factory constructor để parse JSON thành object
  factory OrderItem.fromJson(Map<String, dynamic> json) {
    return OrderItem(
      id: json['id'] ?? '',
      name: json['name'] ?? '',
      image: json['image'] ?? '',
      quantity: json['quantity'] ?? 0,
      price: json['price']?.toDouble() ?? 0.0,
      totalPrice: json['totalPrice']?.toDouble() ?? 0.0,
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'name': name,
      'image': image,
      'quantity': quantity,
      'price': price,
      'totalPrice': totalPrice,
    };
  }
}


