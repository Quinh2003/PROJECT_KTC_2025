// delivery_detail.dart
// Models for delivery details API response

// Delivery detail model
class DeliveryDetail {
  final int id;
  final String? deliveryCode;
  final String? status;
  final int? statusId;
  final String? scheduledTime;
  final String? actualDeliveryTime;
  final double deliveryFee;
  final String? notes;
  final String? pickupAddress;
  final double? pickupLatitude;
  final double? pickupLongitude;
  final String? routeName;
  final int? routeId;
  final String? estimatedDistance;
  final String? estimatedDuration;
  final DriverInfo driver;
  final VehicleInfo vehicle;
  final List<OrderSummary> orders;
  final String? createdAt;
  final String? updatedAt;

  DeliveryDetail({
    required this.id,
    this.deliveryCode,
    this.status,
    this.statusId,
    this.scheduledTime,
    this.actualDeliveryTime,
    required this.deliveryFee,
    this.notes,
    this.pickupAddress,
    this.pickupLatitude,
    this.pickupLongitude,
    this.routeName,
    this.routeId,
    this.estimatedDistance,
    this.estimatedDuration,
    required this.driver,
    required this.vehicle,
    required this.orders,
    this.createdAt,
    this.updatedAt,
  });

  factory DeliveryDetail.fromJson(Map<String, dynamic> json) {
    return DeliveryDetail(
      id: json['id'] ?? 0,
      deliveryCode: json['deliveryCode'],
      status: json['status'],
      statusId: json['statusId'],
      scheduledTime: json['scheduledTime'],
      actualDeliveryTime: json['actualDeliveryTime'],
      deliveryFee: json['deliveryFee'] != null ? double.parse(json['deliveryFee'].toString()) : 0.0,
      notes: json['notes'],
      pickupAddress: json['pickupAddress'],
      pickupLatitude: json['pickupLatitude'],
      pickupLongitude: json['pickupLongitude'],
      routeName: json['routeName'],
      routeId: json['routeId'],
      estimatedDistance: json['estimatedDistance'],
      estimatedDuration: json['estimatedDuration'],
      driver: json['driver'] != null ? DriverInfo.fromJson(json['driver']) : DriverInfo.empty(),
      vehicle: json['vehicle'] != null ? VehicleInfo.fromJson(json['vehicle']) : VehicleInfo.empty(),
      orders: json['orders'] != null 
          ? List<OrderSummary>.from(json['orders'].map((x) => OrderSummary.fromJson(x)))
          : [],
      createdAt: json['createdAt'],
      updatedAt: json['updatedAt'],
    );
  }
}

// Driver Info model
class DriverInfo {
  final int id;
  final String fullName;
  final String email;
  final String phone;
  final String? address;
  final bool? isActive;
  final String? notes;
  final int? roleId;
  final String? roleName;
  final String? roleDescription;
  final String? createdAt;
  final String? updatedAt;
  final String displayName;
  final bool activeUser;

  DriverInfo({
    required this.id,
    required this.fullName,
    required this.email,
    required this.phone,
    this.address,
    this.isActive,
    this.notes,
    this.roleId,
    this.roleName,
    this.roleDescription,
    this.createdAt,
    this.updatedAt,
    required this.displayName,
    required this.activeUser,
  });

  factory DriverInfo.fromJson(Map<String, dynamic> json) {
    return DriverInfo(
      id: json['id'] ?? 0,
      fullName: json['fullName'] ?? '',
      email: json['email'] ?? '',
      phone: json['phone'] ?? '',
      address: json['address'],
      isActive: json['isActive'],
      notes: json['notes'],
      roleId: json['roleId'],
      roleName: json['roleName'],
      roleDescription: json['roleDescription'],
      createdAt: json['createdAt'],
      updatedAt: json['updatedAt'],
      displayName: json['displayName'] ?? '',
      activeUser: json['activeUser'] ?? false,
    );
  }

  factory DriverInfo.empty() {
    return DriverInfo(
      id: 0,
      fullName: 'Unknown',
      email: '',
      phone: '',
      displayName: 'Unknown',
      activeUser: false,
    );
  }
}

// Vehicle Info model
class VehicleInfo {
  final int id;
  final String licensePlate;
  final String vehicleType;
  final String? model;

  VehicleInfo({
    required this.id,
    required this.licensePlate,
    required this.vehicleType,
    this.model,
  });

  factory VehicleInfo.fromJson(Map<String, dynamic> json) {
    return VehicleInfo(
      id: json['id'] ?? 0,
      licensePlate: json['licensePlate'] ?? '',
      vehicleType: json['vehicleType'] ?? '',
      model: json['model'],
    );
  }

  factory VehicleInfo.empty() {
    return VehicleInfo(
      id: 0,
      licensePlate: 'Unknown',
      vehicleType: 'Unknown',
    );
  }
}

// Order Summary model
class OrderSummary {
  final int id;
  final String? orderCode;
  final String status;
  final int statusId;
  final String? deliveryAddress;
  final String? recipientName;
  final String? recipientPhone;
  final double totalAmount;
  final String createdAt;

  OrderSummary({
    required this.id,
    this.orderCode,
    required this.status,
    required this.statusId,
    this.deliveryAddress,
    this.recipientName,
    this.recipientPhone,
    required this.totalAmount,
    required this.createdAt,
  });

  factory OrderSummary.fromJson(Map<String, dynamic> json) {
    return OrderSummary(
      id: json['id'] ?? 0,
      orderCode: json['orderCode'],
      status: json['status'] ?? 'Unknown',
      statusId: json['statusId'] ?? 0,
      deliveryAddress: json['deliveryAddress'],
      recipientName: json['recipientName'],
      recipientPhone: json['recipientPhone'],
      totalAmount: json['totalAmount'] != null ? double.parse(json['totalAmount'].toString()) : 0.0,
      createdAt: json['createdAt'] ?? '',
    );
  }
}
