// delivery.dart
// Models cho thông tin giao hàng và tài xế

// Model cho tài xế giao hàng (đổi tên từ Delivery cũ thành DeliveryPerson)
class DeliveryPerson {
  final int personId;             // ID tài xế
  final String nameDelivery;      // Tên tài xế
  final String phone;             // Số điện thoại
  final String image;             // URL ảnh đại diện
  final String notificationToken; // Token cho notification

  // Constructor với các tham số bắt buộc
  DeliveryPerson({
    required this.personId,
    required this.nameDelivery,
    required this.phone,
    required this.image,
    required this.notificationToken,
  });

  // Factory constructor để parse JSON thành object
  factory DeliveryPerson.fromJson(Map<String, dynamic> json) {
    return DeliveryPerson(
      personId: json['personId'] ?? 0,
      nameDelivery: json['nameDelivery'] ?? '',
      phone: json['phone'] ?? '',
      image: json['image'] ?? '',
      notificationToken: json['notificationToken'] ?? '',
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'personId': personId,
      'nameDelivery': nameDelivery,
      'phone': phone,
      'image': image,
      'notificationToken': notificationToken,
    };
  }
}

// Model mới cho thông tin đơn giao hàng theo API
class Delivery {
  final int id;
  final int orderId;
  final String? orderNumber;
  final String? orderDescription;
  final int? statusId;
  final String? deliveryStatus;
  final double deliveryFee;
  final String? transportMode;
  final String? serviceType;
  final String? pickupDate;
  final String? scheduleDeliveryTime;
  final String? scheduledTime; // Thêm scheduledTime
  final String? actualDeliveryTime;
  final int? lateDeliveryRisk;
  final int? deliveryAttempts;
  final String? deliveryNotes;
  final String? orderDate;
  final int vehicleId;
  final String? vehicleLicensePlate;
  final String? vehicleType;
  final int? driverId;
  final String? driverName;
  final String? driverPhone;
  final String? deliveryAddress;
  final String? trackingId;
  final String? currentLocation;
  final int? routeId;
  final String? routeName;
  final String? estimatedDistance;
  final String? estimatedDuration;
  final String? createdAt;
  final String? updatedAt;
  final bool completed;
  final bool delayed;
  final bool pending;
  final bool onTime;
  final bool highRisk;
  final String driverDisplay;
  final String estimatedInfo;
  final String formattedFee;
  final bool inTransit;
  final String statusDisplay;
  final String vehicleDisplay;
  final String serviceTypeDisplay;
  final String transportModeDisplay;

  Delivery({
    required this.id,
    required this.orderId,
    this.orderNumber,
    this.orderDescription,
    this.statusId,
    this.deliveryStatus,
    required this.deliveryFee,
    this.transportMode,
    this.serviceType,
    this.pickupDate,
    this.scheduleDeliveryTime,
    this.actualDeliveryTime,
    this.scheduledTime, // Thêm scheduledTime
    this.lateDeliveryRisk,
    this.deliveryAttempts,
    this.deliveryNotes,
    this.orderDate,
    required this.vehicleId,
    this.vehicleLicensePlate,
    this.vehicleType,
    this.driverId,
    this.driverName,
    this.driverPhone,
    this.deliveryAddress,
    this.trackingId,
    this.currentLocation,
    this.routeId,
    this.routeName,
    this.estimatedDistance,
    this.estimatedDuration,
    this.createdAt,
    this.updatedAt,
    required this.completed,
    required this.delayed,
    required this.pending,
    required this.onTime,
    required this.highRisk,
    required this.driverDisplay,
    required this.estimatedInfo,
    required this.formattedFee,
    required this.inTransit,
    required this.statusDisplay,
    required this.vehicleDisplay,
    required this.serviceTypeDisplay,
    required this.transportModeDisplay,
  });

  factory Delivery.fromJson(Map<String, dynamic> json) {
    // Handle deliveryFee which might be int, double, or null
    double parseDeliveryFee() {
      var fee = json['deliveryFee'];
      if (fee == null) return 0.0;
      if (fee is int) return fee.toDouble();
      if (fee is double) return fee;
      if (fee is String) return double.tryParse(fee) ?? 0.0;
      return 0.0;
    }

    return Delivery(
      id: json['id'] ?? 0,
      orderId: json['orderId'] ?? 0,
      orderNumber: json['orderNumber'],
      orderDescription: json['orderDescription'],
      statusId: json['statusId'],
      deliveryStatus: json['deliveryStatus'],
      deliveryFee: parseDeliveryFee(),
      transportMode: json['transportMode'],
      serviceType: json['serviceType'],
      pickupDate: json['pickupDate'],
      scheduleDeliveryTime: json['scheduleDeliveryTime'],
      scheduledTime: json['scheduledTime'], // Thêm đọc scheduledTime từ JSON
      actualDeliveryTime: json['actualDeliveryTime'],
      lateDeliveryRisk: json['lateDeliveryRisk'],
      deliveryAttempts: json['deliveryAttempts'],
      deliveryNotes: json['deliveryNotes'],
      orderDate: json['orderDate'],
      vehicleId: json['vehicleId'] ?? 0,
      vehicleLicensePlate: json['vehicleLicensePlate'],
      vehicleType: json['vehicleType'],
      driverId: json['driverId'],
      driverName: json['driverName'],
      driverPhone: json['driverPhone'],
      deliveryAddress: json['deliveryAddress'],
      trackingId: json['trackingId'],
      currentLocation: json['currentLocation'],
      routeId: json['routeId'],
      routeName: json['routeName'],
      estimatedDistance: json['estimatedDistance'],
      estimatedDuration: json['estimatedDuration'],
      createdAt: json['createdAt'],
      updatedAt: json['updatedAt'],
      completed: json['completed'] ?? false,
      delayed: json['delayed'] ?? false,
      pending: json['pending'] ?? false,
      onTime: json['onTime'] ?? false,
      highRisk: json['highRisk'] ?? false,
      driverDisplay: json['driverDisplay'] ?? 'Unknown',
      estimatedInfo: json['estimatedInfo'] ?? 'No estimate',
      formattedFee: json['formattedFee'] ?? '\$0.00',
      inTransit: json['inTransit'] ?? false,
      statusDisplay: json['statusDisplay'] ?? 'Unknown',
      vehicleDisplay: json['vehicleDisplay'] ?? 'Unknown',
      serviceTypeDisplay: json['serviceTypeDisplay'] ?? 'Standard',
      transportModeDisplay: json['transportModeDisplay'] ?? 'Road',
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'orderId': orderId,
      'orderNumber': orderNumber,
      'orderDescription': orderDescription,
      'statusId': statusId,
      'deliveryStatus': deliveryStatus,
      'deliveryFee': deliveryFee,
      'transportMode': transportMode,
      'serviceType': serviceType,
      'pickupDate': pickupDate,
      'scheduleDeliveryTime': scheduleDeliveryTime,
      'scheduledTime': scheduledTime, // Thêm scheduledTime vào toJson
      'actualDeliveryTime': actualDeliveryTime,
      'lateDeliveryRisk': lateDeliveryRisk,
      'deliveryAttempts': deliveryAttempts,
      'deliveryNotes': deliveryNotes,
      'orderDate': orderDate,
      'vehicleId': vehicleId,
      'vehicleLicensePlate': vehicleLicensePlate,
      'vehicleType': vehicleType,
      'driverId': driverId,
      'driverName': driverName,
      'driverPhone': driverPhone,
      'deliveryAddress': deliveryAddress,
      'trackingId': trackingId,
      'currentLocation': currentLocation,
      'routeId': routeId,
      'routeName': routeName,
      'estimatedDistance': estimatedDistance,
      'estimatedDuration': estimatedDuration,
      'createdAt': createdAt,
      'updatedAt': updatedAt,
      'completed': completed,
      'delayed': delayed,
      'pending': pending,
      'onTime': onTime,
      'highRisk': highRisk,
      'driverDisplay': driverDisplay,
      'estimatedInfo': estimatedInfo,
      'formattedFee': formattedFee,
      'inTransit': inTransit,
      'statusDisplay': statusDisplay,
      'vehicleDisplay': vehicleDisplay,
      'serviceTypeDisplay': serviceTypeDisplay,
      'transportModeDisplay': transportModeDisplay,
    };
  }

  static List<Delivery> fromJsonList(List<dynamic> jsonList) {
    return jsonList.map((item) => Delivery.fromJson(item)).toList();
  }
}
