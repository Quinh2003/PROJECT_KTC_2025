/*
 * File: backend_route_models.dart
 * Purpose: Consolidated models for route management from Spring Boot backend
 * Context: Route management screens, viewing lists, details and route status
 */

// ===================== Response Models =====================

class RoutesResponse {
  final bool success;
  final String message;
  final List<RouteDetail> routes;
  final Pagination pagination;

  RoutesResponse({
    required this.success,
    required this.message,
    required this.routes,
    required this.pagination,
  });

  factory RoutesResponse.fromJson(Map<String, dynamic> json) {
    return RoutesResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      routes: (json['data']?['routes'] as List?)
              ?.map((route) => RouteDetail.fromJson(route))
              .toList() ??
          [],
      pagination: Pagination.fromJson(json['data']?['pagination'] ?? {}),
    );
  }
}

class RouteResponse {
  final bool success;
  final String message;
  final RouteDetail route;

  RouteResponse({
    required this.success,
    required this.message,
    required this.route,
  });

  factory RouteResponse.fromJson(Map<String, dynamic> json) {
    return RouteResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      route: RouteDetail.fromJson(json['data'] ?? {}),
    );
  }
}

// ===================== Core Models =====================

class RouteDetail {
  final String id;
  final String name;
  final String description;
  final String status;
  final String driverId;
  final String? driverName;
  final String vehicleId;
  final DateTime createdAt;
  final DateTime? startedAt;
  final DateTime? completedAt;
  final List<Delivery> deliveries;
  final RouteStatistics statistics;

  RouteDetail({
    required this.id,
    required this.name,
    required this.description,
    required this.status,
    required this.driverId,
    this.driverName,
    required this.vehicleId,
    required this.createdAt,
    this.startedAt,
    this.completedAt,
    required this.deliveries,
    required this.statistics,
  });

  factory RouteDetail.fromJson(Map<String, dynamic> json) {
    return RouteDetail(
      id: json['id'] ?? '',
      name: json['name'] ?? '',
      description: json['description'] ?? '',
      status: json['status'] ?? '',
      driverId: json['driverId'] ?? '',
      driverName: json['driverName'],
      vehicleId: json['vehicleId'] ?? '',
      createdAt: DateTime.parse(json['createdAt'] ?? DateTime.now().toIso8601String()),
      startedAt: json['startedAt'] != null ? DateTime.parse(json['startedAt']) : null,
      completedAt: json['completedAt'] != null ? DateTime.parse(json['completedAt']) : null,
      deliveries: (json['deliveries'] as List?)
              ?.map((delivery) => Delivery.fromJson(delivery))
              .toList() ??
          [],
      statistics: RouteStatistics.fromJson(json['statistics'] ?? {}),
    );
  }
}

class Delivery {
  final String id;
  final String orderId;
  final String customerId;
  final String customerName;
  final String customerPhone;
  final Address pickupAddress;
  final Address deliveryAddress;
  final String status;
  final DateTime createdAt;
  final DateTime? scheduledAt;
  final DateTime? completedAt;
  final double totalValue;
  final List<DeliveryItem> items;

  Delivery({
    required this.id,
    required this.orderId,
    required this.customerId,
    required this.customerName,
    required this.customerPhone,
    required this.pickupAddress,
    required this.deliveryAddress,
    required this.status,
    required this.createdAt,
    this.scheduledAt,
    this.completedAt,
    required this.totalValue,
    required this.items,
  });

  factory Delivery.fromJson(Map<String, dynamic> json) {
    return Delivery(
      id: json['id'] ?? '',
      orderId: json['orderId'] ?? '',
      customerId: json['customerId'] ?? '',
      customerName: json['customerName'] ?? '',
      customerPhone: json['customerPhone'] ?? '',
      pickupAddress: Address.fromJson(json['pickupAddress'] ?? {}),
      deliveryAddress: Address.fromJson(json['deliveryAddress'] ?? {}),
      status: json['status'] ?? '',
      createdAt: DateTime.parse(json['createdAt'] ?? DateTime.now().toIso8601String()),
      scheduledAt: json['scheduledAt'] != null ? DateTime.parse(json['scheduledAt']) : null,
      completedAt: json['completedAt'] != null ? DateTime.parse(json['completedAt']) : null,
      totalValue: json['totalValue']?.toDouble() ?? 0.0,
      items: (json['items'] as List?)
              ?.map((item) => DeliveryItem.fromJson(item))
              .toList() ??
          [],
    );
  }
}

class Address {
  final String street;
  final String ward;
  final String district;
  final String city;
  final String country;
  final double latitude;
  final double longitude;

  Address({
    required this.street,
    required this.ward,
    required this.district,
    required this.city,
    required this.country,
    required this.latitude,
    required this.longitude,
  });

  factory Address.fromJson(Map<String, dynamic> json) {
    return Address(
      street: json['street'] ?? '',
      ward: json['ward'] ?? '',
      district: json['district'] ?? '',
      city: json['city'] ?? '',
      country: json['country'] ?? '',
      latitude: json['latitude']?.toDouble() ?? 0.0,
      longitude: json['longitude']?.toDouble() ?? 0.0,
    );
  }

  String get fullAddress => '$street, $ward, $district, $city, $country';
}

class DeliveryItem {
  final String id;
  final String name;
  final int quantity;
  final double unitPrice;
  final double totalPrice;

  DeliveryItem({
    required this.id,
    required this.name,
    required this.quantity,
    required this.unitPrice,
    required this.totalPrice,
  });

  factory DeliveryItem.fromJson(Map<String, dynamic> json) {
    return DeliveryItem(
      id: json['id'] ?? '',
      name: json['name'] ?? '',
      quantity: json['quantity'] ?? 0,
      unitPrice: json['unitPrice']?.toDouble() ?? 0.0,
      totalPrice: json['totalPrice']?.toDouble() ?? 0.0,
    );
  }
}

class RouteStatistics {
  final int totalDeliveries;
  final int completedDeliveries;
  final int pendingDeliveries;
  final double totalDistance;
  final Duration estimatedDuration;
  final Duration actualDuration;

  RouteStatistics({
    required this.totalDeliveries,
    required this.completedDeliveries,
    required this.pendingDeliveries,
    required this.totalDistance,
    required this.estimatedDuration,
    required this.actualDuration,
  });

  factory RouteStatistics.fromJson(Map<String, dynamic> json) {
    return RouteStatistics(
      totalDeliveries: json['totalDeliveries'] ?? 0,
      completedDeliveries: json['completedDeliveries'] ?? 0,
      pendingDeliveries: json['pendingDeliveries'] ?? 0,
      totalDistance: json['totalDistance']?.toDouble() ?? 0.0,
      estimatedDuration: Duration(seconds: json['estimatedDurationSeconds'] ?? 0),
      actualDuration: Duration(seconds: json['actualDurationSeconds'] ?? 0),
    );
  }
}

class Pagination {
  final int currentPage;
  final int totalPages;
  final int totalItems;
  final int itemsPerPage;

  Pagination({
    required this.currentPage,
    required this.totalPages,
    required this.totalItems,
    required this.itemsPerPage,
  });

  factory Pagination.fromJson(Map<String, dynamic> json) {
    return Pagination(
      currentPage: json['currentPage'] ?? 1,
      totalPages: json['totalPages'] ?? 1,
      totalItems: json['totalItems'] ?? 0,
      itemsPerPage: json['itemsPerPage'] ?? 10,
    );
  }
}

// ===================== Enums =====================

enum RouteStatus {
  created,
  assigned,
  inProgress,
  completed,
  cancelled,
}

enum DeliveryStatus {
  pending,
  inTransit,
  delivered,
  failed,
  cancelled,
}

extension RouteStatusExtension on RouteStatus {
  String get value {
    switch (this) {
      case RouteStatus.created:
        return 'CREATED';
      case RouteStatus.assigned:
        return 'ASSIGNED';
      case RouteStatus.inProgress:
        return 'IN_PROGRESS';
      case RouteStatus.completed:
        return 'COMPLETED';
      case RouteStatus.cancelled:
        return 'CANCELLED';
    }
  }
}

extension DeliveryStatusExtension on DeliveryStatus {
  String get value {
    switch (this) {
      case DeliveryStatus.pending:
        return 'PENDING';
      case DeliveryStatus.inTransit:
        return 'IN_TRANSIT';
      case DeliveryStatus.delivered:
        return 'DELIVERED';
      case DeliveryStatus.failed:
        return 'FAILED';
      case DeliveryStatus.cancelled:
        return 'CANCELLED';
    }
  }
}
