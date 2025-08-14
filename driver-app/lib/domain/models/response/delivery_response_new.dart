// delivery_response_new.dart
// Response models for delivery related data from Spring Boot backend

import 'package:ktc_logistics_driver/domain/models/response/route_response.dart';

class DeliveriesResponse {
  final bool success;
  final String message;
  final List<Delivery> deliveries;
  final Pagination pagination;

  DeliveriesResponse({
    required this.success,
    required this.message,
    required this.deliveries,
    required this.pagination,
  });

  factory DeliveriesResponse.fromJson(Map<String, dynamic> json) {
    return DeliveriesResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      deliveries: (json['data']?['deliveries'] as List?)
              ?.map((delivery) => Delivery.fromJson(delivery))
              .toList() ??
          [],
      pagination: Pagination.fromJson(json['data']?['pagination'] ?? {}),
    );
  }
}

class Delivery {
  final String id;
  final String orderId;
  final String driverId;
  final String vehicleId;
  final String routeId;
  final String status;
  final String startTime;
  final String? endTime;
  final String? notes;
  final List<DeliveryStop> stops;
  final String createdAt;
  final String updatedAt;

  Delivery({
    required this.id,
    required this.orderId,
    required this.driverId,
    required this.vehicleId,
    required this.routeId,
    required this.status,
    required this.startTime,
    this.endTime,
    this.notes,
    required this.stops,
    required this.createdAt,
    required this.updatedAt,
  });

  factory Delivery.fromJson(Map<String, dynamic> json) {
    return Delivery(
      id: json['id'] ?? '',
      orderId: json['orderId'] ?? '',
      driverId: json['driverId'] ?? '',
      vehicleId: json['vehicleId'] ?? '',
      routeId: json['routeId'] ?? '',
      status: json['status'] ?? '',
      startTime: json['startTime'] ?? '',
      endTime: json['endTime'],
      notes: json['notes'],
      stops: (json['stops'] as List?)
              ?.map((stop) => DeliveryStop.fromJson(stop))
              .toList() ??
          [],
      createdAt: json['createdAt'] ?? '',
      updatedAt: json['updatedAt'] ?? '',
    );
  }
}

class DeliveryStop {
  final String id;
  final int sequenceNumber;
  final String address;
  final double latitude;
  final double longitude;
  final String type; // pickup, delivery
  final String status;
  final String? scheduledTime;
  final String? actualTime;
  final String? notes;

  DeliveryStop({
    required this.id,
    required this.sequenceNumber,
    required this.address,
    required this.latitude,
    required this.longitude,
    required this.type,
    required this.status,
    this.scheduledTime,
    this.actualTime,
    this.notes,
  });

  factory DeliveryStop.fromJson(Map<String, dynamic> json) {
    return DeliveryStop(
      id: json['id'] ?? '',
      sequenceNumber: json['sequenceNumber'] ?? 0,
      address: json['address'] ?? '',
      latitude: (json['latitude'] as num?)?.toDouble() ?? 0.0,
      longitude: (json['longitude'] as num?)?.toDouble() ?? 0.0,
      type: json['type'] ?? '',
      status: json['status'] ?? '',
      scheduledTime: json['scheduledTime'],
      actualTime: json['actualTime'],
      notes: json['notes'],
    );
  }
}


