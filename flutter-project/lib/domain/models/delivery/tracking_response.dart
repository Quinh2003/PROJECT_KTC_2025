// tracking_response.dart
// Response models for tracking related data from Spring Boot backend

class OrderTrackingResponse {
  final bool success;
  final String message;
  final List<TrackingPoint> trackingPoints;

  OrderTrackingResponse({
    required this.success,
    required this.message,
    required this.trackingPoints,
  });

  factory OrderTrackingResponse.fromJson(Map<String, dynamic> json) {
    return OrderTrackingResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      trackingPoints: (json['data'] as List?)
              ?.map((point) => TrackingPoint.fromJson(point))
              .toList() ??
          [],
    );
  }
}

class DeliveryTrackingResponse {
  final bool success;
  final String message;
  final List<TrackingPoint> trackingPoints;

  DeliveryTrackingResponse({
    required this.success,
    required this.message,
    required this.trackingPoints,
  });

  factory DeliveryTrackingResponse.fromJson(Map<String, dynamic> json) {
    return DeliveryTrackingResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      trackingPoints: (json['data'] as List?)
              ?.map((point) => TrackingPoint.fromJson(point))
              .toList() ??
          [],
    );
  }
}

class RouteTrackingResponse {
  final bool success;
  final String message;
  final List<TrackingPoint> trackingPoints;

  RouteTrackingResponse({
    required this.success,
    required this.message,
    required this.trackingPoints,
  });

  factory RouteTrackingResponse.fromJson(Map<String, dynamic> json) {
    return RouteTrackingResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      trackingPoints: (json['data'] as List?)
              ?.map((point) => TrackingPoint.fromJson(point))
              .toList() ??
          [],
    );
  }
}

class TrackingPoint {
  final String id;
  final double latitude;
  final double longitude;
  final String timestamp;
  final Map<String, dynamic>? metadata;

  TrackingPoint({
    required this.id,
    required this.latitude,
    required this.longitude,
    required this.timestamp,
    this.metadata,
  });

  factory TrackingPoint.fromJson(Map<String, dynamic> json) {
    return TrackingPoint(
      id: json['id'] ?? '',
      latitude: (json['latitude'] as num?)?.toDouble() ?? 0.0,
      longitude: (json['longitude'] as num?)?.toDouble() ?? 0.0,
      timestamp: json['timestamp'] ?? '',
      metadata: json['metadata'],
    );
  }
}

class VehicleLocationResponse {
  final bool success;
  final String message;
  final VehicleLocation location;

  VehicleLocationResponse({
    required this.success,
    required this.message,
    required this.location,
  });

  factory VehicleLocationResponse.fromJson(Map<String, dynamic> json) {
    return VehicleLocationResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      location: VehicleLocation.fromJson(json['data'] ?? {}),
    );
  }
}

class VehicleLocation {
  final String vehicleId;
  final String driverId;
  final double latitude;
  final double longitude;
  final String timestamp;
  final String? status;

  VehicleLocation({
    required this.vehicleId,
    required this.driverId,
    required this.latitude,
    required this.longitude,
    required this.timestamp,
    this.status,
  });

  factory VehicleLocation.fromJson(Map<String, dynamic> json) {
    return VehicleLocation(
      vehicleId: json['vehicleId'] ?? '',
      driverId: json['driverId'] ?? '',
      latitude: (json['latitude'] as num?)?.toDouble() ?? 0.0,
      longitude: (json['longitude'] as num?)?.toDouble() ?? 0.0,
      timestamp: json['timestamp'] ?? '',
      status: json['status'],
    );
  }
}

class TrackingHistoryResponse {
  final bool success;
  final String message;
  final List<TrackingPoint> history;

  TrackingHistoryResponse({
    required this.success,
    required this.message,
    required this.history,
  });

  factory TrackingHistoryResponse.fromJson(Map<String, dynamic> json) {
    return TrackingHistoryResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      history: (json['data'] as List?)
              ?.map((point) => TrackingPoint.fromJson(point))
              .toList() ??
          [],
    );
  }
}

class LocationUpdateResponse {
  final bool success;
  final String message;
  final TrackingPoint? data;

  LocationUpdateResponse({
    required this.success,
    required this.message,
    this.data,
  });

  factory LocationUpdateResponse.fromJson(Map<String, dynamic> json) {
    return LocationUpdateResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      data: json['data'] != null ? TrackingPoint.fromJson(json['data']) : null,
    );
  }
}

class DriverStatusUpdateResponse {
  final bool success;
  final String message;
  final String status;

  DriverStatusUpdateResponse({
    required this.success,
    required this.message,
    required this.status,
  });

  factory DriverStatusUpdateResponse.fromJson(Map<String, dynamic> json) {
    return DriverStatusUpdateResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      status: json['data']?['status'] ?? '',
    );
  }
}


