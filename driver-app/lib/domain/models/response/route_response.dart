// route_response.dart
// Response models for route related data from Spring Boot backend

import 'package:ktc_logistics_driver/domain/models/response/tracking_response.dart';

class RoutesResponse {
  final bool success;
  final String message;
  final List<Route> routes;
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
              ?.map((route) => Route.fromJson(route))
              .toList() ??
          [],
      pagination: Pagination.fromJson(json['data']?['pagination'] ?? {}),
    );
  }
}

class RouteResponse {
  final bool success;
  final String message;
  final Route route;

  RouteResponse({
    required this.success,
    required this.message,
    required this.route,
  });

  factory RouteResponse.fromJson(Map<String, dynamic> json) {
    return RouteResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      route: Route.fromJson(json['data'] ?? {}),
    );
  }
}

class Route {
  final String id;
  final String name;
  final String startPoint;
  final String endPoint;
  final double distance;
  final String estimatedTime;
  final String status;
  final String? driverId;
  final String? vehicleId;
  final List<RouteWaypoint> waypoints;
  final String createdAt;
  final String updatedAt;

  Route({
    required this.id,
    required this.name,
    required this.startPoint,
    required this.endPoint,
    required this.distance,
    required this.estimatedTime,
    required this.status,
    this.driverId,
    this.vehicleId,
    required this.waypoints,
    required this.createdAt,
    required this.updatedAt,
  });

  factory Route.fromJson(Map<String, dynamic> json) {
    return Route(
      id: json['id'] ?? '',
      name: json['name'] ?? '',
      startPoint: json['startPoint'] ?? '',
      endPoint: json['endPoint'] ?? '',
      distance: (json['distance'] as num?)?.toDouble() ?? 0.0,
      estimatedTime: json['estimatedTime'] ?? '',
      status: json['status'] ?? '',
      driverId: json['driverId'],
      vehicleId: json['vehicleId'],
      waypoints: (json['waypoints'] as List?)
              ?.map((waypoint) => RouteWaypoint.fromJson(waypoint))
              .toList() ??
          [],
      createdAt: json['createdAt'] ?? '',
      updatedAt: json['updatedAt'] ?? '',
    );
  }
}

class RouteWaypoint {
  final String id;
  final double latitude;
  final double longitude;
  final String address;
  final int sequenceNumber;
  final String? type;
  final String? notes;

  RouteWaypoint({
    required this.id,
    required this.latitude,
    required this.longitude,
    required this.address,
    required this.sequenceNumber,
    this.type,
    this.notes,
  });

  factory RouteWaypoint.fromJson(Map<String, dynamic> json) {
    return RouteWaypoint(
      id: json['id'] ?? '',
      latitude: (json['latitude'] as num?)?.toDouble() ?? 0.0,
      longitude: (json['longitude'] as num?)?.toDouble() ?? 0.0,
      address: json['address'] ?? '',
      sequenceNumber: json['sequenceNumber'] ?? 0,
      type: json['type'],
      notes: json['notes'],
    );
  }
}

class Pagination {
  final int page;
  final int perPage;
  final int totalItems;
  final int totalPages;

  Pagination({
    required this.page,
    required this.perPage,
    required this.totalItems,
    required this.totalPages,
  });

  factory Pagination.fromJson(Map<String, dynamic> json) {
    return Pagination(
      page: json['page'] ?? 1,
      perPage: json['perPage'] ?? 10,
      totalItems: json['totalItems'] ?? 0,
      totalPages: json['totalPages'] ?? 0,
    );
  }
}


