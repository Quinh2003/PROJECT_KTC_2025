import 'dart:convert';
import 'package:http/http.dart' as http;

import '../../domain/models/delivery/tracking_model.dart' hide RoutesResponse, RouteResponse;
import '../../domain/models/map/backend_route_models.dart';
import '../../domain/repositories/tracking_repository_interfaces.dart';
import '../env/environment.dart';

class TrackingRepositoryImpl implements TrackingRepository {
  final String baseUrl;
  final http.Client httpClient;

  TrackingRepositoryImpl({
    required this.baseUrl,
    required this.httpClient,
  });

  factory TrackingRepositoryImpl.fromEnvironment(Environment environment) {
    return TrackingRepositoryImpl(
      baseUrl: '${environment.apiBaseUrl}/tracking',
      httpClient: http.Client(),
    );
  }

  Map<String, String> _getHeaders(String? token) {
    final headers = <String, String>{
      'Content-Type': 'application/json',
    };

    if (token != null) {
      headers['Authorization'] = 'Bearer $token';
    }

    return headers;
  }

  @override
  Future<LocationUpdateResponse> updateLocation(TrackingPoint trackingPoint) async {
    try {
      final response = await httpClient.post(
        Uri.parse('$baseUrl/location'),
        headers: _getHeaders(null),
        body: jsonEncode(trackingPoint.toJson()),
      );

      if (response.statusCode == 200) {
        return LocationUpdateResponse.fromJson(
          jsonDecode(response.body),
        );
      } else {
        throw Exception('Failed to update location: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to update location: $e');
    }
  }

  @override
  Future<bool> startRouteTracking({
    required String routeId,
    required String driverId,
    String? vehicleId,
  }) async {
    try {
      final response = await httpClient.post(
        Uri.parse('$baseUrl/routes/$routeId/start'),
        headers: _getHeaders(null),
        body: jsonEncode({
          'driverId': driverId,
          'vehicleId': vehicleId,
          'startTime': DateTime.now().toIso8601String(),
        }),
      );

      if (response.statusCode == 200) {
        return true;
      } else {
        throw Exception('Failed to start route tracking: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to start route tracking: $e');
    }
  }

  @override
  Future<bool> endRouteTracking({
    required String routeId,
  }) async {
    try {
      final response = await httpClient.post(
        Uri.parse('$baseUrl/routes/$routeId/end'),
        headers: _getHeaders(null),
        body: jsonEncode({
          'endTime': DateTime.now().toIso8601String(),
        }),
      );

      if (response.statusCode == 200) {
        return true;
      } else {
        throw Exception('Failed to end route tracking: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to end route tracking: $e');
    }
  }

  @override
  Future<bool> startDeliveryTracking({
    required String deliveryId,
    required String routeId,
  }) async {
    try {
      final response = await httpClient.post(
        Uri.parse('$baseUrl/deliveries/$deliveryId/start'),
        headers: _getHeaders(null),
        body: jsonEncode({
          'routeId': routeId,
          'startTime': DateTime.now().toIso8601String(),
        }),
      );

      if (response.statusCode == 200) {
        return true;
      } else {
        throw Exception('Failed to start delivery tracking: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to start delivery tracking: $e');
    }
  }

  @override
  Future<bool> endDeliveryTracking({
    required String deliveryId,
    required String status,
    Map<String, dynamic>? metadata,
  }) async {
    try {
      final response = await httpClient.post(
        Uri.parse('$baseUrl/deliveries/$deliveryId/end'),
        headers: _getHeaders(null),
        body: jsonEncode({
          'status': status,
          'endTime': DateTime.now().toIso8601String(),
          'metadata': metadata,
        }),
      );

      if (response.statusCode == 200) {
        return true;
      } else {
        throw Exception('Failed to end delivery tracking: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to end delivery tracking: $e');
    }
  }

  @override
  Future<bool> logTrackingEvent(TrackingEvent event) async {
    try {
      final response = await httpClient.post(
        Uri.parse('$baseUrl/events'),
        headers: _getHeaders(null),
        body: jsonEncode(event.toJson()),
      );

      if (response.statusCode == 200) {
        return true;
      } else {
        throw Exception('Failed to log tracking event: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to log tracking event: $e');
    }
  }

  @override
  Future<TrackingHistoryResponse> getTrackingHistory({
    required String driverId,
    required DateTime startDate,
    required DateTime endDate,
  }) async {
    try {
      final uri = Uri.parse('$baseUrl/history').replace(
        queryParameters: {
          'driverId': driverId,
          'startDate': startDate.toIso8601String(),
          'endDate': endDate.toIso8601String(),
        },
      );

      final response = await httpClient.get(
        uri,
        headers: _getHeaders(null),
      );

      if (response.statusCode == 200) {
        return TrackingHistoryResponse.fromJson(
          jsonDecode(response.body),
        );
      } else {
        throw Exception('Failed to get tracking history: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to get tracking history: $e');
    }
  }

  @override
  Future<RouteTrackingResponse> getRouteTracking({
    required String routeId,
  }) async {
    try {
      final response = await httpClient.get(
        Uri.parse('$baseUrl/routes/$routeId'),
        headers: _getHeaders(null),
      );

      if (response.statusCode == 200) {
        return RouteTrackingResponse.fromJson(
          jsonDecode(response.body),
        );
      } else {
        throw Exception('Failed to get route tracking: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to get route tracking: $e');
    }
  }

  @override
  Future<OrderTrackingResponse> getOrderTracking({
    required String orderId,
  }) async {
    try {
      final response = await httpClient.get(
        Uri.parse('$baseUrl/orders/$orderId'),
        headers: _getHeaders(null),
      );

      if (response.statusCode == 200) {
        return OrderTrackingResponse.fromJson(
          jsonDecode(response.body),
        );
      } else {
        throw Exception('Failed to get order tracking: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to get order tracking: $e');
    }
  }

  @override
  Future<DeliveryTrackingResponse> getDeliveryTracking({
    required String deliveryId,
  }) async {
    try {
      final response = await httpClient.get(
        Uri.parse('$baseUrl/deliveries/$deliveryId'),
        headers: _getHeaders(null),
      );

      if (response.statusCode == 200) {
        return DeliveryTrackingResponse.fromJson(
          jsonDecode(response.body),
        );
      } else {
        throw Exception('Failed to get delivery tracking: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to get delivery tracking: $e');
    }
  }

  @override
  Future<VehicleLocationResponse> getVehicleLocation({
    required String vehicleId,
  }) async {
    try {
      final response = await httpClient.get(
        Uri.parse('$baseUrl/vehicles/$vehicleId/location'),
        headers: _getHeaders(null),
      );

      if (response.statusCode == 200) {
        return VehicleLocationResponse.fromJson(
          jsonDecode(response.body),
        );
      } else {
        throw Exception('Failed to get vehicle location: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to get vehicle location: $e');
    }
  }

  @override
  Future<RoutesResponse> getDriverRoutes({
    required String driverId,
    int page = 1,
    int limit = 10,
    String? status,
    DateTime? startDate,
    DateTime? endDate,
  }) async {
    try {
      final Map<String, String> queryParams = {
        'driverId': driverId,
        'page': page.toString(),
        'limit': limit.toString(),
      };

      if (status != null) {
        queryParams['status'] = status;
      }

      if (startDate != null) {
        queryParams['startDate'] = startDate.toIso8601String();
      }

      if (endDate != null) {
        queryParams['endDate'] = endDate.toIso8601String();
      }

      final uri = Uri.parse('$baseUrl/routes').replace(
        queryParameters: queryParams,
      );

      final response = await httpClient.get(
        uri,
        headers: _getHeaders(null),
      );

      if (response.statusCode == 200) {
        return RoutesResponse.fromJson(
          jsonDecode(response.body),
        );
      } else {
        throw Exception('Failed to get driver routes: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to get driver routes: $e');
    }
  }

  @override
  Future<RouteResponse> getRouteDetails({
    required String routeId,
  }) async {
    try {
      final response = await httpClient.get(
        Uri.parse('$baseUrl/routes/$routeId/details'),
        headers: _getHeaders(null),
      );

      if (response.statusCode == 200) {
        return RouteResponse.fromJson(
          jsonDecode(response.body),
        );
      } else {
        throw Exception('Failed to get route details: ${response.body}');
      }
    } catch (e) {
      throw Exception('Failed to get route details: $e');
    }
  }
}


