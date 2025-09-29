import 'package:flutter/foundation.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/data/network/http_client.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/delivery.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/delivery_detail_response.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/delivery_status_update.dart';

/// DeliveryServices - Handles all API operations related to deliveries
class DeliveryServices {
  // Dependencies
  final Environment _env = Environment.getInstance();
  final SecureStorageFrave secureStorage = SecureStorageFrave();
  late final HttpClient _httpClient;

  // Constructor
  DeliveryServices() {
    _httpClient = HttpClient(baseUrl: _env.apiBaseUrl, secureStorage: secureStorage);
  }

  /// Get driver ID from secure storage
  Future<int?> _getDriverId() async {
    final driverId = await secureStorage.readDriverId();
    if (driverId == null || driverId == "0" || driverId.isEmpty) {
      debugPrint('⚠️ DriverId not found, attempting recovery...');
      
      // Fallback: Try to use userId if available
      final userId = await secureStorage.readUserId();
      if (userId != null && userId.isNotEmpty && userId != "0") {
        debugPrint('🔄 Using userId as fallback for driverId: $userId');
        try {
          final id = int.parse(userId);
          // Save it as driverId for future use
          await secureStorage.persistentDriverId(userId);
          return id;
        } catch (e) {
          debugPrint('Error parsing userId: $e');
        }
      }
      
      debugPrint('❌ Cannot get driverId - login may be required');
      return null;
    }

    try {
      return int.parse(driverId);
    } catch (e) {
      debugPrint('Error parsing driverId: $e');
      return null;
    }
  }

  /// Get all deliveries assigned to the current driver
  ///
  /// Optional query parameters for filtering and sorting
  /// [status] Filter by delivery status
  /// [sortBy] Field to sort by
  /// [sortDirection] Direction to sort (asc/desc)
  ///
  /// Returns a list of driver deliveries or empty list if failed
  Future<List<Delivery>> getDriverDeliveries({
    String? status,
    String? sortBy,
    String? sortDirection,
  }) async {
    try {
      final driverId = await _getDriverId();
      if (driverId == null) {
        debugPrint('Driver ID not found - cannot get deliveries');
        return [];
      }

      final token = await secureStorage.readToken();
      if (token == null) {
        debugPrint('Token not found - cannot get deliveries');
        return [];
      }

      // Build query string for filters and sorting
      final queryParams = <String, String>{};
      if (status != null) queryParams['status'] = status;
      if (sortBy != null) queryParams['sortBy'] = sortBy;
      if (sortDirection != null) queryParams['sortDirection'] = sortDirection;

      final response = await _httpClient.get<List<dynamic>>(
        '/drivers/$driverId/deliveries',
        queryParams: queryParams,
        useCache: false,
        timeout: const Duration(seconds: 30),
      );

      final deliveries = <Delivery>[];
      for (var i = 0; i < response.length; i++) {
        try {
          final delivery = Delivery.fromJson(response[i]);
          deliveries.add(delivery);
        } catch (e) {
          debugPrint('Error parsing delivery at index $i: $e');
        }
      }

      return deliveries;
    } catch (e) {
      debugPrint('Error getting driver deliveries: $e');
      return [];
    }
  }

  /// Get detailed information about a specific delivery
  ///
  /// [deliveryId] ID of the delivery to get complete details for
  /// Returns detailed delivery response or null if failed
  Future<DeliveryDetailResponse?> getDeliveryDetail(int deliveryId) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      debugPrint('Driver ID not found - cannot get delivery detail');
      return null;
    }

    final token = await secureStorage.readToken();
    if (token == null) {
      debugPrint('Token not found - cannot get delivery detail');
      return null;
    }

    try {
      final response = await _httpClient.get<Map<String, dynamic>>(
        '/drivers/$driverId/deliveries/$deliveryId',
        useCache: false,
        timeout: const Duration(seconds: 30),
      );

      return DeliveryDetailResponse.fromJson(response);
    } catch (e) {
      debugPrint('Error getting delivery detail: $e');
      return null;
    }
  }

  /// Get route information for a specific delivery
  ///
  /// [deliveryId] ID of the delivery to get route for
  /// Returns route data as Map or null if failed
  Future<Map<String, dynamic>?> getDeliveryRoute(int deliveryId) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      debugPrint('Driver ID not found - cannot get delivery route');
      return null;
    }

    final token = await secureStorage.readToken();
    if (token == null) {
      debugPrint('Token not found - cannot get delivery route');
      return null;
    }

    try {
      final response = await _httpClient.get<Map<String, dynamic>>(
        '/drivers/$driverId/deliveries/$deliveryId/route',
        useCache: false,
        timeout: const Duration(seconds: 30),
      );

      return response;
    } catch (e) {
      debugPrint('Error getting delivery route: $e');
      return null;
    }
  }

  /// Update delivery status
  ///
  /// [deliveryId] ID of the delivery to update
  /// [statusUpdate] Status update details
  /// Returns the updated delivery or null if failed
  Future<Delivery?> updateDeliveryStatus(
      int deliveryId, DeliveryStatusUpdate statusUpdate) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      debugPrint('Driver ID not found - cannot update delivery status');
      return null;
    }

    final token = await secureStorage.readToken();
    if (token == null) {
      debugPrint('Token not found - cannot update delivery status');
      return null;
    }

    try {
      final response = await _httpClient.patch<Map<String, dynamic>>(
        '/drivers/$driverId/deliveries/$deliveryId/status',
        body: statusUpdate.toJson(),
        fromJson: (json) => json,
      );

      return Delivery.fromJson(response);
    } catch (e) {
      debugPrint('Error updating delivery status: $e');
      return null;
    }
  }
}

final deliveryServices = DeliveryServices();
