import 'package:flutter/foundation.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/data/network/http_client.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_details_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_status_update.dart';
import 'package:ktc_logistics_driver/services/auth_services.dart';

/// OrdersServices - Handles all API operations related to orders
class OrdersServices {
  // Dependencies
  final Environment _env = Environment.getInstance();
  final SecureStorageFrave secureStorage = SecureStorageFrave();
  late final HttpClient _httpClient;
  final AuthServices _authServices = AuthServices();

  // Constructor
  OrdersServices() {
    _httpClient = HttpClient(baseUrl: _env.apiBaseUrl, secureStorage: secureStorage);
  }

  /// Get driver ID from secure storage
  Future<int?> _getDriverId() async {
    final driverId = await secureStorage.readDriverId();
    if (driverId == null || driverId.isEmpty) {
      // Fallback: Try to use userId if available
      final userId = await secureStorage.readUserId();
      if (userId != null && userId.isNotEmpty) {
        try {
          final id = int.parse(userId);
          // Save it as driverId for future use
          await secureStorage.persistentDriverId(userId);
          return id;
        } catch (e) {
          debugPrint('Error parsing userId: $e');
          return null;
        }
      }
      return null;
    }
    
    try {
      return int.parse(driverId);
    } catch (e) {
      debugPrint('Error parsing driverId: $e');
      return null;
    }
  }

  /// Retrieves all orders assigned to the current driver
  ///
  /// Returns a list of [OrdersResponse] objects
  Future<List<OrdersResponse>> getDriverOrdersList() async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      debugPrint('Driver ID not found - cannot get orders');
      return [];
    }

    final token = await secureStorage.readToken();
    if (token == null) {
      debugPrint('Token not found - cannot get orders');
      return [];
    }

    try {
      print('📦 OrdersService: Getting orders via HttpClient...');

      final response = await _httpClient.get<List<dynamic>>(
        '/drivers/$driverId/orders',
        useCache: false,
        timeout: const Duration(seconds: 45),
      );

      print('📦 OrdersService: Response received: ${response.length} items');
      return response.map((item) => OrdersResponse.fromJson(item)).toList();
    } catch (e) {
      debugPrint('Error getting driver orders: $e');
      return [];
    }
  }

  /// Get detailed information about a specific order
  ///
  /// [orderId] The ID of the order to retrieve
  /// Returns [OrderDetailsResponse] object with order details
  Future<OrderDetailsResponse?> getDriverOrderDetail(int orderId) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      debugPrint('Driver ID not found - cannot get orders');
      return null;
    }

    final token = await secureStorage.readToken();
    if (token == null) {
      debugPrint('Token not found - cannot get order details');
      return null;
    }

    try {
      print('📦 OrdersService: Getting order details via HttpClient...');

      final response = await _httpClient.get<Map<String, dynamic>>(
        '/drivers/$driverId/orders/$orderId',
        useCache: false,
        timeout: const Duration(seconds: 45),
      );

      return OrderDetailsResponse.fromJson(response);
    } catch (e) {
      debugPrint('Error getting order detail: $e');
      return null;
    }
  }
  
  /// Update order status using OrderStatusUpdate model
  /// 
  /// [orderId] ID of the order to update
  /// [statusUpdate] Status update details
  /// [driverId] Optional driver ID (will use stored driver ID if not provided)
  /// Returns true if successful, false otherwise
  Future<bool> updateOrderStatus({
    int? driverId,
    required int orderId,
    required OrderStatusUpdate statusUpdate
  }) async {
    // Use provided driverId or get from secure storage
    final dId = driverId ?? await _getDriverId();
    if (dId == null) {
      debugPrint('Driver ID not found - cannot update order status');
      return false;
    }

    final token = await secureStorage.readToken();
    if (token == null) {
      debugPrint('Token not found - cannot update order status');
      return false;
    }

    try {
      // Thay đổi kiểu trả về thành bool thay vì Map<String, dynamic>
      await _httpClient.patch<bool>(
        '/drivers/$dId/orders/$orderId/status',
        body: statusUpdate.toJson(),
      );

      return true;
    } catch (e) {
      // Log chi tiết lỗi để debug
      if (e is ApiException) {
        debugPrint('API Error updating order status: Code=${e.statusCode}, Message=${e.message}');
        debugPrint('Response body: ${e.body}');
        
        // Nếu là lỗi 401, có thể token đã hết hạn
        if (e.statusCode == 401) {
          debugPrint('Unauthorized error - trying to refresh token');
          
          try {
            // Thử làm mới token
            final loginResponse = await _authServices.refreshToken();
            
            if (loginResponse.accessToken.isNotEmpty) {
              debugPrint('Token refreshed successfully, trying update again');
              
              // Thử lại cập nhật trạng thái đơn hàng
              await _httpClient.patch<bool>(
                '/drivers/$dId/orders/$orderId/status',
                body: statusUpdate.toJson(),
              );
              
              return true;
            }
          } catch (refreshError) {
            debugPrint('Failed to refresh token: $refreshError');
          }
          return false;
        }
      } else {
        debugPrint('Error updating order status: ${e.toString()}');
      }
      return false;
    }
  }
}

/// Singleton instance for app-wide use
final ordersServices = OrdersServices();
