// api_service.dart
// Service class để giao tiếp với Spring Boot backend

import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import '../data/env/environment.dart';
import '../domain/models/auth/auth_response.dart';
import '../domain/models/order/order_response.dart' as order_models;
import '../domain/models/delivery/delivery_response.dart';
import '../domain/models/order/orders_by_status_response.dart' as status_models;

class ApiService {
  // Instance của FlutterSecureStorage để lưu JWT token
  final _storage = const FlutterSecureStorage();
  
  // HTTP Client với timeout
  final _client = http.Client();
  
  // Environment instance
  final _env = Environment.getInstance();
  
  // Singleton pattern cho ApiService
  static final ApiService _instance = ApiService._internal();
  factory ApiService() => _instance;
  ApiService._internal();

  // ======== Authentication API ========

  // Đăng nhập tài xế với Spring Boot backend
  Future<AuthResponse> login(String email, String password) async {
    try {
      final response = await _client.post(
        Uri.parse(_env.loginUrl),
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          'email': email,
          'password': password,
        }),
      );

      // Kiểm tra status code
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        
        // Parse JSON response thành AuthResponse model
        final authResponse = AuthResponse.fromJson(decodedData);
        
        // Lưu JWT token an toàn
        await _storage.write(key: 'token', value: authResponse.token);
        await _storage.write(key: 'uid', value: authResponse.user.uid);
        
        return authResponse;
      } else {
        // Xử lý lỗi từ server
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Login failed');
      }
    } catch (e) {
      // Xử lý lỗi network hoặc exception khác
      throw Exception('Login error: $e');
    }
  }

  // Kiểm tra token đã lưu có còn hợp lệ không
  Future<bool> checkTokenValidity() async {
    try {
      // Lấy token từ secure storage
      final token = await _storage.read(key: 'token');
      
      if (token == null) return false;
      
      // Kiểm tra token bằng cách gọi API protected
      final response = await _client.get(
        Uri.parse(_env.userProfileUrl),
        headers: await _getAuthHeaders(),
      );
      
      return response.statusCode == 200;
    } catch (e) {
      return false;
    }
  }

  // Đăng xuất - xóa token từ secure storage và gọi API logout
  Future<void> logout() async {
    try {
      // Gọi API logout
      await _client.post(
        Uri.parse(_env.logoutUrl),
        headers: await _getAuthHeaders(),
      );
    } catch (e) {
      // Bỏ qua lỗi khi logout API
    } finally {
      // Luôn xóa token locally
      await _storage.delete(key: 'token');
      await _storage.delete(key: 'uid');
    }
  }

  // ======== Order API ========

  // Lấy đơn hàng theo trạng thái
  Future<status_models.OrdersByStatusResponse> getOrdersByStatus(String status) async {
    try {
      final uid = await _storage.read(key: 'uid');
      
      final response = await _client.get(
        Uri.parse('${_env.ordersUrl}?status=$status&driverId=$uid'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return status_models.OrdersByStatusResponse.fromJson(decodedData);
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to load orders');
      }
    } catch (e) {
      throw Exception('Get orders error: $e');
    }
  }

  // Lấy tất cả đơn hàng của tài xế
  Future<order_models.OrdersResponse> getAllOrders({int page = 1, int perPage = 10}) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.ordersUrl}?page=$page&perPage=$perPage'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return order_models.OrdersResponse.fromJson(decodedData);
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to load orders');
      }
    } catch (e) {
      throw Exception('Get orders error: $e');
    }
  }

  // Lấy chi tiết một đơn hàng
  Future<Map<String, dynamic>> getOrderDetail(String orderId) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.ordersUrl}/$orderId'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to load order details');
      }
    } catch (e) {
      throw Exception('Get order details error: $e');
    }
  }

  // Cập nhật trạng thái đơn hàng
  Future<Map<String, dynamic>> updateOrderStatus(String orderId, String status) async {
    try {
      final response = await _client.patch(
        Uri.parse('${_env.orderStatusUrl}/$orderId/status'),
        headers: await _getAuthHeaders(),
        body: jsonEncode({
          'status': status,
        }),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to update order status');
      }
    } catch (e) {
      throw Exception('Update order status error: $e');
    }
  }

  // Lấy thông tin tracking của đơn hàng
  Future<Map<String, dynamic>> getOrderTracking(String orderId) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.orderTrackingUrl}/$orderId/tracking'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to get order tracking');
      }
    } catch (e) {
      throw Exception('Get order tracking error: $e');
    }
  }

  // ======== Delivery API ========

  // Lấy tất cả deliveries của tài xế
  Future<Map<String, dynamic>> getAllDeliveries({int page = 1, int perPage = 10}) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.deliveriesUrl}?page=$page&perPage=$perPage'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to load deliveries');
      }
    } catch (e) {
      throw Exception('Get deliveries error: $e');
    }
  }

  // Lấy chi tiết một delivery
  Future<DeliveryResponse> getDeliveryDetail(String deliveryId) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.deliveriesUrl}/$deliveryId'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return DeliveryResponse.fromJson(decodedData);
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to load delivery details');
      }
    } catch (e) {
      throw Exception('Get delivery detail error: $e');
    }
  }

  // Lấy thông tin tracking của delivery
  Future<Map<String, dynamic>> getDeliveryTracking(String deliveryId) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.deliveryTrackingUrl}/$deliveryId/tracking'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to get delivery tracking');
      }
    } catch (e) {
      throw Exception('Get delivery tracking error: $e');
    }
  }

  // ======== Route API ========

  // Lấy tất cả routes
  Future<Map<String, dynamic>> getAllRoutes({int page = 1, int perPage = 10}) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.routesUrl}?page=$page&perPage=$perPage'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to load routes');
      }
    } catch (e) {
      throw Exception('Get routes error: $e');
    }
  }

  // Lấy chi tiết một route
  Future<Map<String, dynamic>> getRouteDetail(String routeId) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.routesUrl}/$routeId'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to load route details');
      }
    } catch (e) {
      throw Exception('Get route detail error: $e');
    }
  }

  // Lấy tracking của route
  Future<Map<String, dynamic>> getRouteTracking(String routeId) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.routeTrackingUrl}/$routeId/tracking'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to get route tracking');
      }
    } catch (e) {
      throw Exception('Get route tracking error: $e');
    }
  }

  // ======== Tracking API ========

  // Cập nhật vị trí GPS
  Future<Map<String, dynamic>> updateDriverLocation(double latitude, double longitude) async {
    try {
      final response = await _client.post(
        Uri.parse(_env.updateLocationUrl),
        headers: await _getAuthHeaders(),
        body: jsonEncode({
          'latitude': latitude,
          'longitude': longitude,
          'timestamp': DateTime.now().toIso8601String(),
        }),
      );
      
      if (response.statusCode == 200 || response.statusCode == 201) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to update location');
      }
    } catch (e) {
      throw Exception('Update location error: $e');
    }
  }

  // Lấy vị trí thời gian thực của xe
  Future<Map<String, dynamic>> getVehicleLocation(String vehicleId) async {
    try {
      final response = await _client.get(
        Uri.parse('${_env.vehicleTrackingUrl}/$vehicleId'),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to get vehicle location');
      }
    } catch (e) {
      throw Exception('Get vehicle location error: $e');
    }
  }

  // Lấy lịch sử tracking
  Future<Map<String, dynamic>> getTrackingHistory({String? vehicleId, String? driverId, String? startDate, String? endDate}) async {
    try {
      // Build query parameters
      final queryParams = <String, String>{};
      if (vehicleId != null) queryParams['vehicleId'] = vehicleId;
      if (driverId != null) queryParams['driverId'] = driverId;
      if (startDate != null) queryParams['startDate'] = startDate;
      if (endDate != null) queryParams['endDate'] = endDate;
      
      final uri = Uri.parse(_env.trackingHistoryUrl).replace(queryParameters: queryParams);
      
      final response = await _client.get(
        uri,
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to get tracking history');
      }
    } catch (e) {
      throw Exception('Get tracking history error: $e');
    }
  }

  // ======== User Profile API ========

  // Lấy thông tin profile của tài xế
  Future<AuthResponse> getDriverProfile() async {
    try {
      final response = await _client.get(
        Uri.parse(_env.userProfileUrl),
        headers: await _getAuthHeaders(),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return AuthResponse.fromJson(decodedData);
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to get driver profile');
      }
    } catch (e) {
      throw Exception('Get driver profile error: $e');
    }
  }

  // Cập nhật thông tin profile của tài xế
  Future<AuthResponse> updateDriverProfile(Map<String, dynamic> profileData) async {
    try {
      final response = await _client.put(
        Uri.parse(_env.updateUserProfileUrl),
        headers: await _getAuthHeaders(),
        body: jsonEncode(profileData),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return AuthResponse.fromJson(decodedData);
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to update driver profile');
      }
    } catch (e) {
      throw Exception('Update driver profile error: $e');
    }
  }

  // Cập nhật trạng thái tài xế (online, offline, busy, etc.)
  Future<Map<String, dynamic>> updateDriverStatus(String status) async {
    try {
      final response = await _client.post(
        Uri.parse('${_env.userProfileUrl}/status'),
        headers: await _getAuthHeaders(),
        body: jsonEncode({
          'status': status,
        }),
      );
      
      if (response.statusCode == 200) {
        final decodedData = jsonDecode(response.body);
        return decodedData;
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['message'] ?? 'Failed to update driver status');
      }
    } catch (e) {
      throw Exception('Update driver status error: $e');
    }
  }

  // ======== Helper Methods ========

  // Tạo Authorization header với JWT token
  Future<Map<String, String>> _getAuthHeaders() async {
    final token = await _storage.read(key: 'token');
    return {
      'Content-Type': 'application/json',
      'Authorization': 'Bearer $token',
    };
  }
}


