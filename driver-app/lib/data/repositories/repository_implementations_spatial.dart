import 'package:ktc_logistics_driver/domain/repositories/repository_interfaces.dart';
import 'package:ktc_logistics_driver/domain/models/response/auth_response.dart';
import 'package:ktc_logistics_driver/data/services/api_service.dart';
import 'package:ktc_logistics_driver/data/services/socket_service.dart';
import 'package:ktc_logistics_driver/data/services/mock_auth_service.dart';
import 'package:ktc_logistics_driver/data/services/mock_data_service.dart';
import 'package:ktc_logistics_driver/data/services/notification_service.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

/// AuthRepository Implementation using Mock Service
class AuthRepositoryImpl implements AuthRepository {
  final MockAuthService _mockAuthService;
  final FlutterSecureStorage _secureStorage;

  AuthRepositoryImpl({
    required MockAuthService mockAuthService,
    required FlutterSecureStorage secureStorage,
  }) : _mockAuthService = mockAuthService,
       _secureStorage = secureStorage;

  @override
  Future<AuthResponse> login(String email, String password) async {
    try {
      final user = await _mockAuthService.login(email, password);
      
      // Store user info securely
      await _secureStorage.write(key: 'user_token', value: 'mock_token_${user['id']}');
      await _secureStorage.write(key: 'user_id', value: user['id']);
      
      return AuthResponse(
        success: true,
        message: 'Đăng nhập thành công',
        user: User.fromJson(user),
        token: 'mock_token_${user['id']}',
      );
    } catch (e) {
      return AuthResponse(
        success: false,
        message: e.toString(),
        user: null,
        token: '',
      );
    }
  }

  @override
  Future<AuthResponse> refreshToken() async {
    return AuthResponse(
      success: true,
      message: 'Token refreshed',
      user: await getCurrentUser(),
      token: 'mock_refreshed_token',
    );
  }

  @override
  Future<void> logout() async {
    await _mockAuthService.logout();
    await _secureStorage.deleteAll();
  }

  @override
  Future<bool> isLoggedIn() async {
    final token = await _secureStorage.read(key: 'user_token');
    return token != null && _mockAuthService.isLoggedIn;
  }

  @override
  Future<User?> getCurrentUser() async {
    final user = _mockAuthService.currentUser;
    return user != null ? User.fromJson(user) : null;
  }
}

/// TrackingRepository Implementation using Mock Service
class TrackingRepositoryImpl implements TrackingRepository {
  final ApiService _apiService;
  final SocketService _socketService;
  final MockDataService _mockDataService;

  TrackingRepositoryImpl({
    required ApiService apiService,
    required SocketService socketService,
    required MockDataService mockDataService,
  }) : _apiService = apiService,
       _socketService = socketService,
       _mockDataService = mockDataService;

  @override
  Future<void> startTracking() async {
    // Mock implementation
    await Future.delayed(const Duration(seconds: 1));
    // In real implementation, this would start GPS tracking
  }

  @override
  Future<void> stopTracking() async {
    // Mock implementation
    await Future.delayed(const Duration(seconds: 1));
  }

  @override
  Future<Map<String, dynamic>> getCurrentLocation() async {
    return await _mockDataService.getDriverLocation();
  }

  @override
  Stream<Map<String, dynamic>> getLocationStream() async* {
    // Mock stream that emits location every 10 seconds
    while (true) {
      await Future.delayed(const Duration(seconds: 10));
      yield await _mockDataService.getDriverLocation();
    }
  }
}

/// DeliveryRepository Implementation using Mock Service  
class DeliveryRepositoryImpl implements DeliveryRepository {
  final ApiService _apiService;
  final MockDataService _mockDataService;

  DeliveryRepositoryImpl({
    required ApiService apiService,
    required MockDataService mockDataService,
  }) : _apiService = apiService,
       _mockDataService = mockDataService;

  @override
  Future<List<Map<String, dynamic>>> getActiveDeliveries() async {
    return await _mockDataService.getActiveOrders();
  }

  @override
  Future<Map<String, dynamic>> getDeliveryById(String deliveryId) async {
    return await _mockDataService.getOrderById(deliveryId);
  }

  @override
  Future<void> confirmPickup(String deliveryId) async {
    await _mockDataService.confirmPickup(deliveryId);
  }

  @override
  Future<void> confirmDelivery(String deliveryId, Map<String, dynamic> deliveryData) async {
    await _mockDataService.confirmDelivery(
      deliveryId,
      signature: deliveryData['signature'],
      photo: deliveryData['photo'],
      notes: deliveryData['notes'],
    );
  }

  @override
  Future<void> reportProblem(String deliveryId, String problemType, String description) async {
    await _mockDataService.reportProblem(
      deliveryId,
      problemType: problemType,
      description: description,
    );
  }

  @override
  Future<List<Map<String, dynamic>>> getDeliveryHistory({int limit = 20, int offset = 0}) async {
    return await _mockDataService.getDeliveryHistory(limit: limit, offset: offset);
  }
}

/// NotificationRepository Implementation using Local Notifications
class NotificationRepositoryImpl implements NotificationRepository {
  final NotificationService _notificationService;
  final MockDataService _mockDataService;

  NotificationRepositoryImpl({
    required NotificationService notificationService,
    required MockDataService mockDataService,
  }) : _notificationService = notificationService,
       _mockDataService = mockDataService;

  @override
  Future<void> sendNotification(String title, String message, {Map<String, dynamic>? data}) async {
    await _notificationService.showNotification(
      id: DateTime.now().millisecondsSinceEpoch ~/ 1000,
      title: title,
      body: message,
      payload: data?.toString(),
    );
  }

  @override
  Future<void> scheduleNotification(String title, String message, DateTime scheduledTime) async {
    // For now, just send immediately (local notifications scheduling can be added later)
    await sendNotification(title, message);
  }

  @override
  Future<void> cancelNotification(String notificationId) async {
    await _notificationService.cancelNotification(int.parse(notificationId));
  }

  @override
  Stream<Map<String, dynamic>> getNotificationStream() async* {
    // Mock stream for notifications (in real app this would come from backend)
    while (true) {
      await Future.delayed(const Duration(minutes: 5));
      yield {
        'title': 'Cập nhật hệ thống',
        'message': 'Có thông báo mới từ hệ thống KTC Logistics',
        'timestamp': DateTime.now().toIso8601String(),
      };
    }
  }
}

// User model for authentication
class User {
  final String id;
  final String email;
  final String name;
  final String phone;
  final String? avatar;
  final Map<String, dynamic>? vehicle;
  final double rating;
  final int totalDeliveries;

  User({
    required this.id,
    required this.email,
    required this.name,
    required this.phone,
    this.avatar,
    this.vehicle,
    required this.rating,
    required this.totalDeliveries,
  });

  factory User.fromJson(Map<String, dynamic> json) {
    return User(
      id: json['id'] ?? '',
      email: json['email'] ?? '',
      name: json['name'] ?? '',
      phone: json['phone'] ?? '',
      avatar: json['avatar'],
      vehicle: json['vehicle'],
      rating: (json['rating'] ?? 0.0).toDouble(),
      totalDeliveries: json['totalDeliveries'] ?? 0,
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'email': email,
      'name': name,
      'phone': phone,
      'avatar': avatar,
      'vehicle': vehicle,
      'rating': rating,
      'totalDeliveries': totalDeliveries,
    };
  }
}


