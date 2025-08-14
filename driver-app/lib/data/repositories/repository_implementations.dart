import 'package:ktc_logistics_driver/domain/repositories/repository_interfaces.dart';
import 'package:ktc_logistics_driver/domain/models/response/auth_response.dart';
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
      
      // Convert to domain User
      final domainUser = User(
        uid: user['id'],
        name: user['name'],
        email: user['email'],
        phone: user['phone'] ?? '',
        image: user['avatar'] ?? '',
        role: 'DRIVER',
        isActive: true,
        permissions: ['DRIVE', 'TRACK'],
      );
      
      return AuthResponse(
        success: true,
        user: domainUser,
        token: 'mock_token_${user['id']}',
      );
    } catch (e) {
      return AuthResponse(
        success: false,
        user: User(
          uid: '',
          name: '',
          email: '',
          phone: '',
          image: '',
          role: '',
          isActive: false,
          permissions: [],
        ),
        token: '',
      );
    }
  }

  @override
  Future<AuthResponse> refreshToken() async {
    final currentUser = await getCurrentUser();
    return AuthResponse(
      success: true,
      user: currentUser ?? User(
        uid: '',
        name: '',
        email: '',
        phone: '',
        image: '',
        role: '',
        isActive: false,
        permissions: [],
      ),
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
    if (user != null) {
      return User(
        uid: user['id'] ?? '',
        name: user['name'] ?? '',
        email: user['email'] ?? '',
        phone: user['phone'] ?? '',
        image: user['avatar'] ?? '',
        role: 'DRIVER',
        isActive: true,
        permissions: ['DRIVE', 'TRACK'],
      );
    }
    return null;
  }
}

/// TrackingRepository Implementation using Mock Service
class TrackingRepositoryImpl implements TrackingRepository {
  final MockDataService _mockDataService;

  TrackingRepositoryImpl({
    required MockDataService mockDataService,
  }) : _mockDataService = mockDataService;

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

  @override
  Future<List<Map<String, dynamic>>> getTrackingHistory({
    String? driverId,
    String? vehicleId,
    DateTime? startDate,
    DateTime? endDate,
  }) async {
    // Mock implementation
    await Future.delayed(const Duration(seconds: 1));
    return [
      {
        'id': '1',
        'latitude': 21.028511,
        'longitude': 105.804817,
        'timestamp': DateTime.now().subtract(const Duration(hours: 1)).millisecondsSinceEpoch,
        'speed': 30.0,
      },
      {
        'id': '2',
        'latitude': 21.030000,
        'longitude': 105.806000,
        'timestamp': DateTime.now().subtract(const Duration(minutes: 30)).millisecondsSinceEpoch,
        'speed': 25.0,
      },
    ];
  }
}

/// DeliveryRepository Implementation using Mock Service  
class DeliveryRepositoryImpl implements DeliveryRepository {
  final MockDataService _mockDataService;

  DeliveryRepositoryImpl({
    required MockDataService mockDataService,
  }) : _mockDataService = mockDataService;

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

  NotificationRepositoryImpl({
    required NotificationService notificationService,
  }) : _notificationService = notificationService;

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