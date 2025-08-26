import '../../domain/repositories/repository_interfaces.dart';
import '../../domain/repositories/tracking_repository_interfaces.dart';
import '../../domain/models/response/auth_response.dart';
import '../../domain/models/tracking_model.dart';
import '../../services/mock_auth_service.dart';
import '../../services/mock_data_service.dart';
import '../../services/notification_service.dart';
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
  TrackingRepositoryImpl({
    required MockDataService mockDataService,
  });

  @override
  Future<LocationUpdateResponse> updateLocation(TrackingPoint trackingPoint) async {
    await Future.delayed(const Duration(milliseconds: 500));
    return LocationUpdateResponse(
      success: true,
      message: 'Location updated successfully',
      trackingPoint: trackingPoint,
    );
  }

  @override
  Future<bool> startRouteTracking({
    required String routeId,
    required String driverId,
    String? vehicleId,
  }) async {
    await Future.delayed(const Duration(seconds: 1));
    return true;
  }

  @override
  Future<bool> endRouteTracking({required String routeId}) async {
    await Future.delayed(const Duration(seconds: 1));
    return true;
  }

  @override
  Future<bool> startDeliveryTracking({
    required String deliveryId,
    required String routeId,
  }) async {
    await Future.delayed(const Duration(seconds: 1));
    return true;
  }

  @override
  Future<bool> endDeliveryTracking({
    required String deliveryId,
    required String status,
    Map<String, dynamic>? metadata,
  }) async {
    await Future.delayed(const Duration(seconds: 1));
    return true;
  }

  @override
  Future<bool> logTrackingEvent(TrackingEvent event) async {
    await Future.delayed(const Duration(milliseconds: 500));
    return true;
  }

  @override
  Future<TrackingHistoryResponse> getTrackingHistory({
    required String driverId,
    required DateTime startDate,
    required DateTime endDate,
  }) async {
    await Future.delayed(const Duration(seconds: 1));
    return TrackingHistoryResponse(
      trackingPoints: [],
      totalPoints: 0,
      timespan: '${startDate.toIso8601String()} to ${endDate.toIso8601String()}',
    );
  }

  @override
  Future<RouteTrackingResponse> getRouteTracking({required String routeId}) async {
    await Future.delayed(const Duration(seconds: 1));
    return RouteTrackingResponse(
      routeId: routeId,
      status: 'active',
      startTime: DateTime.now(),
      trackingPoints: [],
      totalDistance: 0.0,
      totalDuration: Duration.zero,
      deliveryIds: [],
    );
  }

  @override
  Future<OrderTrackingResponse> getOrderTracking({required String orderId}) async {
    await Future.delayed(const Duration(seconds: 1));
    return OrderTrackingResponse(
      orderId: orderId,
      status: 'in_progress',
      createdAt: DateTime.now(),
      trackingPoints: [],
      events: [],
    );
  }

  @override
  Future<DeliveryTrackingResponse> getDeliveryTracking({required String deliveryId}) async {
    await Future.delayed(const Duration(seconds: 1));
    return DeliveryTrackingResponse(
      deliveryId: deliveryId,
      status: 'in_progress',
      startTime: DateTime.now(),
      trackingPoints: [],
      events: [],
    );
  }

  @override
  Future<VehicleLocationResponse> getVehicleLocation({required String vehicleId}) async {
    await Future.delayed(const Duration(seconds: 1));
    return VehicleLocationResponse(
      vehicleId: vehicleId,
      driverId: 'driver-123',
      driverName: 'John Doe',
      latitude: 21.028511,
      longitude: 105.804817,
      timestamp: DateTime.now(),
    );
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
    await Future.delayed(const Duration(seconds: 1));
    return RoutesResponse(
      routes: [],
      totalRoutes: 0,
      currentPage: page,
      totalPages: 0,
    );
  }

  @override
  Future<RouteResponse> getRouteDetails({required String routeId}) async {
    await Future.delayed(const Duration(seconds: 1));
    return RouteResponse(
      route: RouteDetail(
        routeId: routeId,
        name: 'Sample Route',
        status: 'active',
        startTime: DateTime.now(),
        deliveryIds: [],
        totalDistance: 0.0,
        estimatedDuration: Duration.zero,
        waypoints: [],
      ),
    );
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