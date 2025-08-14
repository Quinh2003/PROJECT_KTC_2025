// Data layer - Repository implementations
// Implement các repository interfaces từ domain layer

import 'package:geolocator/geolocator.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

import '../../domain/repositories/repository_interfaces.dart';
import '../../domain/models/response/auth_response.dart';
import '../../domain/models/tracking_model.dart';
import '../../domain/models/notification_model.dart';
import '../../domain/models/order_model.dart';
import '../services/api_service.dart';
import '../services/socket_service.dart';

class AuthRepositoryImpl implements AuthRepository {
  final ApiService _apiService;
  final FlutterSecureStorage _secureStorage;
  
  AuthRepositoryImpl({
    required ApiService apiService,
    required FlutterSecureStorage secureStorage,
  }) : _apiService = apiService, _secureStorage = secureStorage;
  
  @override
  Future<AuthResponse> login(String email, String password) async {
    final response = await _apiService.login(email, password);
    if (response.token.isNotEmpty) {
      await _secureStorage.write(key: 'auth_token', value: response.token);
    }
    return response;
  }

  @override
  Future<AuthResponse> refreshToken() async {
    final token = await _secureStorage.read(key: 'refresh_token');
    if (token == null) throw Exception('No refresh token found');
    
    return await _apiService.refreshToken(token);
  }

  @override
  Future<void> logout() async {
    await _secureStorage.delete(key: 'auth_token');
    await _secureStorage.delete(key: 'refresh_token');
  }

  @override
  Future<bool> isLoggedIn() async {
    final token = await _secureStorage.read(key: 'auth_token');
    return token != null && token.isNotEmpty;
  }

  @override
  Future<String?> getStoredToken() async {
    return await _secureStorage.read(key: 'auth_token');
  }
}

class TrackingRepositoryImpl implements TrackingRepository {
  final ApiService _apiService;
  final SocketService _socketService;
  
  TrackingRepositoryImpl({
    required ApiService apiService,
    required SocketService socketService,
  }) : _apiService = apiService, _socketService = socketService;

  @override
  Future<void> startLocationTracking() async {
    // Implement start tracking logic
    await _socketService.connect();
  }

  @override
  Future<void> stopLocationTracking() async {
    // Implement stop tracking logic  
    _socketService.disconnect();
  }

  @override
  Future<void> updateLocation(double latitude, double longitude) async {
    // Send location update to backend
    await _apiService.updateDriverLocation(latitude, longitude);
  }

  @override
  Future<List<TrackingPoint>> getTrackingHistory({
    String? driverId,
    String? vehicleId,
    String? startDate,
    String? endDate,
  }) async {
    // Fetch tracking history from API
    return await _apiService.getTrackingHistory(
      driverId: driverId,
      vehicleId: vehicleId,
      startDate: startDate,
      endDate: endDate,
    );
  }

  @override
  Stream<Position> get locationStream => Geolocator.getPositionStream(
    locationSettings: const LocationSettings(
      accuracy: LocationAccuracy.high,
      distanceFilter: 10,
    ),
  );
}

class DeliveryRepositoryImpl implements DeliveryRepository {
  final ApiService _apiService;
  
  DeliveryRepositoryImpl({required ApiService apiService}) : _apiService = apiService;

  @override
  Future<List<OrderModel>> getActiveDeliveries() async {
    return await _apiService.getActiveDeliveries();
  }

  @override
  Future<OrderModel> getDeliveryById(String orderId) async {
    return await _apiService.getDeliveryById(orderId);
  }

  @override
  Future<void> updateDeliveryStatus(String orderId, DeliveryStatus status) async {
    await _apiService.updateDeliveryStatus(orderId, status.toString());
  }

  @override
  Future<void> markAsDelivered(String orderId, String signature, String photo) async {
    await _apiService.markAsDelivered(orderId, signature, photo);
  }
}

class NotificationRepositoryImpl implements NotificationRepository {
  final SocketService _socketService;
  
  NotificationRepositoryImpl({required SocketService socketService}) 
    : _socketService = socketService;

  @override
  Future<void> initializeNotifications() async {
    await _socketService.connect();
  }

  @override
  Stream<NotificationModel> get notificationStream => 
    _socketService.onNotificationReceived();

  @override
  Future<void> markAsRead(String notificationId) async {
    _socketService.markNotificationAsRead(notificationId);
  }
}


