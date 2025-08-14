// Domain layer - Repository interfaces
// Định nghĩa contracts cho các repository

import 'package:geolocator/geolocator.dart';

// Import các models đã có
import '../models/response/auth_response.dart';
import '../models/tracking_model.dart';
import '../models/notification_model.dart';
import '../models/order_model.dart';

abstract class AuthRepository {
  Future<AuthResponse> login(String email, String password);
  Future<AuthResponse> refreshToken();
  Future<void> logout();
  Future<bool> isLoggedIn();
  Future<User?> getCurrentUser();
}

abstract class TrackingRepository {
  Future<void> startTracking();
  Future<void> stopTracking();
  Future<Map<String, dynamic>> getCurrentLocation();
  Stream<Map<String, dynamic>> getLocationStream();
  Future<List<Map<String, dynamic>>> getTrackingHistory({
    String? driverId,
    String? vehicleId,
    DateTime? startDate,
    DateTime? endDate,
  });
}

enum DeliveryStatus { pending, inProgress, delivered, failed }

abstract class DeliveryRepository {
  Future<List<Map<String, dynamic>>> getActiveDeliveries();
  Future<Map<String, dynamic>> getDeliveryById(String deliveryId);
  Future<void> confirmPickup(String deliveryId);
  Future<void> confirmDelivery(String deliveryId, Map<String, dynamic> deliveryData);
  Future<void> reportProblem(String deliveryId, String problemType, String description);
  Future<List<Map<String, dynamic>>> getDeliveryHistory({int limit = 20, int offset = 0});
}

abstract class NotificationRepository {
  Future<void> sendNotification(String title, String message, {Map<String, dynamic>? data});
  Future<void> scheduleNotification(String title, String message, DateTime scheduledTime);
  Future<void> cancelNotification(String notificationId);
  Stream<Map<String, dynamic>> getNotificationStream();
}


