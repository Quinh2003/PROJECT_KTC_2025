// Domain layer - Repository interfaces
// Định nghĩa contracts cho các repository

// Import các models đã có
import '../models/auth/auth_response.dart';

abstract class AuthRepository {
  Future<AuthResponse> login(String email, String password);
  Future<AuthResponse> refreshToken();
  Future<void> logout();
  Future<bool> isLoggedIn();
  Future<User?> getCurrentUser();
}

// Note: TrackingRepository is defined in tracking_repository.dart due to its complexity

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


