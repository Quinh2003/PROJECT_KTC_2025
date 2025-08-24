// Domain layer - Use Cases
// Business logic use cases theo Clean Architecture

import '../repositories/repository_interfaces.dart';
import '../repositories/tracking_repository_interfaces.dart';
import '../models/auth/auth_response.dart';
import '../models/delivery/tracking_model.dart';

abstract class UseCase<Type, Params> {
  Future<Type> call(Params params);
}

abstract class StreamUseCase<Type, Params> {
  Stream<Type> call(Params params);
}

class NoParams {}

// ======== Authentication Use Cases ========

class LoginUseCase implements UseCase<AuthResponse, LoginParams> {
  final AuthRepository repository;
  
  LoginUseCase({required this.repository});
  
  @override
  Future<AuthResponse> call(LoginParams params) {
    return repository.login(params.email, params.password);
  }
}

class LogoutUseCase implements UseCase<void, NoParams> {
  final AuthRepository repository;
  
  LogoutUseCase({required this.repository});
  
  @override
  Future<void> call(NoParams params) async {
    return await repository.logout();
  }
}

class CheckLoginStatusUseCase implements UseCase<bool, NoParams> {
  final AuthRepository repository;
  
  CheckLoginStatusUseCase({required this.repository});
  
  @override
  Future<bool> call(NoParams params) async {
    return await repository.isLoggedIn();
  }
}

class GetCurrentUserUseCase implements UseCase<User?, NoParams> {
  final AuthRepository repository;
  
  GetCurrentUserUseCase({required this.repository});
  
  @override
  Future<User?> call(NoParams params) async {
    return await repository.getCurrentUser();
  }
}

// ======== Tracking Use Cases ========

class StartRouteTrackingUseCase implements UseCase<bool, StartRouteTrackingParams> {
  final TrackingRepository repository;
  
  StartRouteTrackingUseCase({required this.repository});
  
  @override
  Future<bool> call(StartRouteTrackingParams params) async {
    return await repository.startRouteTracking(
      routeId: params.routeId,
      driverId: params.driverId,
      vehicleId: params.vehicleId,
    );
  }
}

class EndRouteTrackingUseCase implements UseCase<bool, EndRouteTrackingParams> {
  final TrackingRepository repository;
  
  EndRouteTrackingUseCase({required this.repository});
  
  @override
  Future<bool> call(EndRouteTrackingParams params) async {
    return await repository.endRouteTracking(routeId: params.routeId);
  }
}

class UpdateLocationUseCase implements UseCase<LocationUpdateResponse, TrackingPoint> {
  final TrackingRepository repository;
  
  UpdateLocationUseCase({required this.repository});
  
  @override
  Future<LocationUpdateResponse> call(TrackingPoint params) {
    return repository.updateLocation(params);
  }
}

class GetTrackingHistoryUseCase implements UseCase<TrackingHistoryResponse, TrackingHistoryParams> {
  final TrackingRepository repository;
  
  GetTrackingHistoryUseCase({required this.repository});
  
  @override
  Future<TrackingHistoryResponse> call(TrackingHistoryParams params) {
    return repository.getTrackingHistory(
      driverId: params.driverId!,
      startDate: DateTime.parse(params.startDate!),
      endDate: DateTime.parse(params.endDate!),
    );
  }
}

// ======== Delivery Use Cases ========

class GetActiveDeliveriesUseCase implements UseCase<List<Map<String, dynamic>>, NoParams> {
  final DeliveryRepository repository;
  
  GetActiveDeliveriesUseCase({required this.repository});
  
  @override
  Future<List<Map<String, dynamic>>> call(NoParams params) async {
    return await repository.getActiveDeliveries();
  }
}

class GetDeliveryByIdUseCase implements UseCase<Map<String, dynamic>, DeliveryDetailsParams> {
  final DeliveryRepository repository;
  
  GetDeliveryByIdUseCase({required this.repository});
  
  @override
  Future<Map<String, dynamic>> call(DeliveryDetailsParams params) async {
    return await repository.getDeliveryById(params.deliveryId);
  }
}

class ConfirmPickupUseCase implements UseCase<void, ConfirmPickupParams> {
  final DeliveryRepository repository;
  
  ConfirmPickupUseCase({required this.repository});
  
  @override
  Future<void> call(ConfirmPickupParams params) async {
    return await repository.confirmPickup(params.deliveryId);
  }
}

class ConfirmDeliveryUseCase implements UseCase<void, ConfirmDeliveryParams> {
  final DeliveryRepository repository;
  
  ConfirmDeliveryUseCase({required this.repository});
  
  @override
  Future<void> call(ConfirmDeliveryParams params) async {
    return await repository.confirmDelivery(params.deliveryId, params.deliveryData);
  }
}

class ReportProblemUseCase implements UseCase<void, ReportProblemParams> {
  final DeliveryRepository repository;
  
  ReportProblemUseCase({required this.repository});
  
  @override
  Future<void> call(ReportProblemParams params) async {
    return await repository.reportProblem(params.deliveryId, params.problemType, params.description);
  }
}

class GetDeliveryHistoryUseCase implements UseCase<List<Map<String, dynamic>>, DeliveryHistoryParams> {
  final DeliveryRepository repository;
  
  GetDeliveryHistoryUseCase({required this.repository});
  
  @override
  Future<List<Map<String, dynamic>>> call(DeliveryHistoryParams params) async {
    return await repository.getDeliveryHistory(limit: params.limit, offset: params.offset);
  }
}

// ======== Notification Use Cases ========

class SendNotificationUseCase implements UseCase<void, SendNotificationParams> {
  final NotificationRepository repository;
  
  SendNotificationUseCase({required this.repository});
  
  @override
  Future<void> call(SendNotificationParams params) async {
    return await repository.sendNotification(params.title, params.message, data: params.data);
  }
}

// ======== Parameters Classes ========

class LoginParams {
  final String email;
  final String password;
  
  LoginParams({required this.email, required this.password});
}

class TrackingHistoryParams {
  final String? driverId;
  final String? vehicleId;
  final String? startDate;
  final String? endDate;
  
  TrackingHistoryParams({
    this.driverId,
    this.vehicleId,
    this.startDate,
    this.endDate,
  });
}

class DeliveryDetailsParams {
  final String deliveryId;
  
  DeliveryDetailsParams({required this.deliveryId});
}

class ConfirmPickupParams {
  final String deliveryId;
  
  ConfirmPickupParams({required this.deliveryId});
}

class ConfirmDeliveryParams {
  final String deliveryId;
  final Map<String, dynamic> deliveryData;
  
  ConfirmDeliveryParams({required this.deliveryId, required this.deliveryData});
}

class ReportProblemParams {
  final String deliveryId;
  final String problemType;
  final String description;
  
  ReportProblemParams({required this.deliveryId, required this.problemType, required this.description});
}

class DeliveryHistoryParams {
  final int limit;
  final int offset;
  
  DeliveryHistoryParams({this.limit = 20, this.offset = 0});
}

class SendNotificationParams {
  final String title;
  final String message;
  final Map<String, dynamic>? data;
  
  SendNotificationParams({required this.title, required this.message, this.data});
}

class StartRouteTrackingParams {
  final String routeId;
  final String driverId;
  final String? vehicleId;
  
  StartRouteTrackingParams({
    required this.routeId,
    required this.driverId,
    this.vehicleId,
  });
}

class EndRouteTrackingParams {
  final String routeId;
  
  EndRouteTrackingParams({required this.routeId});
}
