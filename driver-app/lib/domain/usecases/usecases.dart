// Domain layer - Use Cases
// Business logic use cases theo Clean Architecture

import '../repositories/repository_interfaces.dart';
import '../models/response/auth_response.dart';

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
  
  LoginUseCase(this.repository);
  
  @override
  Future<AuthResponse> call(LoginParams params) async {
    return await repository.login(params.email, params.password);
  }
}

class LogoutUseCase implements UseCase<void, NoParams> {
  final AuthRepository repository;
  
  LogoutUseCase(this.repository);
  
  @override
  Future<void> call(NoParams params) async {
    return await repository.logout();
  }
}

class CheckLoginStatusUseCase implements UseCase<bool, NoParams> {
  final AuthRepository repository;
  
  CheckLoginStatusUseCase(this.repository);
  
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

class StartTrackingUseCase implements UseCase<void, NoParams> {
  final TrackingRepository repository;
  
  StartTrackingUseCase({required this.repository});
  
  @override
  Future<void> call(NoParams params) async {
    return await repository.startTracking();
  }
}

class StopTrackingUseCase implements UseCase<void, NoParams> {
  final TrackingRepository repository;
  
  StopTrackingUseCase({required this.repository});
  
  @override
  Future<void> call(NoParams params) async {
    return await repository.stopTracking();
  }
}

class GetCurrentLocationUseCase implements UseCase<Map<String, dynamic>, NoParams> {
  final TrackingRepository repository;
  
  GetCurrentLocationUseCase({required this.repository});
  
  @override
  Future<Map<String, dynamic>> call(NoParams params) async {
    return await repository.getCurrentLocation();
  }
}

class GetLocationStreamUseCase implements StreamUseCase<Map<String, dynamic>, NoParams> {
  final TrackingRepository repository;
  
  GetLocationStreamUseCase({required this.repository});
  
  @override
  Stream<Map<String, dynamic>> call(NoParams params) {
    return repository.getLocationStream();
  }
}

class UpdateLocationUseCase implements UseCase<void, LocationParams> {
  final TrackingRepository repository;
  
  UpdateLocationUseCase(this.repository);
  
  @override
  Future<void> call(LocationParams params) async {
    return await repository.updateLocation(params.latitude, params.longitude);
  }
}

class GetTrackingHistoryUseCase implements UseCase<List<TrackingPoint>, TrackingHistoryParams> {
  final TrackingRepository repository;
  
  GetTrackingHistoryUseCase(this.repository);
  
  @override
  Future<List<TrackingPoint>> call(TrackingHistoryParams params) async {
    return await repository.getTrackingHistory(
      driverId: params.driverId,
      vehicleId: params.vehicleId,
      startDate: params.startDate,
      endDate: params.endDate,
    );
  }
}

// ======== Delivery Use Cases ========

class GetActiveDeliveriesUseCase implements UseCase<List<OrderModel>, NoParams> {
  final DeliveryRepository repository;
  
  GetActiveDeliveriesUseCase(this.repository);
  
  @override
  Future<List<OrderModel>> call(NoParams params) async {
    return await repository.getActiveDeliveries();
  }
}

class GetDeliveryDetailsUseCase implements UseCase<OrderModel, DeliveryDetailsParams> {
  final DeliveryRepository repository;
  
  GetDeliveryDetailsUseCase(this.repository);
  
  @override
  Future<OrderModel> call(DeliveryDetailsParams params) async {
    return await repository.getDeliveryById(params.orderId);
  }
}

class UpdateDeliveryStatusUseCase implements UseCase<void, UpdateDeliveryStatusParams> {
  final DeliveryRepository repository;
  
  UpdateDeliveryStatusUseCase(this.repository);
  
  @override
  Future<void> call(UpdateDeliveryStatusParams params) async {
    return await repository.updateDeliveryStatus(params.orderId, params.status);
  }
}

class MarkAsDeliveredUseCase implements UseCase<void, MarkAsDeliveredParams> {
  final DeliveryRepository repository;
  
  MarkAsDeliveredUseCase(this.repository);
  
  @override
  Future<void> call(MarkAsDeliveredParams params) async {
    return await repository.markAsDelivered(
      params.orderId, 
      params.signature, 
      params.photo
    );
  }
}

// ======== Parameters Classes ========

class LoginParams {
  final String email;
  final String password;
  
  LoginParams({required this.email, required this.password});
}

class LocationParams {
  final double latitude;
  final double longitude;
  
  LocationParams({required this.latitude, required this.longitude});
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
  final String orderId;
  
  DeliveryDetailsParams({required this.orderId});
}

class UpdateDeliveryStatusParams {
  final String orderId;
  final DeliveryStatus status;
  
  UpdateDeliveryStatusParams({required this.orderId, required this.status});
}

class MarkAsDeliveredParams {
  final String orderId;
  final String signature;
  final String photo;
  
  MarkAsDeliveredParams({
    required this.orderId,
    required this.signature,
    required this.photo,
  });
}


