// Dependency Injection setup
// Thiết lập injection cho toàn bộ app theo Clean Architecture

import 'package:get_it/get_it.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

// Data layer
import '../data/services/api_service.dart';
import '../data/services/socket_service.dart';
import '../data/repositories/repository_implementations.dart';

// Domain layer
import '../domain/repositories/repository_interfaces.dart';
import '../domain/usecases/usecases.dart';

// Presentation layer
import '../presentation/blocs/auth_bloc.dart';
import '../presentation/blocs/tracking_bloc.dart';

final GetIt getIt = GetIt.instance;

Future<void> setupDependencyInjection() async {
  // ======== External Dependencies ========
  getIt.registerLazySingleton<FlutterSecureStorage>(
    () => const FlutterSecureStorage(),
  );

  // ======== Data Layer ========
  
  // Services (Data Sources)
  getIt.registerLazySingleton<ApiService>(
    () => ApiService(),
  );

  getIt.registerLazySingleton<SocketService>(
    () => SocketService(),
  );

  // Repository Implementations
  getIt.registerLazySingleton<AuthRepository>(
    () => AuthRepositoryImpl(
      apiService: getIt<ApiService>(),
      secureStorage: getIt<FlutterSecureStorage>(),
    ),
  );

  getIt.registerLazySingleton<TrackingRepository>(
    () => TrackingRepositoryImpl(
      apiService: getIt<ApiService>(),
      socketService: getIt<SocketService>(),
    ),
  );

  getIt.registerLazySingleton<DeliveryRepository>(
    () => DeliveryRepositoryImpl(
      apiService: getIt<ApiService>(),
    ),
  );

  getIt.registerLazySingleton<NotificationRepository>(
    () => NotificationRepositoryImpl(
      socketService: getIt<SocketService>(),
    ),
  );

  // ======== Domain Layer ========
  
  // Use Cases
  getIt.registerLazySingleton<LoginUseCase>(
    () => LoginUseCase(getIt<AuthRepository>()),
  );

  getIt.registerLazySingleton<LogoutUseCase>(
    () => LogoutUseCase(getIt<AuthRepository>()),
  );

  getIt.registerLazySingleton<CheckLoginStatusUseCase>(
    () => CheckLoginStatusUseCase(getIt<AuthRepository>()),
  );

  getIt.registerLazySingleton<StartTrackingUseCase>(
    () => StartTrackingUseCase(repository: getIt<TrackingRepository>()),
  );

  getIt.registerLazySingleton<StopTrackingUseCase>(
    () => StopTrackingUseCase(repository: getIt<TrackingRepository>()),
  );

  getIt.registerLazySingleton<UpdateLocationUseCase>(
    () => UpdateLocationUseCase(getIt<TrackingRepository>()),
  );

  getIt.registerLazySingleton<GetTrackingHistoryUseCase>(
    () => GetTrackingHistoryUseCase(getIt<TrackingRepository>()),
  );

  getIt.registerLazySingleton<GetActiveDeliveriesUseCase>(
    () => GetActiveDeliveriesUseCase(getIt<DeliveryRepository>()),
  );

  getIt.registerLazySingleton<GetDeliveryDetailsUseCase>(
    () => GetDeliveryDetailsUseCase(getIt<DeliveryRepository>()),
  );

  getIt.registerLazySingleton<UpdateDeliveryStatusUseCase>(
    () => UpdateDeliveryStatusUseCase(getIt<DeliveryRepository>()),
  );

  getIt.registerLazySingleton<MarkAsDeliveredUseCase>(
    () => MarkAsDeliveredUseCase(getIt<DeliveryRepository>()),
  );

  // ======== Presentation Layer ========
  
  // BLoCs - Factory để tạo instance mới mỗi lần
  getIt.registerFactory<AuthBloc>(
    () => AuthBloc(
      loginUseCase: getIt<LoginUseCase>(),
      logoutUseCase: getIt<LogoutUseCase>(),
      checkLoginStatusUseCase: getIt<CheckLoginStatusUseCase>(),
      socketService: getIt<SocketService>(),
      secureStorage: getIt<FlutterSecureStorage>(),
    ),
  );

  getIt.registerFactory<TrackingBloc>(
    () => TrackingBloc(
      trackingRepository: getIt<TrackingRepository>(),
    ),
  );
}

// Helper methods để get instances
AuthBloc getAuthBloc() => getIt<AuthBloc>();
TrackingBloc getTrackingBloc() => getIt<TrackingBloc>();

// Clean up method
Future<void> resetDependencies() async {
  await getIt.reset();
}


