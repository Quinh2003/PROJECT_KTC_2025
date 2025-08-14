// Spatial UI Dependency Injection setup with Mock Services
// Thiết lập injection cho toàn bộ app với Mock services thay thế Firebase

import 'package:get_it/get_it.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

// Data layer - Mock Services
import '../data/services/api_service.dart';
import '../data/services/socket_service.dart';
import '../data/services/mock_auth_service.dart';
import '../data/services/mock_data_service.dart';
import '../data/services/notification_service.dart';
import '../data/repositories/repository_implementations.dart';

// Domain layer
import '../domain/repositories/repository_interfaces.dart';
import '../domain/usecases/usecases_spatial.dart';

// Presentation layer
import '../presentation/blocs/auth/auth_bloc.dart';
import '../presentation/blocs/tracking/tracking_bloc.dart';

final GetIt getIt = GetIt.instance;

Future<void> setupSpatialDependencyInjection() async {
  // Clear any existing registrations
  await getIt.reset();

  // ======== External Dependencies ========
  getIt.registerLazySingleton<FlutterSecureStorage>(
    () => const FlutterSecureStorage(),
  );

  // ======== Mock Services ========
  
  // Mock Auth Service (replaces Firebase Auth)
  getIt.registerLazySingleton<MockAuthService>(
    () => MockAuthService(),
  );

  // Mock Data Service (replaces backend API calls)
  getIt.registerLazySingleton<MockDataService>(
    () => MockDataService(),
  );

  // Local Notification Service (replaces Firebase Messaging)
  getIt.registerLazySingleton<NotificationService>(
    () => NotificationService(),
  );

  // Original services (kept for future backend integration)
  getIt.registerLazySingleton<ApiService>(
    () => ApiService(),
  );

  getIt.registerLazySingleton<SocketService>(
    () => SocketService(),
  );

  // ======== Repository Implementations with Mock Services ========
  
  getIt.registerLazySingleton<AuthRepository>(
    () => AuthRepositoryImpl(
      mockAuthService: getIt<MockAuthService>(),
      secureStorage: getIt<FlutterSecureStorage>(),
    ),
  );

  getIt.registerLazySingleton<TrackingRepository>(
    () => TrackingRepositoryImpl(
      apiService: getIt<ApiService>(),
      socketService: getIt<SocketService>(),
      mockDataService: getIt<MockDataService>(),
    ),
  );

  getIt.registerLazySingleton<DeliveryRepository>(
    () => DeliveryRepositoryImpl(
      apiService: getIt<ApiService>(),
      mockDataService: getIt<MockDataService>(),
    ),
  );

  getIt.registerLazySingleton<NotificationRepository>(
    () => NotificationRepositoryImpl(
      notificationService: getIt<NotificationService>(),
      mockDataService: getIt<MockDataService>(),
    ),
  );

  // ======== Domain Layer - Use Cases ========
  
  // Auth Use Cases
  getIt.registerLazySingleton<LoginUseCase>(
    () => LoginUseCase(repository: getIt<AuthRepository>()),
  );

  getIt.registerLazySingleton<LogoutUseCase>(
    () => LogoutUseCase(repository: getIt<AuthRepository>()),
  );

  getIt.registerLazySingleton<CheckLoginStatusUseCase>(
    () => CheckLoginStatusUseCase(repository: getIt<AuthRepository>()),
  );

  getIt.registerLazySingleton<GetCurrentUserUseCase>(
    () => GetCurrentUserUseCase(repository: getIt<AuthRepository>()),
  );

  // Tracking Use Cases
  getIt.registerLazySingleton<StartTrackingUseCase>(
    () => StartTrackingUseCase(repository: getIt<TrackingRepository>()),
  );

  getIt.registerLazySingleton<StopTrackingUseCase>(
    () => StopTrackingUseCase(repository: getIt<TrackingRepository>()),
  );

  getIt.registerLazySingleton<GetCurrentLocationUseCase>(
    () => GetCurrentLocationUseCase(repository: getIt<TrackingRepository>()),
  );

  getIt.registerLazySingleton<GetLocationStreamUseCase>(
    () => GetLocationStreamUseCase(repository: getIt<TrackingRepository>()),
  );

  // Delivery Use Cases
  getIt.registerLazySingleton<GetActiveDeliveriesUseCase>(
    () => GetActiveDeliveriesUseCase(repository: getIt<DeliveryRepository>()),
  );

  getIt.registerLazySingleton<GetDeliveryByIdUseCase>(
    () => GetDeliveryByIdUseCase(repository: getIt<DeliveryRepository>()),
  );

  getIt.registerLazySingleton<ConfirmPickupUseCase>(
    () => ConfirmPickupUseCase(repository: getIt<DeliveryRepository>()),
  );

  getIt.registerLazySingleton<ConfirmDeliveryUseCase>(
    () => ConfirmDeliveryUseCase(repository: getIt<DeliveryRepository>()),
  );

  getIt.registerLazySingleton<ReportProblemUseCase>(
    () => ReportProblemUseCase(repository: getIt<DeliveryRepository>()),
  );

  getIt.registerLazySingleton<GetDeliveryHistoryUseCase>(
    () => GetDeliveryHistoryUseCase(repository: getIt<DeliveryRepository>()),
  );

  // Notification Use Cases
  getIt.registerLazySingleton<SendNotificationUseCase>(
    () => SendNotificationUseCase(repository: getIt<NotificationRepository>()),
  );

  // ======== Presentation Layer - BLoCs ========
  
  getIt.registerFactory<AuthBloc>(
    () => AuthBloc(
      loginUseCase: getIt<LoginUseCase>(),
      logoutUseCase: getIt<LogoutUseCase>(),
      checkLoginStatusUseCase: getIt<CheckLoginStatusUseCase>(),
      getCurrentUserUseCase: getIt<GetCurrentUserUseCase>(),
    ),
  );

  getIt.registerFactory<TrackingBloc>(
    () => TrackingBloc(
      startTrackingUseCase: getIt<StartTrackingUseCase>(),
      stopTrackingUseCase: getIt<StopTrackingUseCase>(),
      getCurrentLocationUseCase: getIt<GetCurrentLocationUseCase>(),
      getLocationStreamUseCase: getIt<GetLocationStreamUseCase>(),
    ),
  );

  // Initialize notification service
  await getIt<NotificationService>().initialize();
  
  print('✅ Spatial UI Dependency Injection setup completed with Mock Services');
}


