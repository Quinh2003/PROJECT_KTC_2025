// Dependency Injection setup
// Thiết lập injection đơn giản hơn cho toàn bộ app

import 'package:get_it/get_it.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;

// Environment
import '../data/env/environment.dart';

// Services
import '../services/api_service.dart';
import '../services/socket_service.dart';
import '../services/notification_service.dart';
import '../services/map_box_services.dart';
import '../services/push_notification.dart';
import '../services/auth_services.dart';

// Get_It singleton instance
final GetIt getIt = GetIt.instance;

// Thiết lập dependency injection
Future<void> setupDependencyInjection() async {
  // Reset any existing registrations
  await getIt.reset();

  // Environment
  getIt.registerLazySingleton<Environment>(
    () => Environment.getInstance(),
  );

  // External Dependencies
  getIt.registerLazySingleton<FlutterSecureStorage>(
    () => const FlutterSecureStorage(),
  );
  
  getIt.registerLazySingleton<http.Client>(
    () => http.Client(),
  );

  // Services
  getIt.registerLazySingleton<ApiService>(
    () => ApiService(),
  );
  
  getIt.registerLazySingleton<SocketService>(
    () => SocketService(),
  );
  
  getIt.registerLazySingleton<NotificationService>(
    () => NotificationService(),
  );
  
  getIt.registerLazySingleton<MapBoxServices>(
    () => MapBoxServices(),
  );
  
  getIt.registerLazySingleton<PushNotification>(
    () => PushNotification(),
  );
  
  getIt.registerLazySingleton<AuthServices>(
    () => AuthServices(),
  );

  // BLoCs sẽ được đăng ký trong MultiBlocProvider trong file main.dart
}

// Đơn giản hóa việc lấy services từ GetIt
ApiService get apiService => getIt<ApiService>();
SocketService get socketService => getIt<SocketService>();
NotificationService get notificationService => getIt<NotificationService>();
MapBoxServices get mapBoxService => getIt<MapBoxServices>();
PushNotification get pushNotification => getIt<PushNotification>();
AuthServices get authServices => getIt<AuthServices>();

// Đơn giản hóa việc lấy environment
Environment get environment => getIt<Environment>();
