// Dependency Injection setup
// Thiết lập injection đơn giản hơn cho toàn bộ app theo mẫu project tham khảo

import 'package:get_it/get_it.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;

// Environment
import '../data/env/environment.dart';

// Services (theo mẫu project tham khảo)
import '../services/api_service.dart';
import '../services/socket_service.dart';
import '../services/map_box_services.dart';
import '../services/push_notification_service.dart';
import '../services/auth_services.dart';
import '../services/user_services.dart';

// Get_It singleton instance
final GetIt getIt = GetIt.instance;

// Thiết lập dependency injection
Future<void> setupDependencyInjection() async {
  // Reset any existing registrations
  await getIt.reset();

  print('🔧 Setting up Dependency Injection...');

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

  // Core Services (theo mẫu project tham khảo)
  getIt.registerLazySingleton<PushNotificationService>(
    () => PushNotificationService(),
  );

  getIt.registerLazySingleton<ApiService>(
    () => ApiService(),
  );

  getIt.registerLazySingleton<SocketService>(
    () => SocketService(),
  );

  getIt.registerLazySingleton<MapBoxServices>(
    () => MapBoxServices(),
  );

  // Domain Services (sử dụng Firebase trực tiếp theo mẫu project tham khảo)
  getIt.registerLazySingleton<AuthServices>(
    () => authServices,
  );

  getIt.registerLazySingleton<UserServices>(
    () => userServices,
  );

  print('✅ Dependency Injection setup completed');
}

// Service Getters
ApiService get apiService => getIt<ApiService>();
SocketService get socketService => getIt<SocketService>();
MapBoxServices get mapBoxServices => getIt<MapBoxServices>();
PushNotificationService get pushNotificationService => getIt<PushNotificationService>();
AuthServices get authServices => getIt<AuthServices>();
UserServices get userServices => getIt<UserServices>();
