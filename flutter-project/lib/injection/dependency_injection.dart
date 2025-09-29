// Dependency Injection setup
// Thiết lập injection đơn giản hơn cho toàn bộ app theo mẫu project tham khảo

import 'package:get_it/get_it.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;

// Environment
import '../data/env/environment.dart';

// Services (theo mẫu project tham khảo)
import '../services/socket_service.dart';
import '../services/mapbox_services.dart';
import '../services/push_notification_services.dart';
import '../services/auth_services.dart';
import '../services/user_services.dart';
import '../services/delivery_services.dart';
import '../services/driver_services.dart';
import '../services/orders_services.dart';

// Blocs
import '../presentation/blocs/driver/driver_bloc.dart';
import '../presentation/blocs/delivery/delivery_bloc.dart';
import '../presentation/blocs/orders/orders_bloc.dart';

// Get_It singleton instance
final GetIt getIt = GetIt.instance;

// Thiết lập dependency injection
Future<void> setupDependencyInjection() async {
  print('🔧 Setting up Dependency Injection...');

  // Environment - singleton pattern
  if (!getIt.isRegistered<Environment>()) {
    getIt.registerLazySingleton<Environment>(
      () => Environment.getInstance(),
    );
  }

  // External Dependencies - optimize for reuse
  if (!getIt.isRegistered<FlutterSecureStorage>()) {
    getIt.registerLazySingleton<FlutterSecureStorage>(
      () => const FlutterSecureStorage(
        aOptions: AndroidOptions(
          encryptedSharedPreferences: true,
        ),
        iOptions: IOSOptions(
          accessibility: KeychainAccessibility.first_unlock_this_device,
        ),
      ),
    );
  }
  
  if (!getIt.isRegistered<http.Client>()) {
    getIt.registerLazySingleton<http.Client>(
      () => http.Client(),
      dispose: (client) => client.close(), // Proper disposal
    );
  }

  // Core Services - lazy singletons for better memory management
  _registerServices();
  
  // Blocs - factory pattern for proper lifecycle
  _registerBlocs();

  print('✅ Dependency Injection setup completed');
}

/// Register services with proper disposal
void _registerServices() {
  if (!getIt.isRegistered<PushNotificationService>()) {
    getIt.registerLazySingleton<PushNotificationService>(
      () => PushNotificationService(),
    );
  }

  if (!getIt.isRegistered<SocketService>()) {
    getIt.registerLazySingleton<SocketService>(
      () => SocketService(),
      dispose: (service) => service.disconnect(),
    );
  }

  if (!getIt.isRegistered<MapBoxServices>()) {
    getIt.registerLazySingleton<MapBoxServices>(
      () => MapBoxServices(),
    );
  }

  if (!getIt.isRegistered<AuthServices>()) {
    getIt.registerLazySingleton<AuthServices>(
      () => AuthServices(),
    );
  }

  if (!getIt.isRegistered<UserServices>()) {
    getIt.registerLazySingleton<UserServices>(
      () => UserServices(),
    );
  }
  
  if (!getIt.isRegistered<DeliveryServices>()) {
    getIt.registerLazySingleton<DeliveryServices>(
      () => DeliveryServices(),
    );
  }
  
  if (!getIt.isRegistered<DriverServices>()) {
    getIt.registerLazySingleton<DriverServices>(
      () => DriverServices(),
    );
  }
  
  if (!getIt.isRegistered<OrdersServices>()) {
    getIt.registerLazySingleton<OrdersServices>(
      () => OrdersServices(),
    );
  }
}

/// Register BLoCs as factories for proper lifecycle management
void _registerBlocs() {
  getIt.registerFactory<DriverBloc>(
    () => DriverBloc(driverServices: getIt<DriverServices>()),
  );
  
  getIt.registerFactory<DeliveryBloc>(
    () => DeliveryBloc(deliveryServices: getIt<DeliveryServices>()),
  );
  
  getIt.registerFactory<OrdersBloc>(
    () => OrdersBloc(ordersServices: getIt<OrdersServices>()),
  );
}

// Service Getters
SocketService get socketService => getIt<SocketService>();
MapBoxServices get mapBoxServices => getIt<MapBoxServices>();
PushNotificationService get pushNotificationService => getIt<PushNotificationService>();
AuthServices get authServices => getIt<AuthServices>();
UserServices get userServices => getIt<UserServices>();
DeliveryServices get deliveryServices => getIt<DeliveryServices>();
DriverServices get driverServices => getIt<DriverServices>();
DriverBloc get driverBloc => getIt<DriverBloc>();
DeliveryBloc get deliveryBloc => getIt<DeliveryBloc>();
