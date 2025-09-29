import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:get_it/get_it.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_forgot_password_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_check_email_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/onboarding/onboarding_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/environment/environment_selection_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/map/route_map_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/order/order_detail_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/main_screen.dart';
import 'package:provider/provider.dart';
import 'services/auth_services.dart';
import 'services/tracking_services.dart';
import 'services/push_notification_services.dart';
import 'services/delivery_services.dart';
import 'services/maintenance_services.dart';
import 'services/user_services.dart';
import 'presentation/blocs/tracking/simple_tracking_bloc.dart';

final getIt = GetIt.instance;

class AppRouter {
  static Route<dynamic> generateRoute(RouteSettings settings) {
    switch (settings.name) {
      case '/':
        return MaterialPageRoute(builder: (_) => const OnboardingScreen());
      case '/onboarding':
        return MaterialPageRoute(builder: (_) => const OnboardingScreen());
      case '/environment':
        return MaterialPageRoute(builder: (_) => const EnvironmentSelectionScreen());
      case '/login':
        return MaterialPageRoute(builder: (_) => const SpatialLoginScreen());
      case '/forgot-password':
        return MaterialPageRoute(builder: (_) => const SpatialForgotPasswordScreen());
      case '/check-email':
        final email = settings.arguments as String?;
        return MaterialPageRoute(builder: (_) => SpatialCheckEmailScreen(email: email ?? ''));
      case '/dashboard':
        return MaterialPageRoute(builder: (_) => const MainScreen(initialIndex: 0));
      case '/deliveries':
        return MaterialPageRoute(builder: (_) => const MainScreen(initialIndex: 1));
      case '/maintenance':
        return MaterialPageRoute(builder: (_) => const MainScreen(initialIndex: 2));
      case '/profile':
        return MaterialPageRoute(builder: (_) => const MainScreen(initialIndex: 3));
      case '/routes':
        // Temporarily use a default routeId
        return MaterialPageRoute(
            builder: (_) => const RouteMapScreen(routeId: 'RT-2025-08-14-01'));
      case '/order-detail':
        // Get order ID from arguments
        final orderId = settings.arguments as String;
        return MaterialPageRoute(
            builder: (_) => OrderDetailScreen(orderId: orderId));
      default:
        return MaterialPageRoute(
          builder: (_) => Scaffold(
            body: Center(
              child: Text('Route not found for ${settings.name}'),
            ),
          ),
        );
    }
  }
}

class App extends StatefulWidget {
  const App({super.key});

  @override
  State<App> createState() => _AppState();
}

class _AppState extends State<App> {
  @override
  void initState() {
    super.initState();
    pushNotificationService.onMessagingListener();
  }

  @override
  Widget build(BuildContext context) {
    // Use MultiProvider directly, no need for intermediate class
    return MultiProvider(
      providers: [
        // Location service with managed lifecycle
        Provider<LocationService>(
          create: (_) => LocationService(),
          dispose: (_, service) => service.stopBackgroundLocationService(),
        ),
      ],
      child: MultiBlocProvider(
        providers: [
          // Auth Bloc - chỉ giữ lại những gì cần thiết cho login
          BlocProvider<AuthBloc>(
            create: (context) => AuthBloc(
              authServices: AuthServices(),
            ), // Bỏ CheckLoginEvent để tránh xung đột
          ),
          // Thêm DeliveryBloc
          BlocProvider<DeliveryBloc>(
            create: (context) => DeliveryBloc(
              deliveryServices: DeliveryServices(),
            ),
            lazy: true, // Chỉ tạo khi cần
          ),
          // Thêm MaintenanceBloc
          BlocProvider<MaintenanceBloc>(
            create: (context) => MaintenanceBloc(
              apiService: MaintenanceServices(),
            ),
            lazy: true, // Chỉ tạo khi cần
          ),
          // Thêm UserBloc cho profile screen
          BlocProvider<UserBloc>(
            create: (context) => UserBloc(
              userServices: UserServices(),
            ),
            lazy: true, // Chỉ tạo khi cần
          ),
          // Thêm SimpleTrackingBloc cho dashboard tracking
          BlocProvider<SimpleTrackingBloc>(
            create: (context) => SimpleTrackingBloc(
              locationService: LocationService(),
            ),
            lazy: true, // Chỉ tạo khi cần
          ),
        ],
        child: MaterialApp(
          title: 'KTC Logistics',
          debugShowCheckedModeBanner: false,
          theme: ThemeData(
            useMaterial3: true,
            colorScheme: ColorScheme.fromSeed(
              seedColor: Colors.blue,
              brightness: Brightness.light,
            ),
            fontFamily: 'Poppins',
            scaffoldBackgroundColor: Colors.transparent,
          ),
          darkTheme: ThemeData(
            useMaterial3: true,
            colorScheme: ColorScheme.fromSeed(
              seedColor: Colors.blue,
              brightness: Brightness.dark,
            ),
            fontFamily: 'Poppins',
            scaffoldBackgroundColor: Colors.transparent,
          ),
          themeMode: ThemeMode.system,
          onGenerateRoute: AppRouter.generateRoute,
          // Chuyển thẳng đến màn hình onboarding hoặc login dựa vào trạng thái
          home: const OnboardingStartScreen(),
        ),
      ),
    );
  }
}

/// Màn hình khởi động để kiểm tra trạng thái onboarding và chuyển hướng phù hợp
class OnboardingStartScreen extends StatefulWidget {
  const OnboardingStartScreen({super.key});

  @override
  State<OnboardingStartScreen> createState() => _OnboardingStartScreenState();
}

class _OnboardingStartScreenState extends State<OnboardingStartScreen> {
  @override
  void initState() {
    super.initState();
    // Kiểm tra trạng thái onboarding ngay lập tức mà không hiển thị splash screen
    _checkOnboardingAndNavigate();
  }

  Future<void> _checkOnboardingAndNavigate() async {
    // Always show onboarding for demo purposes
    // Use addPostFrameCallback to ensure navigation happens after build
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (context.mounted) {
        // Navigate to onboarding screen every time for demo
        Navigator.of(context).pushReplacementNamed('/onboarding');
      }
    });
  }

  @override
  Widget build(BuildContext context) {
    // Màn hình trống tối giản trong khi kiểm tra trạng thái
    return Scaffold(
      backgroundColor: Colors.white,
    );
  }
}
