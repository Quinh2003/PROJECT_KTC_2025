import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:get_it/get_it.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/onboarding/onboarding_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/dashboard/dashboard_screen_spatial.dart';
import 'package:ktc_logistics_driver/presentation/screens/developer_test_screen.dart';
import 'package:ktc_logistics_driver/domain/usecases/usecases.dart';
import 'package:ktc_logistics_driver/data/repositories/repository_implementations.dart' as mock_repo;
import 'package:ktc_logistics_driver/services/mock_data_service.dart';
import 'package:ktc_logistics_driver/services/mock_auth_service.dart';
import 'services/push_notification_service.dart';

final getIt = GetIt.instance;

class AppRouter {
  static Route<dynamic> generateRoute(RouteSettings settings) {
    switch (settings.name) {
      case '/':
        return MaterialPageRoute(builder: (_) => const DeveloperTestScreen()); // Debug menu first
      case '/debug':
        return MaterialPageRoute(builder: (_) => const DeveloperTestScreen());
      case '/onboarding':
        return MaterialPageRoute(builder: (_) => const OnboardingScreen());
      case '/login':
        return MaterialPageRoute(builder: (_) => const SpatialLoginScreen());
      case '/dashboard':
        return MaterialPageRoute(builder: (_) => const DashboardScreenSpatial());
      default:
        return MaterialPageRoute(
          builder: (_) => Scaffold(
            body: Center(
              child: Text('Không tìm thấy route cho ${settings.name}'),
            ),
          ),
        );
    }
  }
}

class App extends StatefulWidget {
  const App({Key? key}) : super(key: key);

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
    return MultiBlocProvider(
      providers: [
        // Temporarily using simplified BLoC providers without dependency injection
        // TODO: Implement proper dependency injection with use cases
        BlocProvider<UserBloc>(
          create: (context) => UserBloc(userServices: getIt()),
        ),
        // Add AuthBloc provider to fix the "Provider<AuthBloc> not found" error
        BlocProvider<AuthBloc>(
          create: (context) => AuthBloc(
            loginUseCase: LoginUseCase(
              repository: mock_repo.AuthRepositoryImpl(
                mockAuthService: MockAuthService(),
                secureStorage: const FlutterSecureStorage(),
              ),
            ),
            logoutUseCase: LogoutUseCase(
              repository: mock_repo.AuthRepositoryImpl(
                mockAuthService: MockAuthService(),
                secureStorage: const FlutterSecureStorage(),
              ),
            ),
            checkLoginStatusUseCase: CheckLoginStatusUseCase(
              repository: mock_repo.AuthRepositoryImpl(
                mockAuthService: MockAuthService(),
                secureStorage: const FlutterSecureStorage(),
              ),
            ),
            getCurrentUserUseCase: GetCurrentUserUseCase(
              repository: mock_repo.AuthRepositoryImpl(
                mockAuthService: MockAuthService(),
                secureStorage: const FlutterSecureStorage(),
              ),
            ),
          ),
        ),
        // Add TrackingBloc with mock implementation to fix Provider error
        BlocProvider<TrackingBloc>(
          create: (context) => TrackingBloc(
            startRouteTrackingUseCase: StartRouteTrackingUseCase(
              repository: mock_repo.TrackingRepositoryImpl(mockDataService: MockDataService()),
            ),
            endRouteTrackingUseCase: EndRouteTrackingUseCase(
              repository: mock_repo.TrackingRepositoryImpl(mockDataService: MockDataService()),
            ),
            updateLocationUseCase: UpdateLocationUseCase(
              repository: mock_repo.TrackingRepositoryImpl(mockDataService: MockDataService()),
            ),
            getTrackingHistoryUseCase: GetTrackingHistoryUseCase(
              repository: mock_repo.TrackingRepositoryImpl(mockDataService: MockDataService()),
            ),
          ),
        ),
      ],
      child: MaterialApp(
        title: 'KTC Logistics Driver',
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
        home: const SplashScreen(),
      ),
    );
  }
}

class SplashScreen extends StatefulWidget {
  const SplashScreen({Key? key}) : super(key: key);

  @override
  State<SplashScreen> createState() => _SplashScreenState();
}

class _SplashScreenState extends State<SplashScreen> with SingleTickerProviderStateMixin {
  late AnimationController _animationController;
  late Animation<double> _fadeAnimation;
  
  @override
  void initState() {
    super.initState();
    
    _animationController = AnimationController(
      vsync: this,
      duration: const Duration(seconds: 2),
    );
    
    _fadeAnimation = Tween<double>(begin: 0.0, end: 1.0).animate(
      CurvedAnimation(
        parent: _animationController,
        curve: Curves.easeIn,
      ),
    );
    
    _animationController.forward();
    
    // Check if the user has completed onboarding
    _checkOnboarding();
  }
  
  Future<void> _checkOnboarding() async {
    // Simulating a delay for splash screen
    await Future.delayed(const Duration(seconds: 3));
    
    // Check if the user has completed onboarding using secure storage
    const storage = FlutterSecureStorage();
    final hasCompletedOnboarding = await storage.read(key: 'onboarding_completed') == 'true';
    
    if (context.mounted) {
      if (!hasCompletedOnboarding) {
        // Navigate to onboarding screen
        Navigator.of(context).pushReplacementNamed('/onboarding');
      } else {
        // For now, navigate directly to login since we don't have proper auth state
        // TODO: Implement proper authentication state listening
        Navigator.of(context).pushReplacementNamed('/login');
      }
    }
  }
  
  @override
  void dispose() {
    _animationController.dispose();
    super.dispose();
  }
  
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Container(
        decoration: BoxDecoration(
          gradient: LinearGradient(
            begin: Alignment.topCenter,
            end: Alignment.bottomCenter,
            colors: [
              Colors.blue.shade800,
              Colors.blue.shade600,
              Colors.blue.shade400,
            ],
          ),
        ),
        child: Center(
          child: FadeTransition(
            opacity: _fadeAnimation,
            child: Column(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                // Logo
                Container(
                  padding: const EdgeInsets.all(20),
                  decoration: BoxDecoration(
                    color: Colors.white,
                    shape: BoxShape.circle,
                    boxShadow: [
                      BoxShadow(
                        color: Colors.black.withValues(alpha: 0.1),
                        blurRadius: 20,
                        spreadRadius: 5,
                      ),
                    ],
                  ),
                  child: Icon(
                    Icons.local_shipping_rounded,
                    size: 80,
                    color: Colors.blue.shade700,
                  ),
                ),
                const SizedBox(height: 30),
                // App name
                Text(
                  'KTC Logistics',
                  style: TextStyle(
                    fontSize: 32,
                    fontWeight: FontWeight.bold,
                    color: Colors.white,
                    shadows: [
                      Shadow(
                        color: Colors.black.withValues(alpha: 0.1),
                        offset: const Offset(0, 2),
                        blurRadius: 4,
                      ),
                    ],
                  ),
                ),
                const SizedBox(height: 8),
                // Tagline
                Text(
                  'Đồng hành cùng mọi hành trình',
                  style: TextStyle(
                    fontSize: 16,
                    color: Colors.white.withValues(alpha: 0.9),
                  ),
                ),
                const SizedBox(height: 60),
                // Loading indicator
                CircularProgressIndicator(
                  valueColor: AlwaysStoppedAnimation<Color>(
                    Colors.white.withValues(alpha: 0.9),
                  ),
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}
