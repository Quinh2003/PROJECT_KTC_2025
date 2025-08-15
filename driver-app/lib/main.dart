import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:get_it/get_it.dart';

// Dependency Injection
import 'injection/dependency_injection.dart';

// Services
import 'services/auth_services.dart';
import 'services/api_service.dart';

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  
  // Setup dependency injection
  await setupDependencyInjection();
  
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'KTC Logistics Driver',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
        useMaterial3: true,
      ),
      home: const HomeScreen(),
    );
  }
}

class HomeScreen extends StatelessWidget {
  const HomeScreen({super.key});

  @override
  Widget build(BuildContext context) {
    final authServices = GetIt.instance<AuthServices>();
    
    return Scaffold(
      appBar: AppBar(
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        title: const Text('KTC Logistics Driver'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            const Text(
              'Welcome to KTC Logistics Driver App',
              style: TextStyle(fontSize: 18),
            ),
            const SizedBox(height: 20),
            ElevatedButton(
              onPressed: () async {
                try {
                  final isLoggedIn = await authServices.isLoggedIn();
                  if (!context.mounted) return;
                  
                  ScaffoldMessenger.of(context).showSnackBar(
                    SnackBar(content: Text('Login status: $isLoggedIn')),
                  );
                } catch (e) {
                  if (!context.mounted) return;
                  
                  ScaffoldMessenger.of(context).showSnackBar(
                    SnackBar(content: Text('Error: $e')),
                  );
                }
              },
              child: const Text('Check Login Status'),
            ),
          ],
        ),
      ),
    );
  }
}

class _MyAppState extends State<MyApp> {
  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    // Set system UI overlay style for a modern look
    SystemChrome.setSystemUIOverlayStyle(
      const SystemUiOverlayStyle(
        statusBarColor: Colors.transparent, 
        statusBarIconBrightness: Brightness.dark,
      ),
    );

    return MultiBlocProvider(
      providers: [ 
        // Core authentication and tracking BLoCs with dependency injection
        BlocProvider(create: (context) => getIt<AuthBloc>()..add(CheckLoginEvent())),
        BlocProvider(create: (context) => getIt<TrackingBloc>()),
      ],
      child: MaterialApp(
        debugShowCheckedModeBanner: false,
        title: 'KTC Logistics Driver',
        
        // Use Material theme
        theme: ThemeData(
          useMaterial3: true,
          colorScheme: ColorScheme.fromSeed(
            seedColor: const Color(0xFF00AB6B),
            brightness: Brightness.light,
          ),
          textTheme: const TextTheme(
            displayLarge: TextStyle(
              fontSize: 28.0,
              fontWeight: FontWeight.bold,
              color: Color(0xFF333333),
            ),
            titleMedium: TextStyle(
              fontSize: 16.0,
              fontWeight: FontWeight.w500,
              color: Color(0xFF333333),
            ),
            bodyMedium: TextStyle(
              fontSize: 14.0,
              fontWeight: FontWeight.normal,
              color: Color(0xFF666666),
            ),
          ),
        ),
        
        // App routing based on authentication state
        home: BlocBuilder<AuthBloc, AuthState>(
          builder: (context, state) {
            if (state is AuthenticatedState) {
              return const DashboardScreenSpatial();
            } else {
              return const SpatialLoginScreen();
            }
          },
        ),
        
        // Named routes for navigation
        routes: {
          '/login': (context) => const SpatialLoginScreen(),
          '/dashboard': (context) => const DashboardScreenSpatial(),
        },
      ),
    );
  }
}


