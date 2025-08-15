import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter_bloc/flutter_bloc.dart';

// Dependencies
import 'package:ktc_logistics_driver/injection/dependency_injection.dart';
import 'package:ktc_logistics_driver/presentation/blocs/auth/auth_bloc.dart';
import 'package:ktc_logistics_driver/presentation/blocs/auth/auth_event.dart';
import 'package:ktc_logistics_driver/presentation/blocs/auth/auth_state.dart';
import 'package:ktc_logistics_driver/presentation/blocs/tracking/tracking_bloc.dart';

// UI
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/dashboard_screen_spatial.dart';

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  
  // Setup dependency injection with simplified approach
  await setupDependencyInjection();
  
  runApp(MyApp());
}

class MyApp extends StatefulWidget {
  @override
  State<MyApp> createState() => _MyAppState();
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
        BlocProvider(create: (context) => getIt<AuthSpatial.AuthBloc>()..add(AuthSpatialEvent.CheckLoginEvent())),
        BlocProvider(create: (context) => getIt<TrackingSpatial.TrackingBloc>()),
        
        // Other app BLoCs (can be updated to use DI later)
        BlocProvider(create: (context) => GeneralBloc()),
        BlocProvider(create: (context) => ProductsBloc()),
        BlocProvider(create: (context) => CartBloc()),
        BlocProvider(create: (context) => UserBloc()),
        BlocProvider(create: (context) => MylocationmapBloc()),
        BlocProvider(create: (context) => PaymentsBloc()),
        BlocProvider(create: (context) => OrdersBloc()),
        BlocProvider(create: (context) => DeliveryBloc()),
        BlocProvider(create: (context) => MapdeliveryBloc()),
        BlocProvider(create: (context) => MapclientBloc()),
      ],
      child: MaterialApp(
        debugShowCheckedModeBanner: false,
        title: 'KTC Logistics Driver - Spatial UI',
        
        // Use Spatial design theme
        theme: SpatialTheme.lightTheme,
        darkTheme: SpatialTheme.darkTheme,
        themeMode: ThemeMode.system,
        
        // App routing based on authentication state
        home: BlocBuilder<AuthSpatial.AuthBloc, AuthSpatialState.AuthState>(
          builder: (context, state) {
            if (state is AuthSpatialState.AuthenticatedState) {
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


