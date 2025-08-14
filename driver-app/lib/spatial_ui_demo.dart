import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'presentation/screens/order_detail_screen.dart';
import 'presentation/screens/route_map_screen.dart';

// This is a demo main file to showcase the new Spatial UI screens
// For production, integrate these routes into the existing main.dart

void main() {
  WidgetsFlutterBinding.ensureInitialized();
  SystemChrome.setPreferredOrientations([
    DeviceOrientation.portraitUp,
    DeviceOrientation.portraitDown,
  ]);
  runApp(const SpatialUIDemo());
}

class SpatialUIDemo extends StatelessWidget {
  const SpatialUIDemo({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'KTC Logistics - Spatial UI Demo',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        useMaterial3: true,
        colorScheme: ColorScheme.fromSeed(
          seedColor: const Color(0xFF4264FB),
          brightness: Brightness.light,
        ),
        fontFamily: 'Poppins',
      ),
      darkTheme: ThemeData(
        useMaterial3: true,
        colorScheme: ColorScheme.fromSeed(
          seedColor: const Color(0xFF4264FB),
          brightness: Brightness.dark,
        ),
        fontFamily: 'Poppins',
      ),
      themeMode: ThemeMode.system,
      initialRoute: '/order-detail',
      routes: {
        '/order-detail': (context) => const OrderDetailScreen(orderId: 'KTC-2025-08-14-123'),
        '/route-map': (context) {
          final args = ModalRoute.of(context)?.settings.arguments as String? ?? 'RT-2025-08-14-01';
          return RouteMapScreen(routeId: args);
        },
      },
    );
  }
}


