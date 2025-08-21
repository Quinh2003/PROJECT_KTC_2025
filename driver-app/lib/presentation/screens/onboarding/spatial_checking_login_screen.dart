import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:flutter_animate/flutter_animate.dart';

class SpatialCheckingLoginScreen extends StatefulWidget {
  const SpatialCheckingLoginScreen({Key? key}) : super(key: key);

  @override
  State<SpatialCheckingLoginScreen> createState() => _SpatialCheckingLoginScreenState();
}

class _SpatialCheckingLoginScreenState extends State<SpatialCheckingLoginScreen> with TickerProviderStateMixin {
  late AnimationController _controller;

  @override
  void initState() {
    super.initState();
    _controller = AnimationController(
      vsync: this,
      duration: const Duration(seconds: 2),
    )..repeat();

    // Trigger check auth status after a delay
    Future.delayed(const Duration(milliseconds: 500), () {
      // For demo purposes, just navigate to login after 2 seconds
      Future.delayed(const Duration(seconds: 2), () {
        Navigator.of(context).pushReplacement(
          MaterialPageRoute(
            builder: (context) => const SpatialLoginScreen(),
          ),
        );
      });
    });
  }

  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SpatialComponents.backgroundContainer(
        useDarkMode: true, // Sử dụng dark mode
        child: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              SizedBox(
                width: 80,
                height: 80,
                child: CircularProgressIndicator(
                  color: SpatialTheme.primaryBlue,
                  strokeWidth: 5,
                ),
              ),
              const SizedBox(height: 32),
              Text(
                'Đang kiểm tra phiên đăng nhập...',
                style: TextStyle(
                  color: Colors.white, // Sử dụng màu trắng cho dark mode
                  fontSize: 16,
                  fontWeight: FontWeight.w500,
                ),
              ).animate().fadeIn(
                duration: 600.ms,
                delay: 300.ms,
              ),
            ],
          ),
        ),
      ),
    );
  }
}
