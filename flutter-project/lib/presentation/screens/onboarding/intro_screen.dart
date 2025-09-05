import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart' as spatialTheme;
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';

class IntroScreen extends StatefulWidget {
  const IntroScreen({super.key});

  @override
  _IntroScreenState createState() => _IntroScreenState();
}

class _IntroScreenState extends State<IntroScreen> {
  final PageController _pageController = PageController();
  int _currentPage = 0;
  final int _totalPages = 3;

  final List<Map<String, dynamic>> _introData = [
    {
      'title': 'Quản lý đơn hàng thông minh',
      'description': 'Theo dõi đơn hàng, lịch trình giao hàng và chi tiết đơn hàng trong một ứng dụng duy nhất.',
      'icon': FontAwesomeIcons.boxOpen,
    },
    {
      'title': 'Dẫn đường chính xác',
      'description': 'Bản đồ 3D hiện đại cùng AI tối ưu lộ trình giúp bạn giao hàng nhanh chóng và tiết kiệm nhiên liệu.',
      'icon': FontAwesomeIcons.route,
    },
    {
      'title': 'Quản lý thu nhập dễ dàng',
      'description': 'Theo dõi thu nhập, tiền thưởng và thanh toán trực tiếp qua ứng dụng một cách minh bạch.',
      'icon': FontAwesomeIcons.wallet,
    },
  ];

  @override
  void dispose() {
    _pageController.dispose();
    super.dispose();
  }

  void _onPageChanged(int page) {
    setState(() {
      _currentPage = page;
    });
  }

  void _navigateToNextPage() {
    if (_currentPage < _totalPages - 1) {
      _pageController.nextPage(
        duration: const Duration(milliseconds: 500),
        curve: Curves.easeInOut,
      );
    } else {
      _navigateToLogin();
    }
  }

  void _navigateToLogin() {
    Navigator.of(context).pushReplacement(
      MaterialPageRoute(builder: (context) => const SpatialLoginScreen()),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SpatialComponents.backgroundContainer(
        useDarkMode: true, // Set to dark mode
        child: SafeArea(
          child: Column(
            children: [
              // Skip button at the top right
              Align(
                alignment: Alignment.topRight,
                child: Padding(
                  padding: const EdgeInsets.all(spatialTheme.SpatialTheme.spaceMD),
                  child: _currentPage < _totalPages - 1
                      ? TextButton(
                          onPressed: _navigateToLogin,
                          child: Text(
                            'Bỏ qua',
                            style: TextStyle(
                              color: Colors.white.withOpacity(0.7),
                              fontWeight: FontWeight.w500,
                            ),
                          ),
                        )
                      : const SizedBox.shrink(),
                ),
              ),

              const SizedBox(height: spatialTheme.SpatialTheme.spaceLG),

              // Logo
              SizedBox(
                height: 80,
                child: Image.asset(
                  'assets/logo/ktc_logistics_logo.png',
                  fit: BoxFit.contain,
                ),
              ).animate().fadeIn(duration: 800.ms),

              const SizedBox(height: spatialTheme.SpatialTheme.spaceXL),

              // Page View
              Expanded(
                child: PageView.builder(
                  controller: _pageController,
                  onPageChanged: _onPageChanged,
                  itemCount: _totalPages,
                  itemBuilder: (context, index) {
                    return _buildIntroPage(
                      title: _introData[index]['title'],
                      description: _introData[index]['description'],
                      icon: _introData[index]['icon'],
                    );
                  },
                ),
              ),

              const SizedBox(height: spatialTheme.SpatialTheme.spaceLG),

              // Page indicators
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: List.generate(
                  _totalPages,
                  (index) => _buildPageIndicator(index),
                ),
              ),

              const SizedBox(height: spatialTheme.SpatialTheme.spaceLG),

              // Next button or Get Started
              Padding(
                padding: const EdgeInsets.symmetric(
                  horizontal: spatialTheme.SpatialTheme.spaceLG,
                  vertical: spatialTheme.SpatialTheme.spaceMD,
                ),
                child: SpatialComponents.gradientButton(
                  text: _currentPage < _totalPages - 1 ? 'Tiếp tục' : 'Bắt đầu',
                  onPressed: _navigateToNextPage,
                  icon: _currentPage < _totalPages - 1
                      ? FontAwesomeIcons.arrowRight
                      : FontAwesomeIcons.rocket,
                ),
              ),

              const SizedBox(height: spatialTheme.SpatialTheme.spaceMD),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildIntroPage({
    required String title,
    required String description,
    required IconData icon,
  }) {
    return Padding(
      padding: const EdgeInsets.symmetric(horizontal: spatialTheme.SpatialTheme.spaceLG),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          // Icon
          SpatialComponents.glassContainer(
            useDarkMode: true,
            width: 120,
            height: 120,
            borderRadius: BorderRadius.circular(60),
            child: Icon(
              icon,
              size: 50,
              color: Colors.white,
            ),
          ).animate().scale(
            duration: 600.ms,
            curve: Curves.easeOutBack,
          ),

          const SizedBox(height: spatialTheme.SpatialTheme.spaceLG),

          // Title
          Text(
            title,
            style: spatialTheme.SpatialTheme.textTheme.headlineSmall?.copyWith(
              fontWeight: FontWeight.bold,
              color: Colors.white,
            ),
            textAlign: TextAlign.center,
          ).animate().fadeIn(
            delay: 300.ms,
            duration: 600.ms,
          ),

          const SizedBox(height: spatialTheme.SpatialTheme.spaceMD),

          // Description
          Text(
            description,
            style: spatialTheme.SpatialTheme.textTheme.bodyMedium?.copyWith(
              color: Colors.white.withOpacity(0.7),
              height: 1.5,
            ),
            textAlign: TextAlign.center,
          ).animate().fadeIn(
            delay: 600.ms,
            duration: 600.ms,
          ),
        ],
      ),
    );
  }

  Widget _buildPageIndicator(int index) {
    bool isCurrentPage = index == _currentPage;
    
    return AnimatedContainer(
      duration: const Duration(milliseconds: 300),
      margin: const EdgeInsets.symmetric(horizontal: 5),
      height: isCurrentPage ? 12 : 8,
      width: isCurrentPage ? 30 : 8,
      decoration: BoxDecoration(
        color: isCurrentPage
            ? spatialTheme.SpatialTheme.primaryCyan
            : Colors.white.withOpacity(0.3),
        borderRadius: BorderRadius.circular(12),
      ),
    );
  }
}
