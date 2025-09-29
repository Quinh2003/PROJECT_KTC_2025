import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'dart:ui';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/screens/environment/environment_selection_screen.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_background.dart';

class OnboardingScreen extends StatefulWidget {
  const OnboardingScreen({super.key});

  @override
  State<OnboardingScreen> createState() => _OnboardingScreenState();
}

class _OnboardingScreenState extends State<OnboardingScreen> {
  final PageController _pageController = PageController();
  int _currentPage = 0;
  
  final List<Map<String, dynamic>> _onboardingData = [
    {
      'title': 'Welcome to\nFastRoute',
      'description': 'Next-generation delivery app with modern Spatial UI, designed specifically for KTC Logistics drivers.',
      'image': 'assets/svg/delivery.svg',
      'color': const Color(0xFF0A84FF), // Apple Vision Pro blue
    },
    {
      'title': 'Smart\nOrder Management',
      'description': 'Receive real-time orders, view details, update statuses, and manage delivery history with ease.',
      'image': 'assets/svg/order-tracking.svg',
      'color': const Color(0xFF32D74B), // Apple Vision Pro green
    },
    {
      'title': 'Maps & Navigation\nwith AI',
      'description': 'Integrated Mapbox with GPS tracking, real-time route optimization, and clear pickup/delivery positions.',
      'image': 'assets/svg/route-optimization.svg',
      'color': const Color(0xFF5E5CE6), // Apple Vision Pro indigo
    },
    {
      'title': 'Ready to get started?',
      'description': 'Log in now to experience the most efficient delivery app for professional drivers.',
      'image': 'assets/svg/ready-to-go.svg',
      'color': const Color(0xFFFF9F0A), // Apple Vision Pro blue
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
  
  Future<void> _onNextTap() async {
    if (_currentPage < _onboardingData.length - 1) {
      _pageController.nextPage(
        duration: const Duration(milliseconds: 300),
        curve: Curves.easeInOut,
      );
    } else {
      // Lưu trạng thái onboarding đã hoàn thành
      const storage = FlutterSecureStorage();
      await storage.write(key: 'onboarding_completed', value: 'true');
      
      // Chuyển đến màn hình lựa chọn môi trường
      if (mounted) {
        Navigator.of(context).pushReplacementNamed('/environment');
      }
    }
  }
  
  Future<void> _onSkipTap() async {
    // Lưu trạng thái onboarding đã hoàn thành
    const storage = FlutterSecureStorage();
    await storage.write(key: 'onboarding_completed', value: 'true');
    
    // Navigate to environment selection screen
    if (mounted) {
      Navigator.of(context).pushReplacementNamed('/environment');
    }
  }
  
  @override
  Widget build(BuildContext context) {
    final Color currentColor = _onboardingData[_currentPage]['color'];
    final bool isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Scaffold(
      backgroundColor: isDark ? const Color(0xFF000000) : const Color(0xFFF2F2F7),
      body: SpatialBackground(
        color: currentColor,
        opacity: 0.05,
        child: SafeArea(
          child: Column(
            children: [
              // Logo và thông tin phiên bản
              Padding(
                padding: const EdgeInsets.only(top: 16.0, bottom: 0),
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    // Logo
                    Container(
                      height: 40,
                      width: 40,
                      padding: const EdgeInsets.all(8),
                      decoration: BoxDecoration(
                        color: currentColor.withValues(alpha: 0.2),
                        shape: BoxShape.circle,
                      ),
                      child: Image.asset(
                        'assets/logo/ktc_logo.png',
                        fit: BoxFit.contain,
                        errorBuilder: (context, error, stackTrace) => 
                          Icon(Icons.local_shipping, size: 24, color: currentColor),
                      ),
                    ),
                    const SizedBox(width: 8),
                    Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          'KTC FastRoute',
                          style: TextStyle(
                            fontWeight: FontWeight.bold,
                            fontSize: 16,
                            color: currentColor,
                          ),
                        ),
                        Text(
                          'v3.32.8 - Driver App',
                          style: TextStyle(
                            fontSize: 12,
                            color: Colors.grey.shade600,
                          ),
                        ),
                      ],
                    ),
                  ],
                ),
              ),
              // Skip button with improved animation
              AnimatedOpacity(
                duration: const Duration(milliseconds: 300),
                opacity: _currentPage < _onboardingData.length - 1 ? 1.0 : 0.0,
                child: Align(
                  alignment: Alignment.topRight,
                  child: Padding(
                    padding: const EdgeInsets.all(16.0),
                    child: _currentPage < _onboardingData.length - 1
                        ? TextButton(
                            onPressed: _onSkipTap,
                            style: TextButton.styleFrom(
                              foregroundColor: Colors.grey.shade700,
                              padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
                              shape: RoundedRectangleBorder(
                                borderRadius: BorderRadius.circular(30),
                              ),
                            ),
                            child: Row(
                              mainAxisSize: MainAxisSize.min,
                              children: [
                                Text(
                                  'Skip',
                                  style: TextStyle(
                                    fontWeight: FontWeight.w500,
                                    fontSize: 14,
                                  ),
                                ),
                                const SizedBox(width: 6),
                                Icon(Icons.skip_next, size: 16),
                              ],
                            ),
                          )
                        : const SizedBox.shrink(),
                  ),
                ),
              ),
              
              // Page content with improved PageView
              Expanded(
                child: PageView.builder(
                  controller: _pageController,
                  onPageChanged: _onPageChanged,
                  itemCount: _onboardingData.length,
                  physics: const BouncingScrollPhysics(),
                  itemBuilder: (context, index) {
                    return _buildPage(context, _onboardingData[index]);
                  },
                ),
              ),
              
              // Enhanced page indicator and next button
              ClipRRect(
                borderRadius: const BorderRadius.vertical(top: Radius.circular(24)),
                child: BackdropFilter(
                  filter: ImageFilter.blur(sigmaX: 10, sigmaY: 10),
                  child: Container(
                    padding: const EdgeInsets.symmetric(horizontal: 24.0, vertical: 20.0),
                    decoration: BoxDecoration(
                      color: isDark 
                          ? Colors.black.withOpacity(0.6) 
                          : Colors.white.withOpacity(0.7),
                      borderRadius: const BorderRadius.vertical(top: Radius.circular(24)),
                      border: Border(
                        top: BorderSide(
                          color: isDark 
                              ? Colors.white.withOpacity(0.1) 
                              : Colors.white.withOpacity(0.5),
                          width: 0.5,
                        ),
                      ),
                      boxShadow: [
                        BoxShadow(
                          color: Colors.black.withOpacity(0.05),
                          blurRadius: 10,
                          offset: const Offset(0, -5),
                        ),
                      ],
                    ),
                    child: Row(
                      mainAxisAlignment: MainAxisAlignment.spaceBetween,
                      children: [
                        // Page indicator with enhanced visual
                        Row(
                          children: List.generate(
                            _onboardingData.length,
                            (index) => _buildPageIndicator(index),
                          ),
                        ),
                        
                        // Next button with animations
                        AnimatedContainer(
                          duration: const Duration(milliseconds: 300),
                          decoration: BoxDecoration(
                            color: _onboardingData[_currentPage]['color'],
                            borderRadius: BorderRadius.circular(30),
                            boxShadow: [
                              BoxShadow(
                                color: _onboardingData[_currentPage]['color'].withOpacity(0.4),
                                blurRadius: 8,
                                offset: const Offset(0, 4),
                              ),
                            ],
                          ),
                          child: Material(
                            color: Colors.transparent,
                            child: InkWell(
                              onTap: _onNextTap,
                              borderRadius: BorderRadius.circular(30),
                              child: Padding(
                                padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 12),
                                child: Row(
                                  mainAxisSize: MainAxisSize.min,
                                  children: [
                                    Text(
                                      _currentPage < _onboardingData.length - 1 ? 'Continue' : 'Get Started',
                                      style: const TextStyle(
                                        color: Colors.white,
                                        fontWeight: FontWeight.bold,
                                        fontSize: 16,
                                      ),
                                    ),
                                    const SizedBox(width: 8),
                                    Icon(
                                      _currentPage < _onboardingData.length - 1
                                          ? Icons.arrow_forward
                                          : Icons.login,
                                      color: Colors.white,
                                    ),
                                  ],
                                ),
                              ),
                            ),
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
  
  Widget _buildPage(context, Map<String, dynamic> data) {
    final Color color = data['color'];
    
    return Padding(
      padding: const EdgeInsets.all(24.0),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          // Image with enhanced animation
          TweenAnimationBuilder(
            duration: const Duration(milliseconds: 800),
            tween: Tween<double>(begin: 0.8, end: 1.0),
            curve: Curves.elasticOut,
            builder: (context, value, child) {
              return Transform.scale(
                scale: value,
                child: Container(
                  height: 240,
                  width: 240,
                  margin: const EdgeInsets.only(bottom: 30),
                  child: Stack(
                    alignment: Alignment.center,
                    children: [
                      // Blur background glow
                      Container(
                        height: 220,
                        width: 220,
                        decoration: BoxDecoration(
                          shape: BoxShape.circle,
                          gradient: RadialGradient(
                            colors: [
                              color.withOpacity(0.3),
                              color.withOpacity(0.0),
                            ],
                            stops: const [0.2, 1.0],
                          ),
                        ),
                      ),
                      
                      // Glass container for image
                      ClipRRect(
                        borderRadius: BorderRadius.circular(120),
                        child: BackdropFilter(
                          filter: ImageFilter.blur(sigmaX: 15, sigmaY: 15),
                          child: Container(
                            height: 220,
                            width: 220,
                            decoration: BoxDecoration(
                              shape: BoxShape.circle,
                              color: Theme.of(context).brightness == Brightness.dark
                                  ? color.withOpacity(0.15)
                                  : Colors.white.withOpacity(0.6),
                              border: Border.all(
                                color: Theme.of(context).brightness == Brightness.dark
                                    ? Colors.white.withOpacity(0.1)
                                    : Colors.white.withOpacity(0.7),
                                width: 1.5,
                              ),
                              boxShadow: [
                                BoxShadow(
                                  color: color.withOpacity(0.2),
                                  blurRadius: 30,
                                  spreadRadius: 0,
                                  offset: const Offset(0, 10),
                                ),
                              ],
                            ),
                            child: Center(
                              child: SvgPicture.asset(
                                data['image'],
                                height: 150,
                                width: 150,
                                colorFilter: ColorFilter.mode(
                                  color.withOpacity(0.9),
                                  BlendMode.srcIn,
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                      
                      // Subtle animation effect
                      AnimatedPositioned(
                        duration: const Duration(milliseconds: 2000),
                        curve: Curves.easeInOut,
                        top: value > 0.95 ? 5 : 0,
                        child: Container(
                          height: 220,
                          width: 220,
                          decoration: BoxDecoration(
                            shape: BoxShape.circle,
                            border: Border.all(
                              color: Colors.white.withOpacity(0.3),
                              width: 2,
                            ),
                          ),
                        ),
                      ),
                    ],
                  ),
                ),
              );
            },
          ),
          
          // Title with animation
          TweenAnimationBuilder(
            duration: const Duration(milliseconds: 600),
            tween: Tween<double>(begin: 0.0, end: 1.0),
            curve: Curves.easeOutCubic,
            builder: (context, value, child) {
              return Opacity(
                opacity: value,
                child: Transform.translate(
                  offset: Offset(0, 20 * (1 - value)),
                  child: ShaderMask(
                    shaderCallback: (Rect bounds) {
                      return LinearGradient(
                        begin: Alignment.topCenter,
                        end: Alignment.bottomCenter,
                        colors: [
                          color,
                          color.withOpacity(0.8),
                        ],
                      ).createShader(bounds);
                    },
                    child: Text(
                      data['title'],
                      style: SpatialDesignSystem.headingLarge.copyWith(
                        color: Colors.white,
                        fontSize: 28,
                        fontWeight: FontWeight.bold,
                        letterSpacing: -0.5,
                        shadows: [
                          Shadow(
                            color: color.withOpacity(0.5),
                            blurRadius: 8,
                            offset: const Offset(0, 2),
                          ),
                        ],
                      ),
                      textAlign: TextAlign.center,
                    ),
                  ),
                ),
              );
            },
          ),
          
          const SizedBox(height: 20),
          
          // Description with animation
          TweenAnimationBuilder(
            duration: const Duration(milliseconds: 800),
            tween: Tween<double>(begin: 0.0, end: 1.0),
            curve: Curves.easeOutCubic,
            builder: (context, value, child) {
              return Opacity(
                opacity: value,
                child: Transform.translate(
                  offset: Offset(0, 30 * (1 - value)),
                  child: ClipRRect(
                    borderRadius: BorderRadius.circular(16),
                    child: BackdropFilter(
                      filter: ImageFilter.blur(sigmaX: 8, sigmaY: 8),
                      child: Container(
                        padding: const EdgeInsets.all(16),
                        decoration: BoxDecoration(
                          color: Theme.of(context).brightness == Brightness.dark
                              ? color.withOpacity(0.1)
                              : Colors.white.withOpacity(0.7),
                          borderRadius: BorderRadius.circular(16),
                          border: Border.all(
                            color: Theme.of(context).brightness == Brightness.dark
                                ? Colors.white.withOpacity(0.1)
                                : Colors.white.withOpacity(0.5),
                            width: 0.5,
                          ),
                          boxShadow: [
                            BoxShadow(
                              color: color.withOpacity(0.2),
                              blurRadius: 15,
                              spreadRadius: 0,
                              offset: const Offset(0, 5),
                            ),
                          ],
                        ),
                        child: Text(
                          data['description'],
                          style: SpatialDesignSystem.bodyLarge.copyWith(
                            color: Theme.of(context).brightness == Brightness.dark
                                ? Colors.white.withOpacity(0.9)
                                : Colors.black.withOpacity(0.8),
                            height: 1.4,
                          ),
                          textAlign: TextAlign.center,
                        ),
                      ),
                    ),
                  ),
                ),
              );
            },
          ),
        ],
      ),
    );
  }
  

  
  Widget _buildPageIndicator(int index) {
    final Color color = _onboardingData[index]['color'];
    final bool isActive = index == _currentPage;
    
    return GestureDetector(
      onTap: () {
        _pageController.animateToPage(
          index,
          duration: const Duration(milliseconds: 500),
          curve: Curves.easeOutCubic,
        );
      },
      child: AnimatedContainer(
        duration: const Duration(milliseconds: 400),
        margin: const EdgeInsets.symmetric(horizontal: 4),
        height: 8,
        width: isActive ? 30 : 8,
        decoration: BoxDecoration(
          color: isActive ? color : Colors.grey.withValues(alpha: 0.3),
          borderRadius: BorderRadius.circular(12),
          boxShadow: isActive 
              ? [
                  BoxShadow(
                    color: color.withValues(alpha: 0.4),
                    blurRadius: 6,
                    spreadRadius: 1,
                  )
                ] 
              : null,
        ),
        child: isActive 
            ? Center(
                child: AnimatedOpacity(
                  duration: const Duration(milliseconds: 200),
                  opacity: isActive ? 1.0 : 0.0,
                  child: Container(
                    height: 4,
                    width: 4,
                    decoration: BoxDecoration(
                      color: Colors.white,
                      shape: BoxShape.circle,
                    ),
                  ),
                ),
              )
            : null,
      ),
    );
  }
}


