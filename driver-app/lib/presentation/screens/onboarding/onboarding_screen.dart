import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/screens/login/login_screen.dart';

class OnboardingScreen extends StatefulWidget {
  const OnboardingScreen({Key? key}) : super(key: key);

  @override
  State<OnboardingScreen> createState() => _OnboardingScreenState();
}

class _OnboardingScreenState extends State<OnboardingScreen> {
  final PageController _pageController = PageController();
  int _currentPage = 0;
  
  final List<Map<String, dynamic>> _onboardingData = [
    {
      'title': 'Chào mừng đến với KTC Logistics',
      'description': 'Ứng dụng tài xế hiện đại giúp bạn quản lý đơn hàng, theo dõi lộ trình và giao hàng hiệu quả.',
      'image': 'Assets/svg/delivery.svg',
      'color': Colors.blue.shade500,
    },
    {
      'title': 'Theo dõi đơn hàng trực quan',
      'description': 'Dễ dàng nắm bắt thông tin đơn hàng, lịch trình và cập nhật trạng thái một cách nhanh chóng.',
      'image': 'Assets/svg/order-tracking.svg',
      'color': Colors.purple.shade500,
    },
    {
      'title': 'Tối ưu lộ trình',
      'description': 'Định tuyến thông minh giúp bạn tiết kiệm thời gian và nhiên liệu với lộ trình tối ưu nhất.',
      'image': 'Assets/svg/route-optimization.svg',
      'color': Colors.green.shade500,
    },
    {
      'title': 'Sẵn sàng bắt đầu?',
      'description': 'Đăng nhập ngay để bắt đầu sử dụng những tính năng tuyệt vời của KTC Logistics.',
      'image': 'Assets/svg/ready-to-go.svg',
      'color': Colors.orange.shade500,
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
  
  void _onNextTap() {
    if (_currentPage < _onboardingData.length - 1) {
      _pageController.nextPage(
        duration: const Duration(milliseconds: 300),
        curve: Curves.easeInOut,
      );
    } else {
      // Navigate to login screen
      Navigator.of(context).pushReplacement(
        MaterialPageRoute(builder: (_) => LoginScreen()),
      );
    }
  }
  
  void _onSkipTap() {
    // Navigate to login screen
    Navigator.of(context).pushReplacement(
      MaterialPageRoute(builder: (_) => LoginScreen()),
    );
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
              SpatialDesignSystem.primaryColor.withOpacity(0.1),
              SpatialDesignSystem.backgroundColor,
            ],
          ),
        ),
        child: SafeArea(
          child: Column(
            children: [
              // Skip button
              Align(
                alignment: Alignment.topRight,
                child: Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: _currentPage < _onboardingData.length - 1
                      ? SpatialButton(
                          text: 'Bỏ qua',
                          onPressed: _onSkipTap,
                          isOutlined: true,
                        )
                      : const SizedBox.shrink(),
                ),
              ),
              
              // Page content
              Expanded(
                child: PageView.builder(
                  controller: _pageController,
                  onPageChanged: _onPageChanged,
                  itemCount: _onboardingData.length,
                  itemBuilder: (context, index) {
                    return _buildPage(context, _onboardingData[index]);
                  },
                ),
              ),
              
              // Page indicator and next button
              Padding(
                padding: const EdgeInsets.all(24.0),
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    // Page indicator
                    Row(
                      children: List.generate(
                        _onboardingData.length,
                        (index) => _buildPageIndicator(index),
                      ),
                    ),
                    
                    // Next button
                    SpatialButton(
                      text: _currentPage < _onboardingData.length - 1
                          ? 'Tiếp tục'
                          : 'Bắt đầu',
                      onPressed: _onNextTap,
                      iconData: _currentPage < _onboardingData.length - 1
                          ? Icons.arrow_forward
                          : Icons.login,
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
  
  Widget _buildPage(BuildContext context, Map<String, dynamic> data) {
    final Color color = data['color'];
    
    return Padding(
      padding: const EdgeInsets.all(24.0),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          // Image
          Container(
            height: 240,
            width: 240,
            margin: const EdgeInsets.only(bottom: 40),
            decoration: BoxDecoration(
              color: color.withOpacity(0.1),
              shape: BoxShape.circle,
            ),
            child: Center(
              child: SvgPicture.asset(
                data['image'],
                height: 180,
                width: 180,
              ),
            ),
          ),
          
          // Title
          Text(
            data['title'],
            style: SpatialDesignSystem.headingLarge.copyWith(
              color: color,
              fontSize: 28,
            ),
            textAlign: TextAlign.center,
          ),
          
          const SizedBox(height: 16),
          
          // Description
          Text(
            data['description'],
            style: SpatialDesignSystem.bodyLarge,
            textAlign: TextAlign.center,
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
          duration: const Duration(milliseconds: 300),
          curve: Curves.easeInOut,
        );
      },
      child: AnimatedContainer(
        duration: const Duration(milliseconds: 300),
        margin: const EdgeInsets.symmetric(horizontal: 4),
        height: isActive ? 12 : 8,
        width: isActive ? 24 : 8,
        decoration: BoxDecoration(
          color: isActive ? color : Colors.grey.withOpacity(0.3),
          borderRadius: BorderRadius.circular(12),
        ),
      ),
    );
  }
}


