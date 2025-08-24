import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'dart:ui';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:flutter_animate/flutter_animate.dart';

class SpatialCheckEmailScreen extends StatelessWidget {
  final String email;
  
  const SpatialCheckEmailScreen({
    Key? key, 
    required this.email,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SpatialComponents.backgroundContainer(
        useDarkMode: true,
        child: SafeArea(
          child: SingleChildScrollView(
            padding: const EdgeInsets.symmetric(horizontal: SpatialTheme.spaceLG, vertical: SpatialTheme.spaceSM),
            physics: const BouncingScrollPhysics(),
            child: Column(
              children: [
                // Header
                _buildHeader(context).animate().fadeIn(duration: 600.ms).slideY(
                  begin: -0.3,
                  duration: 600.ms,
                  curve: Curves.easeOutBack,
                ),
                
                const SizedBox(height: SpatialTheme.space3XL),
                
                // Email Card
                _buildEmailCard().animate().fadeIn(
                  delay: 300.ms,
                  duration: 600.ms,
                ).slideY(
                  begin: 0.3,
                  duration: 600.ms,
                  curve: Curves.easeOutCubic,
                ),
                
                const SizedBox(height: SpatialTheme.space2XL),
                
                // Action Buttons
                _buildActionButtons(context).animate().fadeIn(
                  delay: 600.ms,
                  duration: 500.ms,
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }

  Widget _buildHeader(BuildContext context) {
    return Column(
      children: [
        // Icon
        SpatialComponents.spatialCard(
          width: 100,
          height: 100,
          useDarkMode: true,
          child: const Icon(
            FontAwesomeIcons.envelope,
            size: 40,
            color: SpatialTheme.primaryBlue,
          ),
        ),
        
        const SizedBox(height: SpatialTheme.spaceLG),
        
        Text(
          'Kiểm tra email của bạn',
          style: SpatialTheme.textTheme.headlineMedium?.copyWith(
            fontWeight: FontWeight.w600,
            color: Colors.white,
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          'Chúng tôi đã gửi hướng dẫn đặt lại mật khẩu đến email của bạn.',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withOpacity(0.7),
          ),
          textAlign: TextAlign.center,
        ),
      ],
    );
  }

  Widget _buildEmailCard() {
    return SpatialComponents.glassContainer(
      useDarkMode: true,
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'Email đã gửi đến',
            style: SpatialTheme.textTheme.titleLarge?.copyWith(
              fontWeight: FontWeight.w600,
              color: Colors.white,
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceMD),
          
          Container(
            padding: const EdgeInsets.all(SpatialTheme.spaceMD),
            decoration: BoxDecoration(
              color: SpatialTheme.surfaceColorLight.withOpacity(0.2),
              borderRadius: BorderRadius.circular(SpatialTheme.radiusMD),
              border: Border.all(
                color: SpatialTheme.borderColorLight.withOpacity(0.1),
              ),
            ),
            child: Row(
              children: [
                const Icon(
                  FontAwesomeIcons.envelope,
                  color: SpatialTheme.primaryBlue,
                  size: 20,
                ),
                const SizedBox(width: SpatialTheme.spaceMD),
                Expanded(
                  child: Text(
                    email,
                    style: SpatialTheme.textTheme.bodyLarge?.copyWith(
                      fontWeight: FontWeight.w500,
                    ),
                  ),
                ),
              ],
            ),
          ),
          
          const SizedBox(height: SpatialTheme.spaceLG),
          
          Text(
            'Không nhận được email?',
            style: SpatialTheme.textTheme.bodyMedium?.copyWith(
              fontWeight: FontWeight.w500,
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceSM),
          Text(
            'Vui lòng kiểm tra thư mục Spam hoặc thử gửi lại sau 60 giây.',
            style: SpatialTheme.textTheme.bodySmall,
          ),
          
          const SizedBox(height: SpatialTheme.spaceLG),
          
          // Resend Button
          SizedBox(
            width: double.infinity,
            child: OutlinedButton(
              onPressed: () {
                // Implement resend logic
              },
              style: OutlinedButton.styleFrom(
                side: const BorderSide(color: SpatialTheme.primaryBlue),
                padding: const EdgeInsets.symmetric(vertical: SpatialTheme.spaceMD),
                shape: RoundedRectangleBorder(
                  borderRadius: BorderRadius.circular(SpatialTheme.radiusMD),
                ),
              ),
              child: const Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Icon(FontAwesomeIcons.rotateRight, size: 16),
                  SizedBox(width: SpatialTheme.spaceSM),
                  Text('Gửi lại email'),
                ],
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildActionButtons(BuildContext context) {
    return Column(
      children: [
        SizedBox(
          width: double.infinity,
          child: SpatialComponents.gradientButton(
            text: 'Trở về đăng nhập',
            onPressed: () => Navigator.of(context).pushReplacement(
              MaterialPageRoute(builder: (_) => const SpatialLoginScreen()),
            ),
            icon: FontAwesomeIcons.arrowLeft,
          ),
        ),
        
        const SizedBox(height: SpatialTheme.spaceMD),
        
        Row(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            const Icon(
              FontAwesomeIcons.circleQuestion,
              size: 16,
              color: SpatialTheme.textColorSecondary,
            ),
            const SizedBox(width: SpatialTheme.spaceSM),
            Text(
              'Cần hỗ trợ? ',
              style: SpatialTheme.textTheme.bodyMedium?.copyWith(
                color: SpatialTheme.textColorSecondary,
              ),
            ),
            TextButton(
              onPressed: () {
                // Implement support action
              },
              child: Text(
                'Liên hệ hỗ trợ',
                style: TextStyle(
                  color: SpatialTheme.primaryBlue,
                  fontWeight: FontWeight.w600,
                ),
              ),
            ),
          ],
        ),
      ],
    );
  }
}
