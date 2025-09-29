import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:flutter_animate/flutter_animate.dart';

class SpatialCheckEmailScreen extends StatelessWidget {
  final String email;
  
  const SpatialCheckEmailScreen({
    super.key, 
    required this.email,
  });

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
          'Check Your Email',
          style: SpatialTheme.textTheme.headlineMedium?.copyWith(
            fontWeight: FontWeight.w600,
            color: Colors.white,
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          'We have sent password reset instructions to your email.',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withValues(alpha: 0.7),
          ),
          textAlign: TextAlign.center,
        ),
      ],
    );
  }

  Widget _buildEmailCard() {
    return Container(
      padding: const EdgeInsets.all(SpatialTheme.spaceMD),
      decoration: BoxDecoration(
        gradient: LinearGradient(
          begin: Alignment.topLeft,
          end: Alignment.bottomRight,
          colors: [
            Color(0xFF2A2D3E).withValues(alpha: 0.75),
            Color(0xFF1F2133).withValues(alpha: 0.9),
          ],
        ),
        borderRadius: SpatialTheme.borderRadiusMedium,
        border: Border.all(
          color: Colors.white.withValues(alpha: 0.15),
          width: 1,
        ),
        boxShadow: [
          BoxShadow(
            color: Colors.black.withValues(alpha: 0.2),
            blurRadius: 15,
            offset: const Offset(0, 5),
          ),
        ],
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'Email Sent To',
            style: SpatialTheme.textTheme.titleLarge?.copyWith(
              fontWeight: FontWeight.w600,
              color: Colors.white,
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceMD),
          
          Container(
            padding: const EdgeInsets.all(SpatialTheme.spaceMD),
            decoration: BoxDecoration(
              color: SpatialTheme.surfaceColorLight.withValues(alpha: 0.2),
              borderRadius: BorderRadius.circular(SpatialTheme.radiusMD),
              border: Border.all(
                color: SpatialTheme.borderColorLight.withValues(alpha: 0.1),
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
            'Did not receive the email?',
            style: SpatialTheme.textTheme.bodyMedium?.copyWith(
              fontWeight: FontWeight.w500,
              color: Colors.white,
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceSM),
          Text(
            'Please check your spam folder or try resending after 60 seconds.',
            style: SpatialTheme.textTheme.bodySmall?.copyWith(
              color: Colors.white.withValues(alpha: 0.7),
            ),
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
                side: const BorderSide(color: Colors.white),
                padding: const EdgeInsets.symmetric(vertical: SpatialTheme.spaceMD),
                foregroundColor: Colors.white,
                shape: RoundedRectangleBorder(
                  borderRadius: BorderRadius.circular(SpatialTheme.radiusMD),
                ),
              ),
              child: const Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Icon(FontAwesomeIcons.rotateRight, size: 16),
                  SizedBox(width: SpatialTheme.spaceSM),
                  Text('Resend Email'),
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
          child: ElevatedButton.icon(
            icon: Icon(FontAwesomeIcons.arrowLeft, size: 16),
            label: Text(
              'Back to Login',
              style: SpatialTheme.textTheme.titleMedium?.copyWith(
                fontWeight: FontWeight.w600,
                color: Colors.black87,
              ),
            ),
            onPressed: () => Navigator.of(context).pushReplacement(
              MaterialPageRoute(builder: (_) => const SpatialLoginScreen()),
            ),
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.white,
              foregroundColor: Colors.black87,
              padding: const EdgeInsets.symmetric(vertical: 16),
              shape: RoundedRectangleBorder(
                borderRadius: SpatialTheme.borderRadiusMedium,
              ),
              elevation: 0,
            ),
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
              'Need help? ',
              style: SpatialTheme.textTheme.bodyMedium?.copyWith(
                color: Colors.white.withValues(alpha: 0.7),
              ),
            ),
            TextButton(
              onPressed: () {
                // Implement support action
              },
              style: TextButton.styleFrom(
                foregroundColor: Colors.white,
              ),
              child: Text(
                'Contact Support',
                style: TextStyle(
                  color: Colors.white,
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
