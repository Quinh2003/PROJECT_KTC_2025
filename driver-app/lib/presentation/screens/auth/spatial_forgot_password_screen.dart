import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'dart:ui';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_check_email_screen.dart';
import 'package:flutter_animate/flutter_animate.dart';

class SpatialForgotPasswordScreen extends StatefulWidget {
  const SpatialForgotPasswordScreen({Key? key}) : super(key: key);

  @override
  State<SpatialForgotPasswordScreen> createState() => _SpatialForgotPasswordScreenState();
}

class _SpatialForgotPasswordScreenState extends State<SpatialForgotPasswordScreen> {
  final _formKey = GlobalKey<FormState>();
  final _emailController = TextEditingController();
  bool _isLoading = false;

  @override
  void dispose() {
    _emailController.dispose();
    super.dispose();
  }

  void _handleResetPassword() {
    if (_formKey.currentState!.validate()) {
      setState(() => _isLoading = true);
      
      // Simulate API call
      Future.delayed(const Duration(seconds: 2), () {
        setState(() => _isLoading = false);
        Navigator.of(context).pushReplacement(
          MaterialPageRoute(
            builder: (context) => SpatialCheckEmailScreen(email: _emailController.text.trim()),
          ),
        );
      });
    }
  }

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
                _buildHeader().animate().fadeIn(duration: 600.ms).slideY(
                  begin: -0.3,
                  duration: 600.ms,
                  curve: Curves.easeOutBack,
                ),
                
                const SizedBox(height: SpatialTheme.space3XL),
                
                // Reset Form
                _buildResetForm().animate().fadeIn(
                  delay: 300.ms,
                  duration: 600.ms,
                ).slideY(
                  begin: 0.3,
                  duration: 600.ms,
                  curve: Curves.easeOutCubic,
                ),
                
                const SizedBox(height: SpatialTheme.space2XL),
                
                // Footer
                _buildFooter().animate().fadeIn(
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

  Widget _buildHeader() {
    return Column(
      children: [
        // Back Button
        Align(
          alignment: Alignment.centerLeft,
          child: SpatialComponents.iconButton(
            icon: FontAwesomeIcons.arrowLeft,
            onPressed: () => Navigator.of(context).pushReplacement(
              MaterialPageRoute(builder: (context) => const SpatialLoginScreen()),
            ),
          ),
        ),
        
        const SizedBox(height: SpatialTheme.spaceLG),
        
        // Icon
        SpatialComponents.spatialCard(
          width: 100,
          height: 100,
          useDarkMode: true,
          child: const Icon(
            FontAwesomeIcons.key,
            size: 40,
            color: SpatialTheme.primaryBlue,
          ),
        ),
        
        const SizedBox(height: SpatialTheme.spaceLG),
        
        Text(
          'Quên mật khẩu?',
          style: SpatialTheme.textTheme.headlineMedium?.copyWith(
            fontWeight: FontWeight.w600,
            color: Colors.white,
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          'Đừng lo lắng! Vui lòng nhập email của bạn và chúng tôi sẽ gửi cho bạn hướng dẫn đặt lại mật khẩu.',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withOpacity(0.7),
          ),
          textAlign: TextAlign.center,
        ),
      ],
    );
  }

  Widget _buildResetForm() {
    return SpatialComponents.glassContainer(
      useDarkMode: true,
      child: Form(
        key: _formKey,
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Đặt lại mật khẩu',
              style: SpatialTheme.textTheme.titleLarge?.copyWith(
                fontWeight: FontWeight.w600,
                color: Colors.white,
              ),
            ),
            const SizedBox(height: SpatialTheme.spaceLG),
            
            // Email Field
            SpatialComponents.spatialTextField(
              useDarkMode: true,
              label: 'Email',
              hint: 'Nhập email của bạn',
              controller: _emailController,
              prefixIcon: FontAwesomeIcons.envelope,
              keyboardType: TextInputType.emailAddress,
              enabled: !_isLoading,
              validator: (value) {
                if (value?.isEmpty ?? true) {
                  return 'Vui lòng nhập email';
                }
                if (!RegExp(r'^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$').hasMatch(value!)) {
                  return 'Email không hợp lệ';
                }
                return null;
              },
            ),
            
            const SizedBox(height: SpatialTheme.spaceLG),
            
            // Reset Button
            SizedBox(
              width: double.infinity,
              child: SpatialComponents.gradientButton(
                text: 'Gửi hướng dẫn đặt lại',
                onPressed: _handleResetPassword,
                loading: _isLoading,
                icon: FontAwesomeIcons.paperPlane,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildFooter() {
    return Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        Text(
          'Nhớ mật khẩu? ',
          style: SpatialTheme.textTheme.bodyMedium,
        ),
        TextButton(
          onPressed: () => Navigator.of(context).pushReplacement(
            MaterialPageRoute(builder: (_) => const SpatialLoginScreen()),
          ),
          child: Text(
            'Đăng nhập',
            style: TextStyle(
              color: SpatialTheme.primaryBlue,
              fontWeight: FontWeight.w600,
            ),
          ),
        ),
      ],
    );
  }
}
