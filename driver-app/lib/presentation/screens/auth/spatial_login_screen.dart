import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/presentation/screens/dashboard/dashboard_screen_spatial.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:flutter_animate/flutter_animate.dart';

class SpatialLoginScreen extends StatefulWidget {
  const SpatialLoginScreen({Key? key}) : super(key: key);

  @override
  State<SpatialLoginScreen> createState() => _SpatialLoginScreenState();
}

class _SpatialLoginScreenState extends State<SpatialLoginScreen> {
  final _formKey = GlobalKey<FormState>();
  final _emailController = TextEditingController(text: 'driver@ktc.com');
  final _passwordController = TextEditingController(text: '123456');
  bool _obscurePassword = true;
  bool _rememberMe = false;

  @override
  void dispose() {
    _emailController.dispose();
    _passwordController.dispose();
    super.dispose();
  }

  void _handleLogin() {
    if (_formKey.currentState!.validate()) {
      context.read<AuthBloc>().add(
        LoginEvent(
          email: _emailController.text.trim(),
          password: _passwordController.text,
        ),
      );
    }
  }

  @override
  Widget build(BuildContext context) {
    return BlocListener<AuthBloc, AuthState>(
      listener: (context, state) {
        if (state is AuthenticatedState || state is AuthSuccessState) {
          Navigator.of(context).pushReplacement(
            MaterialPageRoute(
              builder: (context) => const DashboardScreenSpatial(),
            ),
          );
        } else if (state is AuthErrorState) {
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(
              content: Text(state.message),
              backgroundColor: SpatialTheme.error,
              behavior: SnackBarBehavior.floating,
              shape: RoundedRectangleBorder(
                borderRadius: SpatialTheme.borderRadiusMedium,
              ),
            ),
          );
        }
      },
      child: Scaffold(
        body: SpatialComponents.backgroundContainer(
          useDarkMode: true,
          child: SafeArea(
            child: SingleChildScrollView(
              padding: const EdgeInsets.symmetric(horizontal: SpatialTheme.spaceLG, vertical: SpatialTheme.spaceSM),
              physics: const BouncingScrollPhysics(),
              child: Column(
                children: [
                  const SizedBox(height: SpatialTheme.spaceSM),
                  
                  // Logo & Welcome Section
                  _buildHeader().animate().fadeIn(duration: 800.ms).slideY(
                    begin: -0.3,
                    duration: 800.ms,
                    curve: Curves.easeOutBack,
                  ),
                  
                  const SizedBox(height: SpatialTheme.space3XL),
                  
                  // Login Form
                  _buildLoginForm().animate().fadeIn(
                    delay: 300.ms,
                    duration: 600.ms,
                  ).slideY(
                    begin: 0.3,
                    duration: 600.ms,
                    curve: Curves.easeOutCubic,
                  ),
                  
                  const SizedBox(height: SpatialTheme.spaceXL),
                  
                  // Quick Login Buttons
                  _buildQuickLoginButtons().animate().fadeIn(
                    delay: 600.ms,
                    duration: 500.ms,
                  ),
                  
                  const SizedBox(height: SpatialTheme.spaceLG),
                  
                  // Footer
                  _buildFooter().animate().fadeIn(
                    delay: 900.ms,
                    duration: 500.ms,
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }

  Widget _buildHeader() {
    return Column(
      children: [
        // Logo Container
        SpatialComponents.spatialCard(
          padding: const EdgeInsets.all(SpatialTheme.spaceLG),
          useDarkMode: true,
          child: Column(
            children: [
              Container(
                width: 80,
                height: 80,
                decoration: BoxDecoration(
                  gradient: SpatialTheme.primaryGradient,
                  borderRadius: BorderRadius.circular(20),
                  boxShadow: SpatialTheme.glowShadow,
                ),
                child: const Icon(
                  FontAwesomeIcons.truck,
                  color: Colors.white,
                  size: 40,
                ),
              ),
              const SizedBox(height: SpatialTheme.spaceMD),
              Text(
                'KTC Logistics',
                style: SpatialTheme.textTheme.displayMedium?.copyWith(
                  fontWeight: FontWeight.w700,
                  color: Colors.white,
                ),
              ),
              Text(
                'Driver App',
                style: SpatialTheme.textTheme.titleMedium?.copyWith(
                  color: Colors.white.withOpacity(0.7),
                ),
              ),
            ],
          ),
        ),
        
        const SizedBox(height: SpatialTheme.spaceLG),
        
        Text(
          'Chào mừng trở lại!',
          style: SpatialTheme.textTheme.headlineMedium?.copyWith(
            fontWeight: FontWeight.w600,
            color: Colors.white,
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          'Đăng nhập để bắt đầu làm việc hôm nay',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withOpacity(0.7),
          ),
          textAlign: TextAlign.center,
        ),
      ],
    );
  }

  Widget _buildLoginForm() {
    return BlocBuilder<AuthBloc, AuthState>(
      builder: (context, state) {
        final isLoading = state is AuthLoadingState;
        
        return SpatialComponents.glassContainer(
          child: Form(
            key: _formKey,
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  'Thông tin đăng nhập',
                  style: SpatialTheme.textTheme.titleLarge?.copyWith(
                    fontWeight: FontWeight.w600,
                    color: SpatialTheme.textLight,
                  ),
                ),
                const SizedBox(height: SpatialTheme.spaceLG),
                
                // Email Field
                SpatialComponents.spatialTextField(
                  label: 'Email',
                  hint: 'Nhập email của bạn',
                  controller: _emailController,
                  prefixIcon: FontAwesomeIcons.envelope,
                  keyboardType: TextInputType.emailAddress,
                  enabled: !isLoading,
                  useDarkMode: true,
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
                
                // Password Field
                SpatialComponents.spatialTextField(
                  label: 'Mật khẩu',
                  hint: 'Nhập mật khẩu của bạn',
                  controller: _passwordController,
                  prefixIcon: FontAwesomeIcons.lock,
                  obscureText: _obscurePassword,
                  enabled: !isLoading,
                  useDarkMode: true,
                  suffixIcon: IconButton(
                    icon: Icon(
                      _obscurePassword 
                        ? FontAwesomeIcons.eyeSlash 
                        : FontAwesomeIcons.eye,
                      color: SpatialTheme.textTertiary,
                      size: 16,
                    ),
                    onPressed: () {
                      setState(() {
                        _obscurePassword = !_obscurePassword;
                      });
                    },
                  ),
                  validator: (value) {
                    if (value?.isEmpty ?? true) {
                      return 'Vui lòng nhập mật khẩu';
                    }
                    if (value!.length < 6) {
                      return 'Mật khẩu phải có ít nhất 6 ký tự';
                    }
                    return null;
                  },
                ),
                
                const SizedBox(height: SpatialTheme.spaceMD),
                
                // Remember Me & Forgot Password
                Row(
                  children: [
                    Theme(
                      data: ThemeData(
                        checkboxTheme: CheckboxThemeData(
                          fillColor: MaterialStateProperty.resolveWith<Color>(
                            (Set<MaterialState> states) {
                              if (states.contains(MaterialState.disabled)) {
                                return Colors.grey.withOpacity(.5);
                              }
                              return SpatialTheme.primaryBlue;
                            },
                          ),
                          shape: RoundedRectangleBorder(
                            borderRadius: BorderRadius.circular(4),
                          ),
                        ),
                      ),
                      child: Checkbox(
                        value: _rememberMe,
                        onChanged: isLoading ? null : (value) {
                          setState(() {
                            _rememberMe = value ?? false;
                          });
                        },
                        activeColor: SpatialTheme.primaryBlue,
                        checkColor: Colors.white,
                      ),
                    ),
                    Text(
                      'Ghi nhớ đăng nhập',
                      style: SpatialTheme.textTheme.bodyMedium?.copyWith(
                        color: Colors.white.withOpacity(0.9),
                      ),
                    ),
                    const Spacer(),
                    TextButton(
                      onPressed: isLoading ? null : () {
                        // TODO: Implement forgot password
                      },
                      child: Text(
                        'Quên mật khẩu?',
                        style: TextStyle(
                          color: SpatialTheme.primaryCyan,
                          fontWeight: FontWeight.w500,
                        ),
                      ),
                    ),
                  ],
                ),
                
                const SizedBox(height: SpatialTheme.spaceLG),
                
                // Login Button
                SizedBox(
                  width: double.infinity,
                  child: SpatialComponents.gradientButton(
                    text: 'Đăng nhập',
                    onPressed: _handleLogin,
                    loading: isLoading,
                    icon: FontAwesomeIcons.arrowRightToBracket,
                  ),
                ),
              ],
            ),
          ),
        );
      },
    );
  }

  Widget _buildQuickLoginButtons() {
    return SpatialComponents.glassContainer(
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'Đăng nhập nhanh',
            style: SpatialTheme.textTheme.titleMedium?.copyWith(
              fontWeight: FontWeight.w600,
              color: Colors.white,
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceMD),
          Text(
            'Chọn tài khoản demo để trải nghiệm ứng dụng',
            style: SpatialTheme.textTheme.bodySmall?.copyWith(
              color: Colors.white.withOpacity(0.7),
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceMD),
          
          Row(
            children: [
              Expanded(
                child: _buildQuickLoginCard(
                  name: 'Nguyễn Văn An',
                  email: 'driver@ktc.com',
                  vehicleType: 'Xe tải nhỏ',
                  avatar: 'https://images.unsplash.com/photo-1507003211169-0a1dd7228f2d?w=150&h=150&fit=crop&crop=face',
                  onTap: () {
                    _emailController.text = 'driver@ktc.com';
                    _passwordController.text = '123456';
                  },
                ),
              ),
              const SizedBox(width: SpatialTheme.spaceMD),
              Expanded(
                child: _buildQuickLoginCard(
                  name: 'Trần Thị Lan',
                  email: 'driver2@ktc.com',
                  vehicleType: 'Xe máy',
                  avatar: 'https://images.unsplash.com/photo-1494790108755-2616b332c108?w=150&h=150&fit=crop&crop=face',
                  onTap: () {
                    _emailController.text = 'driver2@ktc.com';
                    _passwordController.text = '123456';
                  },
                ),
              ),
            ],
          ),
        ],
      ),
    );
  }

  Widget _buildQuickLoginCard({
    required String name,
    required String email,
    required String vehicleType,
    required String avatar,
    required VoidCallback onTap,
  }) {
    return GestureDetector(
      onTap: onTap,
      child: Container(
        padding: const EdgeInsets.all(SpatialTheme.spaceMD),
        decoration: BoxDecoration(
          color: Colors.white.withOpacity(0.12),
          borderRadius: SpatialTheme.borderRadiusMedium,
          border: Border.all(
            color: Colors.white.withOpacity(0.15),
          ),
          boxShadow: [
            BoxShadow(
              color: Colors.black.withValues(alpha: 0.15),
              blurRadius: 8,
              offset: const Offset(0, 2),
            ),
          ],
        ),
        child: Column(
          children: [
            CircleAvatar(
              radius: 25,
              backgroundImage: NetworkImage(avatar),
            ),
            const SizedBox(height: SpatialTheme.spaceSM),
            Text(
              name,
              style: SpatialTheme.textTheme.titleSmall?.copyWith(
                fontWeight: FontWeight.w600,
                color: Colors.white,
              ),
              textAlign: TextAlign.center,
              maxLines: 1,
              overflow: TextOverflow.ellipsis,
            ),
            Text(
              vehicleType,
              style: SpatialTheme.textTheme.bodySmall?.copyWith(
                color: Colors.white.withOpacity(0.6),
              ),
              textAlign: TextAlign.center,
              maxLines: 1,
              overflow: TextOverflow.ellipsis,
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildFooter() {
    return Column(
      children: [
        Text(
          'KTC Logistics Driver App v1.0.0',
          style: SpatialTheme.textTheme.bodySmall?.copyWith(
            color: Colors.white.withOpacity(0.5),
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          '© 2025 KTC Logistics. All rights reserved.',
          style: SpatialTheme.textTheme.bodySmall?.copyWith(
            color: Colors.white.withOpacity(0.5),
          ),
          textAlign: TextAlign.center,
        ),
      ],
    );
  }
}


