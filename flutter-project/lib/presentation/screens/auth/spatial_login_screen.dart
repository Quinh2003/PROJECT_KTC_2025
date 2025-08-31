import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:flutter_animate/flutter_animate.dart';

class SpatialLoginScreen extends StatefulWidget {
  const SpatialLoginScreen({super.key});

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
          Navigator.of(context).pushReplacementNamed('/dashboard');
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
              padding: const EdgeInsets.symmetric(
                  horizontal: SpatialTheme.spaceLG,
                  vertical: SpatialTheme.spaceSM),
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

                  const SizedBox(height: SpatialTheme.spaceXL),

                  // Login Form
                  _buildLoginForm()
                      .animate()
                      .fadeIn(
                        delay: 300.ms,
                        duration: 600.ms,
                      )
                      .slideY(
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
          padding: const EdgeInsets.fromLTRB(0, SpatialTheme.spaceLG, 0, 0),
          useDarkMode: true,
          backgroundColor: SpatialTheme.primaryPurple.withValues(alpha: 0),
          child: Column(
            children: [
              Container(
                width: 80,
                height: 80,
                decoration: BoxDecoration(
                  gradient: SpatialTheme.accentGradient,
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
                  color: Colors.white.withValues(alpha: 0.7),
                ),
              ),
            ],
          ),
        ),
        
        const SizedBox(height: SpatialTheme.spaceMD),
        
        Text(
          'Welcome Back!',
          style: SpatialTheme.textTheme.headlineMedium?.copyWith(
            fontWeight: FontWeight.w600,
            color: Colors.white,
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          'Sign in to start your work today',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withValues(alpha: 0.7),
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
          child: Form(
            key: _formKey,
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                // Email Field
                SpatialComponents.spatialTextField(
                  label: 'Email',
                  hint: 'Enter your email',
                  controller: _emailController,
                  prefixIcon: FontAwesomeIcons.envelope,
                  keyboardType: TextInputType.emailAddress,
                  enabled: !isLoading,
                  useDarkMode: true,
                  validator: (value) {
                    if (value?.isEmpty ?? true) {
                      return 'Please enter your email';
                    }
                    if (!RegExp(r'^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$')
                        .hasMatch(value!)) {
                      return 'Invalid email format';
                    }
                    return null;
                  },
                ),

                const SizedBox(height: SpatialTheme.spaceLG),

                // Password Field
                SpatialComponents.spatialTextField(
                  label: 'Password',
                  hint: 'Enter your password',
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
                      return 'Please enter your password';
                    }
                    if (value!.length < 6) {
                      return 'Password must be at least 6 characters';
                    }
                    return null;
                  },
                ),

                const SizedBox(height: SpatialTheme.spaceMD),

                // Remember Me & Forgot Password
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Row(
                      mainAxisSize: MainAxisSize.min,
                      children: [
                        Theme(
                          data: ThemeData(
                            checkboxTheme: CheckboxThemeData(
                              fillColor:
                                  WidgetStateProperty.resolveWith<Color>(
                                (Set<WidgetState> states) {
                                  if (states.contains(WidgetState.disabled)) {
                                    return Colors.grey.withValues(alpha: .5);
                                  }
                                  return Colors.white;
                                },
                              ),
                              shape: RoundedRectangleBorder(
                                borderRadius: BorderRadius.circular(4),
                              ),
                            ),
                          ),
                          child: Checkbox(
                            value: _rememberMe,
                            onChanged: isLoading
                                ? null
                                : (value) {
                                    setState(() {
                                      _rememberMe = value ?? false;
                                    });
                                  },
                            activeColor: Colors.white,
                            checkColor: Color(0xFF1F2133),
                          ),
                        ),
                        Text(
                          'Remember Me',
                          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
                            color: Colors.white.withValues(alpha: 0.9),
                          ),
                        ),
                      ],
                    ),
                    TextButton(
                      onPressed: isLoading
                          ? null
                          : () {
                              Navigator.of(context).pushNamed('/forgot-password');
                            },
                      style: TextButton.styleFrom(
                        foregroundColor: Colors.white,
                      ),
                      child: Text(
                        'Forgot Password?',
                        style: TextStyle(
                          color: Colors.white,
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
                  child: ElevatedButton.icon(
                    icon: Icon(FontAwesomeIcons.arrowRightToBracket, size: 16),
                    label: Text(
                      isLoading ? 'Logging in...' : 'Log In',
                      style: SpatialTheme.textTheme.titleMedium?.copyWith(
                        fontWeight: FontWeight.w600,
                        color: Colors.black87,
                      ),
                    ),
                    onPressed: isLoading ? null : _handleLogin,
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
              ],
            ),
          ),
        );
      },
    );
  }

  Widget _buildQuickLoginButtons() {
    return Container(
      padding: const EdgeInsets.all(SpatialTheme.spaceMD),
      decoration: BoxDecoration(
        gradient: LinearGradient(
          begin: Alignment.topLeft,
          end: Alignment.bottomRight,
          colors: [
            Color(0xFF2A2D3E).withOpacity(0.75),
            Color(0xFF1F2133).withOpacity(0.9),
          ],
        ),
        borderRadius: SpatialTheme.borderRadiusMedium,
        border: Border.all(
          color: Colors.white.withOpacity(0.15),
          width: 1,
        ),
        boxShadow: [
          BoxShadow(
            color: Colors.black.withOpacity(0.2),
            blurRadius: 15,
            offset: const Offset(0, 5),
          ),
        ],
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'Quick Login',
            style: SpatialTheme.textTheme.titleMedium?.copyWith(
              fontWeight: FontWeight.w600,
              color: Colors.white,
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceMD),
          Text(
            'Select a demo account to experience the app',
            style: SpatialTheme.textTheme.bodySmall?.copyWith(
              color: Colors.white.withValues(alpha: 0.7),
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceMD),

          // Sử dụng Row để luôn hiển thị ngang
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              SizedBox(
                width: (MediaQuery.of(context).size.width -
                        SpatialTheme.spaceMD * 4) /
                    2.2,
                child: _buildQuickLoginCard(
                  name: 'John Smith',
                  email: 'driver_01@fr.com',
                  vehicleType: 'Small Truck',
                  avatar:
                      'https://images.unsplash.com/photo-1507003211169-0a1dd7228f2d?w=150&h=150&fit=crop&crop=face',
                  onTap: () {
                    _emailController.text = 'driver_01@fr.com';
                    _passwordController.text = '123456';
                  },
                ),
              ),
              SizedBox(
                width: (MediaQuery.of(context).size.width -
                        SpatialTheme.spaceMD * 4) /
                    2.2,
                child: _buildQuickLoginCard(
                  name: 'Sarah Johnson',
                  email: 'driver_offline@ktc.com',
                  vehicleType: 'Motorcycle',
                  avatar:
                      'https://images.unsplash.com/photo-1580489944761-15a19d654956?w=150&h=150&fit=crop&crop=face',
                  onTap: () {
                    _emailController.text = 'driver_offline@ktc.com';
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
          color: Colors.white.withValues(alpha: 0.12),
          borderRadius: SpatialTheme.borderRadiusMedium,
          border: Border.all(
            color: Colors.white.withValues(alpha: 0.15),
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
                color: Colors.white.withValues(alpha: 0.6),
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
            color: Colors.white.withValues(alpha: 0.5),
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          '© 2025 Phan Lê Việt Hùng. All rights reserved.',
          style: SpatialTheme.textTheme.bodySmall?.copyWith(
            color: Colors.white.withValues(alpha: 0.5),
          ),
          textAlign: TextAlign.center,
        ),
      ],
    );
  }
}
