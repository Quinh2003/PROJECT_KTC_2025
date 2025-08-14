import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'dart:ui';

import '../design/spatial_ui.dart';

class LoginScreen extends StatefulWidget {
  const LoginScreen({Key? key}) : super(key: key);

  @override
  State<LoginScreen> createState() => _LoginScreenState();
}

class _LoginScreenState extends State<LoginScreen> with SingleTickerProviderStateMixin {
  final TextEditingController _emailController = TextEditingController();
  final TextEditingController _passwordController = TextEditingController();
  final _formKey = GlobalKey<FormState>();
  bool _isLoading = false;
  bool _obscurePassword = true;
  late AnimationController _animationController;
  late Animation<double> _fadeAnimation;
  late Animation<Offset> _slideAnimation;

  @override
  void initState() {
    super.initState();
    _animationController = AnimationController(
      vsync: this,
      duration: const Duration(milliseconds: 800),
    );
    
    _fadeAnimation = Tween<double>(begin: 0.0, end: 1.0).animate(
      CurvedAnimation(
        parent: _animationController,
        curve: Curves.easeIn,
      ),
    );
    
    _slideAnimation = Tween<Offset>(
      begin: const Offset(0, 0.5),
      end: Offset.zero,
    ).animate(
      CurvedAnimation(
        parent: _animationController,
        curve: Curves.easeOut,
      ),
    );
    
    _animationController.forward();
  }
  
  @override
  void dispose() {
    _emailController.dispose();
    _passwordController.dispose();
    _animationController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    final size = MediaQuery.of(context).size;
    
    return Scaffold(
      backgroundColor: isDark
          ? SpatialDesignSystem.darkBackgroundColor
          : SpatialDesignSystem.backgroundColor,
      body: Stack(
        children: [
          // Background elements
          Positioned(
            top: -100,
            right: -100,
            child: Container(
              width: 300,
              height: 300,
              decoration: BoxDecoration(
                shape: BoxShape.circle,
                gradient: RadialGradient(
                  colors: [
                    SpatialDesignSystem.primaryColor.withOpacity(0.3),
                    SpatialDesignSystem.primaryColor.withOpacity(0.0),
                  ],
                ),
              ),
            ),
          ),
          Positioned(
            bottom: -150,
            left: -100,
            child: Container(
              width: 400,
              height: 400,
              decoration: BoxDecoration(
                shape: BoxShape.circle,
                gradient: RadialGradient(
                  colors: [
                    SpatialDesignSystem.accentColor.withOpacity(0.2),
                    SpatialDesignSystem.accentColor.withOpacity(0.0),
                  ],
                ),
              ),
            ),
          ),
          
          // Main content
          Center(
            child: SingleChildScrollView(
              padding: const EdgeInsets.all(24.0),
              child: FadeTransition(
                opacity: _fadeAnimation,
                child: SlideTransition(
                  position: _slideAnimation,
                  child: Column(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      // Logo and App Name
                      Container(
                        width: 100,
                        height: 100,
                        decoration: BoxDecoration(
                          color: SpatialDesignSystem.primaryColor,
                          borderRadius: BorderRadius.circular(20),
                          boxShadow: [
                            BoxShadow(
                              color: SpatialDesignSystem.primaryColor.withOpacity(0.3),
                              blurRadius: 20,
                              offset: const Offset(0, 10),
                            ),
                          ],
                        ),
                        child: const Center(
                          child: Text(
                            "KTC",
                            style: TextStyle(
                              color: Colors.white,
                              fontSize: 32,
                              fontWeight: FontWeight.bold,
                            ),
                          ),
                        ),
                      ),
                      const SizedBox(height: 24),
                      Text(
                        "KTC Logistics",
                        style: SpatialDesignSystem.headingLarge.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                        ),
                      ),
                      const SizedBox(height: 8),
                      Text(
                        "Driver App",
                        style: SpatialDesignSystem.subtitleLarge.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkSecondaryColor
                              : SpatialDesignSystem.textSecondaryColor,
                        ),
                      ),
                      const SizedBox(height: 40),
                      
                      // Login Form
                      GlassCard(
                        width: size.width > 600 ? 500 : null,
                        padding: const EdgeInsets.all(24),
                        child: Form(
                          key: _formKey,
                          child: Column(
                            crossAxisAlignment: CrossAxisAlignment.start,
                            children: [
                              Text(
                                "Log In",
                                style: SpatialDesignSystem.headingMedium.copyWith(
                                  color: isDark
                                      ? SpatialDesignSystem.textDarkPrimaryColor
                                      : SpatialDesignSystem.textPrimaryColor,
                                ),
                              ),
                              const SizedBox(height: 8),
                              Text(
                                "Please sign in to continue",
                                style: SpatialDesignSystem.bodyMedium.copyWith(
                                  color: isDark
                                      ? SpatialDesignSystem.textDarkSecondaryColor
                                      : SpatialDesignSystem.textSecondaryColor,
                                ),
                              ),
                              const SizedBox(height: 24),
                              
                              // Email Field
                              SpatialTextField(
                                label: "Email",
                                hint: "Enter your email",
                                controller: _emailController,
                                keyboardType: TextInputType.emailAddress,
                                prefix: const Icon(Icons.email_outlined),
                                validator: (value) {
                                  if (value == null || value.isEmpty) {
                                    return 'Please enter your email';
                                  }
                                  if (!RegExp(r'^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$').hasMatch(value)) {
                                    return 'Please enter a valid email';
                                  }
                                  return null;
                                },
                                isGlass: true,
                              ),
                              const SizedBox(height: 16),
                              
                              // Password Field
                              SpatialTextField(
                                label: "Password",
                                hint: "Enter your password",
                                controller: _passwordController,
                                obscureText: _obscurePassword,
                                prefix: const Icon(Icons.lock_outline),
                                suffix: IconButton(
                                  icon: Icon(
                                    _obscurePassword
                                        ? Icons.visibility_outlined
                                        : Icons.visibility_off_outlined,
                                  ),
                                  onPressed: () {
                                    setState(() {
                                      _obscurePassword = !_obscurePassword;
                                    });
                                  },
                                ),
                                validator: (value) {
                                  if (value == null || value.isEmpty) {
                                    return 'Please enter your password';
                                  }
                                  if (value.length < 6) {
                                    return 'Password must be at least 6 characters';
                                  }
                                  return null;
                                },
                                isGlass: true,
                              ),
                              const SizedBox(height: 16),
                              
                              // Forgot Password Link
                              Align(
                                alignment: Alignment.centerRight,
                                child: TextButton(
                                  onPressed: () {
                                    // Navigate to forgot password screen
                                  },
                                  child: Text(
                                    "Forgot Password?",
                                    style: SpatialDesignSystem.subtitleSmall.copyWith(
                                      color: SpatialDesignSystem.primaryColor,
                                    ),
                                  ),
                                ),
                              ),
                              const SizedBox(height: 24),
                              
                              // Login Button
                              SpatialButton(
                                text: "Log In",
                                onPressed: _isLoading ? null : _handleLogin,
                                isGradient: true,
                                width: double.infinity,
                                child: _isLoading
                                    ? const SizedBox(
                                        width: 24,
                                        height: 24,
                                        child: CircularProgressIndicator(
                                          color: Colors.white,
                                          strokeWidth: 2,
                                        ),
                                      )
                                    : null,
                              ),
                              
                              const SizedBox(height: 16),
                              
                              // Biometric Login
                              Align(
                                alignment: Alignment.center,
                                child: TextButton.icon(
                                  onPressed: _handleBiometricLogin,
                                  icon: const Icon(Icons.fingerprint),
                                  label: Text(
                                    "Login with Fingerprint",
                                    style: SpatialDesignSystem.subtitleSmall,
                                  ),
                                ),
                              ),
                            ],
                          ),
                        ),
                      ),
                      
                      const SizedBox(height: 24),
                      
                      // Version Info
                      Text(
                        "v1.0.0 (2025)",
                        style: SpatialDesignSystem.captionText.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkSecondaryColor.withOpacity(0.6)
                              : SpatialDesignSystem.textSecondaryColor.withOpacity(0.6),
                        ),
                      ),
                    ],
                  ),
                ),
              ),
            ),
          ),
        ],
      ),
    );
  }

  void _handleLogin() {
    if (_formKey.currentState!.validate()) {
      setState(() {
        _isLoading = true;
      });
      
      // Simulate API call
      Future.delayed(const Duration(seconds: 2), () {
        setState(() {
          _isLoading = false;
        });
        
        // Navigate to main screen
        Navigator.pushReplacementNamed(context, '/');
      });
    }
  }

  void _handleBiometricLogin() async {
    // Show biometric authentication dialog
    setState(() {
      _isLoading = true;
    });
    
    // Simulate biometric auth
    Future.delayed(const Duration(seconds: 1), () {
      setState(() {
        _isLoading = false;
      });
      
      // Navigate to main screen
      Navigator.pushReplacementNamed(context, '/');
    });
  }
}


