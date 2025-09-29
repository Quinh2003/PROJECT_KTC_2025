import 'package:flutter/material.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_background.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';

class EnvironmentSelectionScreen extends StatelessWidget {
  const EnvironmentSelectionScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SpatialBackground(
        color: SpatialTheme.primaryBlue,
        opacity: 0.1,
        child: SafeArea(
          child: Center(
            child: Padding(
              padding: const EdgeInsets.all(24.0),
              child: Column(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  // Logo
                  const SizedBox(height: 40),
                  _buildLogo().animate().fadeIn(duration: 800.ms),
                  
                  const SizedBox(height: 40),
                  
                  // Title
                  Text(
                    'Choose Environment',
                    style: SpatialTheme.textTheme.displayMedium?.copyWith(
                      color: Colors.white,
                      fontWeight: FontWeight.bold,
                    ),
                  ).animate().fadeIn(
                    delay: 200.ms,
                    duration: 500.ms,
                  ),
                  
                  const SizedBox(height: 16),
                  
                  // Description
                  Text(
                    'Select the environment for the application',
                    textAlign: TextAlign.center,
                    style: SpatialTheme.textTheme.bodyLarge?.copyWith(
                      color: Colors.white70,
                    ),
                  ).animate().fadeIn(
                    delay: 400.ms,
                    duration: 500.ms,
                  ),
                  
                  const SizedBox(height: 60),
                  
                  // Environment Buttons
                  _buildEnvironmentButtons(context),
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }
  
  Widget _buildLogo() {
    return Container(
      height: 100,
      width: 100,
      decoration: BoxDecoration(
        color: Colors.white.withOpacity(0.1),
        borderRadius: BorderRadius.circular(20),
        boxShadow: [
          BoxShadow(
            color: Colors.black.withOpacity(0.1),
            blurRadius: 10,
            spreadRadius: 1,
          ),
        ],
      ),
      child: Center(
        child: Icon(
          Icons.local_shipping_rounded,
          size: 60,
          color: SpatialTheme.primaryBlue,
        ),
      ),
    );
  }
  
  Widget _buildEnvironmentButtons(BuildContext context) {
    return Column(
      children: [
        // Live Environment Button
        _buildEnvButton(
          text: "Live environment",
          icon: Icons.cloud_done,
          color: SpatialTheme.primaryBlue,
          onPressed: () => _selectLiveEnvironment(context),
        ).animate().fadeIn(delay: 600.ms, duration: 500.ms).slideY(
          begin: 0.3,
          end: 0,
          delay: 600.ms,
          duration: 500.ms,
          curve: Curves.easeOutCubic,
        ),
        
        const SizedBox(height: 16),
        
        // Test Environment Button
        _buildEnvButton(
          text: "Test environment",
          icon: Icons.code,
          color: SpatialTheme.warning,
          onPressed: () => _selectTestEnvironment(context),
        ).animate().fadeIn(delay: 800.ms, duration: 500.ms).slideY(
          begin: 0.3,
          end: 0,
          delay: 800.ms,
          duration: 500.ms,
          curve: Curves.easeOutCubic,
        ),
      ],
    );
  }
  
  Widget _buildEnvButton({
    required String text,
    required IconData icon,
    required Color color,
    required VoidCallback onPressed,
  }) {
    return SpatialComponents.glassContainer(
      width: double.infinity,
      padding: const EdgeInsets.symmetric(vertical: 16, horizontal: 20),
      child: Material(
        color: Colors.transparent,
        child: InkWell(
          onTap: onPressed,
          borderRadius: SpatialTheme.borderRadiusMedium,
          child: Padding(
            padding: const EdgeInsets.all(12.0),
            child: Row(
              children: [
                Container(
                  padding: const EdgeInsets.all(10),
                  decoration: BoxDecoration(
                    color: color.withOpacity(0.1),
                    borderRadius: SpatialTheme.borderRadiusSmall,
                  ),
                  child: Icon(
                    icon,
                    color: color,
                    size: 24,
                  ),
                ),
                const SizedBox(width: 16),
                Expanded(
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(
                        text,
                        style: SpatialTheme.textTheme.titleLarge?.copyWith(
                          color: Colors.white,
                          fontWeight: FontWeight.w600,
                        ),
                      ),
                    ],
                  ),
                ),
                const Icon(
                  Icons.arrow_forward_ios,
                  color: Colors.white54,
                  size: 16,
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
  
  void _selectLiveEnvironment(BuildContext context) async {
    final env = Environment.getInstance();
    
    // Set to live mode and use default URL
    await env.setTestMode(false);
    await env.updateApiBaseUrl('http://live-api.ktclogistics.com/api');
    
    if (context.mounted) {
      // Clear all user session data before switching environment
      try {
        // Trigger logout event to clear all bloc states
        // context.read<AuthBloc>().add(LogOutEvent());
      } catch (e) {
        print('AuthBloc not available: $e');
      }
      
      // Force logout and navigate to login screen since environment changed
      Navigator.of(context).pushAndRemoveUntil(
        MaterialPageRoute(builder: (context) => const SpatialLoginScreen()),
        (route) => false, // Remove all previous routes
      );
    }
  }
  
  void _selectTestEnvironment(BuildContext context) {
    Navigator.of(context).push(
      MaterialPageRoute(builder: (context) => const TestEnvironmentConfigScreen()),
    );
  }
}

class TestEnvironmentConfigScreen extends StatefulWidget {
  const TestEnvironmentConfigScreen({super.key});

  @override
  State<TestEnvironmentConfigScreen> createState() => _TestEnvironmentConfigScreenState();
}

class _TestEnvironmentConfigScreenState extends State<TestEnvironmentConfigScreen> {
  final _formKey = GlobalKey<FormState>();
  final _apiUrlController = TextEditingController(text: 'http://localhost:8080/api');
  
  @override
  void dispose() {
    _apiUrlController.dispose();
    super.dispose();
  }
  
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SpatialBackground(
        color: SpatialTheme.primaryBlue,
        opacity: 0.1,
        child: SafeArea(
          child: SingleChildScrollView(
            padding: const EdgeInsets.symmetric(
                horizontal: SpatialTheme.spaceLG,
                vertical: SpatialTheme.spaceSM),
            physics: const BouncingScrollPhysics(),
            child: Column(
              children: [
                const SizedBox(height: SpatialTheme.spaceMD),

                // Header Section
                _buildHeader().animate().fadeIn(duration: 800.ms).slideY(
                      begin: -0.3,
                      duration: 800.ms,
                      curve: Curves.easeOutBack,
                    ),

                const SizedBox(height: SpatialTheme.spaceXL),

                // Form Section
                _buildConfigForm()
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

                const SizedBox(height: SpatialTheme.spaceLG),
              ],
            ),
          ),
        ),
      ),
    );
  }

  Widget _buildHeader() {
    return Padding(
      padding: const EdgeInsets.all(SpatialTheme.spaceLG),
      child: Column(
        children: [
          // Logo
          Container(
            width: 80,
            height: 80,
            decoration: BoxDecoration(
              gradient: LinearGradient(
                colors: [
                  SpatialTheme.primaryBlue,
                  SpatialTheme.primaryBlue.withValues(alpha: 0.7),
                ],
                begin: Alignment.topLeft,
                end: Alignment.bottomRight,
              ),
              borderRadius: BorderRadius.circular(20),
              boxShadow: [
                BoxShadow(
                  color: SpatialTheme.primaryBlue.withValues(alpha: 0.3),
                  blurRadius: 20,
                  spreadRadius: 2,
                ),
              ],
            ),
            child: const Icon(
              Icons.code,
              color: Colors.white,
              size: 40,
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceMD),
          
          // Title
          Text(
            'Test Environment',
            style: SpatialTheme.textTheme.displayMedium?.copyWith(
              fontWeight: FontWeight.w700,
              color: Colors.white,
            ),
          ),
          const SizedBox(height: SpatialTheme.spaceSM),
          
          // Description
          Text(
            'Configure API URL for testing',
            textAlign: TextAlign.center,
            style: SpatialTheme.textTheme.bodyLarge?.copyWith(
              color: Colors.white70,
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildConfigForm() {
    return SpatialComponents.spatialCard(
      padding: const EdgeInsets.all(SpatialTheme.spaceLG),
      useDarkMode: true,
      child: Form(
        key: _formKey,
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            // Form Title
            Row(
              children: [
                Icon(
                  Icons.link,
                  color: SpatialTheme.primaryBlue,
                  size: 24,
                ),
                const SizedBox(width: SpatialTheme.spaceSM),
                Text(
                  'API Configuration',
                  style: SpatialTheme.textTheme.titleLarge?.copyWith(
                    color: Colors.white,
                    fontWeight: FontWeight.w600,
                  ),
                ),
              ],
            ),
            
            // const SizedBox(height: SpatialTheme.spaceMD),
            
            // // Description
            // Text(
            //   'Enter the API URL for test environment. Examples:',
            //   style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            //     color: Colors.white70,
            //   ),
            // ),
            
            // const SizedBox(height: SpatialTheme.spaceSM),
            
            // // Example URLs
            // _buildExampleUrls(),
            
            // const SizedBox(height: SpatialTheme.spaceLG),
            
            // API URL Field
            SpatialComponents.spatialTextField(
              label: '',
              hint: 'http://example.com/api',
              controller: _apiUrlController,
              prefixIcon: Icons.link,
              useDarkMode: true,
              validator: (value) {
                if (value == null || value.isEmpty) {
                  return 'Please enter the API URL';
                }
                if (!value.contains('http')) {
                  return 'URL must start with http:// or https://';
                }
                return null;
              },
            ),
            
            const SizedBox(height: SpatialTheme.spaceLG),
            
            // Quick URL buttons
            _buildQuickUrlButtons(),
            
            const SizedBox(height: SpatialTheme.spaceXL),
            
            // Action Buttons
            _buildActionButtons(),
          ],
        ),
      ),
    );
  }

  Widget _buildQuickUrlButtons() {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Row(
          children: [
            Icon(
              Icons.flash_on,
              color: SpatialTheme.primaryBlue,
              size: 20,
            ),
            const SizedBox(width: SpatialTheme.spaceSM),
            Text(
              'Quick URLs:',
              style: SpatialTheme.textTheme.titleMedium?.copyWith(
                color: Colors.white,
                fontWeight: FontWeight.w600,
              ),
            ),
          ],
        ),
        
        const SizedBox(height: SpatialTheme.spaceMD),
        
        Wrap(
          spacing: SpatialTheme.spaceSM,
          runSpacing: SpatialTheme.spaceSM,
          children: [
            _quickUrlChip('Localhost', 'http://localhost:8080/api'),
            _quickUrlChip('Emulator', 'http://10.0.2.2:8080/api'),
            _quickUrlChip('Test Server', 'https://ngrok-url.io/api'),
          ],
        ),
      ],
    );
  }
  
  Widget _quickUrlChip(String label, String url) {
    return GestureDetector(
      onTap: () {
        setState(() {
          _apiUrlController.text = url;
        });
      },
      child: Container(
        padding: const EdgeInsets.symmetric(
          horizontal: SpatialTheme.spaceMD,
          vertical: SpatialTheme.spaceSM,
        ),
        decoration: BoxDecoration(
          gradient: LinearGradient(
            colors: [
              SpatialTheme.primaryBlue.withValues(alpha: 0.2),
              SpatialTheme.primaryBlue.withValues(alpha: 0.1),
            ],
            begin: Alignment.topLeft,
            end: Alignment.bottomRight,
          ),
          borderRadius: SpatialTheme.borderRadiusMedium,
          border: Border.all(
            color: SpatialTheme.primaryBlue.withValues(alpha: 0.3),
            width: 1,
          ),
        ),
        child: Row(
          mainAxisSize: MainAxisSize.min,
          children: [
            Icon(
              Icons.touch_app,
              color: SpatialTheme.primaryBlue,
              size: 16,
            ),
            const SizedBox(width: SpatialTheme.spaceSM),
            Text(
              label,
              style: SpatialTheme.textTheme.bodyMedium?.copyWith(
                color: Colors.white,
                fontWeight: FontWeight.w500,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildActionButtons() {
    return Row(
      children: [
        // Back Button
        Expanded(
          child: SpatialComponents.gradientButton(
            text: 'Back',
            icon: Icons.arrow_back,
            onPressed: () {
              Navigator.of(context).pop();
            },
            gradient: LinearGradient(
              colors: [
                Colors.white.withValues(alpha: 0.1),
                Colors.white.withValues(alpha: 0.05),
              ],
            ),
          ),
        ),
        
        const SizedBox(width: SpatialTheme.spaceMD),
        
        // Save Button
        Expanded(
          child: SpatialComponents.gradientButton(
            text: 'Apply',
            icon: Icons.check_circle,
            onPressed: _saveConfiguration,
            gradient: LinearGradient(
              colors: [
                SpatialTheme.primaryBlue,
                SpatialTheme.primaryBlue.withValues(alpha: 0.8),
              ],
              begin: Alignment.topLeft,
              end: Alignment.bottomRight,
            ),
          ),
        ),
      ],
    );
  }
  
  void _saveConfiguration() async {
    if (_formKey.currentState!.validate()) {
      final env = Environment.getInstance();
      
      // Set to test mode with custom URL
      await env.setTestMode(true);
      await env.updateApiBaseUrl(_apiUrlController.text.trim());
      
      if (context.mounted) {
        // Show beautiful success notification
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Row(
              children: [
                Icon(
                  Icons.check_circle,
                  color: Colors.white,
                ),
                const SizedBox(width: 12),
                Expanded(
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    mainAxisSize: MainAxisSize.min,
                    children: [
                      Text(
                        'Environment Updated',
                        style: TextStyle(
                          color: Colors.white,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                      Text(
                        'Test environment settings saved',
                        style: TextStyle(
                          color: Colors.white70,
                          fontSize: 12,
                        ),
                      ),
                    ],
                  ),
                ),
              ],
            ),
            backgroundColor: SpatialTheme.primaryBlue,
            behavior: SnackBarBehavior.floating,
            shape: RoundedRectangleBorder(
              borderRadius: SpatialTheme.borderRadiusMedium,
            ),
            margin: const EdgeInsets.all(SpatialTheme.spaceMD),
            duration: const Duration(seconds: 2),
          ),
        );
        
        // Navigate to login with animation after environment switch
        await Future.delayed(const Duration(milliseconds: 500));
        if (context.mounted) {
          // Clear all user session data before switching environment
          try {
            // Trigger logout event to clear all bloc states
            // context.read<AuthBloc>().add(LogOutEvent());
          } catch (e) {
            print('AuthBloc not available: $e');
          }
          
          Navigator.of(context).pushAndRemoveUntil(
            PageRouteBuilder(
              pageBuilder: (context, animation, secondaryAnimation) => const SpatialLoginScreen(),
              transitionsBuilder: (context, animation, secondaryAnimation, child) {
                return FadeTransition(
                  opacity: animation,
                  child: SlideTransition(
                    position: Tween<Offset>(
                      begin: const Offset(1.0, 0.0),
                      end: Offset.zero,
                    ).animate(CurvedAnimation(
                      parent: animation,
                      curve: Curves.easeOutCubic,
                    )),
                    child: child,
                  ),
                );
              },
              transitionDuration: const Duration(milliseconds: 300),
            ),
            (route) => false, // Remove all previous routes
          );
        }
      }
    }
  }
}