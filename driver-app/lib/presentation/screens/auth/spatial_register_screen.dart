import 'dart:io';
import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'dart:ui';
import 'package:image_picker/image_picker.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_components.dart';
import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';
import 'package:flutter_animate/flutter_animate.dart';

class SpatialRegisterScreen extends StatefulWidget {
  const SpatialRegisterScreen({super.key});

  @override
  State<SpatialRegisterScreen> createState() => _SpatialRegisterScreenState();
}

class _SpatialRegisterScreenState extends State<SpatialRegisterScreen> {
  late TextEditingController _nameController;
  late TextEditingController _lastnameController;
  late TextEditingController _phoneController;
  late TextEditingController _emailController;
  late TextEditingController _passwordController;
  File? _image;
  final _formKey = GlobalKey<FormState>();
  bool _obscurePassword = true;
  bool isLoading = false;

  @override
  void initState() {
    _nameController = TextEditingController();
    _lastnameController = TextEditingController();
    _phoneController = TextEditingController();
    _emailController = TextEditingController();
    _passwordController = TextEditingController();
    super.initState();
  }

  @override
  void dispose() {
    _nameController.dispose();
    _lastnameController.dispose();
    _phoneController.dispose();
    _emailController.dispose();
    _passwordController.dispose();
    super.dispose();
  }

  Future<void> _selectImage() async {
    final PermissionStatus status = await Permission.storage.request();
    
    if (status == PermissionStatus.granted) {
      final ImagePicker picker = ImagePicker();
      final XFile? image = await picker.pickImage(source: ImageSource.gallery);
      
      if (image != null) {
        setState(() {
          _image = File(image.path);
        });
      }
    } else {
      showDialog(
        context: context,
        builder: (_) => AlertDialog(
          title: const Text('Permission Required'),
          content: const Text('The app needs storage access permission to select an image. Please grant permission in settings.'),
          actions: [
            TextButton(
              onPressed: () => Navigator.pop(context),
              child: const Text('OK'),
            ),
          ],
        ),
      );
    }
  }

  void _handleRegister() {
    if (_formKey.currentState!.validate()) {
      if (_image == null) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: const Text('Please select a profile picture'),
            backgroundColor: SpatialTheme.error,
            behavior: SnackBarBehavior.floating,
            shape: RoundedRectangleBorder(
              borderRadius: SpatialTheme.borderRadiusMedium,
            ),
          ),
        );
        return;
      }
      
      // TODO: Implement register logic with AuthBloc
      // Example:
      // BlocProvider.of<AuthBloc>(context).add(
      //   OnRegisterEvent(
      //     name: _nameController.text,
      //     email: _emailController.text,
      //     // ...additional parameters
      //   )
      // );
      
      // For demo, navigate to login screen
      Navigator.of(context).pushReplacement(
        MaterialPageRoute(builder: (context) => const SpatialLoginScreen()),
      );
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SpatialComponents.backgroundContainer(
        useDarkMode: true, // Set to dark mode
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
                
                const SizedBox(height: SpatialTheme.spaceLG),
                
                // Register Form
                _buildRegisterForm().animate().fadeIn(
                  delay: 300.ms,
                  duration: 600.ms,
                ).slideY(
                  begin: 0.3,
                  duration: 600.ms,
                  curve: Curves.easeOutCubic,
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
            onPressed: () => Navigator.of(context).pop(),
          ),
        ),
        
        const SizedBox(height: SpatialTheme.spaceLG),
        
        Text(
          'Create New Account',
          style: SpatialTheme.textTheme.headlineMedium?.copyWith(
            fontWeight: FontWeight.w600,
            color: Colors.white,
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          'Register to become a KTC Logistics driver',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withValues(alpha: 0.7),
          ),
          textAlign: TextAlign.center,
        ),
      ],
    );
  }

  Widget _buildRegisterForm() {
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
            // Profile Image
            Center(
              child: GestureDetector(
                onTap: _selectImage,
                child: Stack(
                  children: [
                    Container(
                      height: 120,
                      width: 120,
                      decoration: BoxDecoration(
                        color: Colors.white.withValues(alpha: 0.2),
                        shape: BoxShape.circle,
                        border: Border.all(color: Colors.white.withValues(alpha: 0.2)),
                        boxShadow: SpatialTheme.spatialShadow,
                        image: _image != null
                            ? DecorationImage(
                                image: FileImage(_image!),
                                fit: BoxFit.cover,
                              )
                            : null,
                      ),
                      child: _image == null
                          ? Icon(
                              FontAwesomeIcons.userAlt,
                              size: 40,
                              color: Colors.white.withValues(alpha: 0.5),
                            )
                          : null,
                    ),
                    Positioned(
                      bottom: 0,
                      right: 0,
                      child: Container(
                        height: 40,
                        width: 40,
                        decoration: BoxDecoration(
                          color: SpatialTheme.primaryBlue,
                          shape: BoxShape.circle,
                          boxShadow: SpatialTheme.spatialShadow,
                        ),
                        child: const Icon(
                          Icons.camera_alt,
                          color: Colors.white,
                          size: 20,
                        ),
                      ),
                    ),
                  ],
                ),
              ),
            ),
                
            const SizedBox(height: SpatialTheme.spaceLG),
                
            // First Name & Last Name (Row)
            Row(
              children: [
                Expanded(
                  child: SpatialComponents.spatialTextField(
                    label: 'Tên',
                    hint: 'Nhập tên của bạn',
                    controller: _nameController,
                    prefixIcon: FontAwesomeIcons.user,
                    enabled: !isLoading,
                    useDarkMode: true,
                    validator: (value) {
                      if (value?.isEmpty ?? true) {
                        return 'Vui lòng nhập tên';
                      }
                      return null;
                    },
                  ),
                ),
                const SizedBox(width: SpatialTheme.spaceMD),
                Expanded(
                  child: SpatialComponents.spatialTextField(
                    label: 'Họ',
                    hint: 'Nhập họ của bạn',
                    controller: _lastnameController,
                    prefixIcon: FontAwesomeIcons.user,
                    enabled: !isLoading,
                    useDarkMode: true,
                    validator: (value) {
                      if (value?.isEmpty ?? true) {
                        return 'Vui lòng nhập họ';
                      }
                      return null;
                    },
                  ),
                ),
              ],
            ),
                
            const SizedBox(height: SpatialTheme.spaceLG),
                
            // Phone
            SpatialComponents.spatialTextField(
              label: 'Phone Number',
              hint: 'Enter your phone number',
              controller: _phoneController,
              prefixIcon: FontAwesomeIcons.phone,
              keyboardType: TextInputType.phone,
              enabled: !isLoading,
              useDarkMode: true,
              validator: (value) {
                if (value?.isEmpty ?? true) {
                  return 'Please enter your phone number';
                }
                if (!RegExp(r'^\d{10,11}$').hasMatch(value!)) {
                  return 'Invalid phone number';
                }
                return null;
              },
            ),
                
            const SizedBox(height: SpatialTheme.spaceLG),
                
            // Email
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
                if (!RegExp(r'^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$').hasMatch(value!)) {
                  return 'Invalid email format';
                }
                return null;
              },
            ),
                
            const SizedBox(height: SpatialTheme.spaceLG),
                
            // Password
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
                  color: Colors.white.withValues(alpha: 0.5),
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
                
            const SizedBox(height: SpatialTheme.spaceLG),
                
            // Register Button
            SizedBox(
              width: double.infinity,
              child: ElevatedButton.icon(
                icon: Icon(FontAwesomeIcons.userPlus, size: 16),
                label: Text(
                  isLoading ? 'Registering...' : 'Register',
                  style: SpatialTheme.textTheme.titleMedium?.copyWith(
                    fontWeight: FontWeight.w600,
                    color: Colors.black87,
                  ),
                ),
                onPressed: isLoading ? null : _handleRegister,
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
  }

  Widget _buildFooter() {
    return Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        Text(
          'Already have an account? ',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withValues(alpha: 0.7),
          ),
        ),
        TextButton(
          onPressed: () => Navigator.of(context).pushReplacement(
            MaterialPageRoute(builder: (context) => const SpatialLoginScreen()),
          ),
          style: TextButton.styleFrom(
            foregroundColor: Colors.white,
            padding: EdgeInsets.zero,
            minimumSize: Size.zero,
            tapTargetSize: MaterialTapTargetSize.shrinkWrap,
          ),
          child: Text(
            'Login',
            style: TextStyle(
              color: Colors.white,
              fontWeight: FontWeight.w600,
            ),
          ),
        ),
      ],
    );
  }
}
