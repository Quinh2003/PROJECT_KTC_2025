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
  const SpatialRegisterScreen({Key? key}) : super(key: key);

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
      final ImagePicker _picker = ImagePicker();
      final XFile? image = await _picker.pickImage(source: ImageSource.gallery);
      
      if (image != null) {
        setState(() {
          _image = File(image.path);
        });
      }
    } else {
      showDialog(
        context: context,
        builder: (_) => AlertDialog(
          title: const Text('Cần cấp quyền'),
          content: const Text('Ứng dụng cần quyền truy cập bộ nhớ để chọn ảnh. Vui lòng cấp quyền trong cài đặt.'),
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
            content: const Text('Vui lòng chọn ảnh đại diện'),
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
          'Tạo tài khoản mới',
          style: SpatialTheme.textTheme.headlineMedium?.copyWith(
            fontWeight: FontWeight.w600,
            color: Colors.white,
          ),
        ),
        const SizedBox(height: SpatialTheme.spaceSM),
        Text(
          'Đăng ký để trở thành tài xế KTC Logistics',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withOpacity(0.7),
          ),
          textAlign: TextAlign.center,
        ),
      ],
    );
  }

  Widget _buildRegisterForm() {
    return SpatialComponents.glassContainer(
      useDarkMode: true,
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
                        color: Colors.white.withOpacity(0.2),
                        shape: BoxShape.circle,
                        border: Border.all(color: Colors.white.withOpacity(0.2)),
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
                              color: Colors.white.withOpacity(0.5),
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
              label: 'Số điện thoại',
              hint: 'Nhập số điện thoại của bạn',
              controller: _phoneController,
              prefixIcon: FontAwesomeIcons.phone,
              keyboardType: TextInputType.phone,
              enabled: !isLoading,
              useDarkMode: true,
              validator: (value) {
                if (value?.isEmpty ?? true) {
                  return 'Vui lòng nhập số điện thoại';
                }
                if (!RegExp(r'^\d{10,11}$').hasMatch(value!)) {
                  return 'Số điện thoại không hợp lệ';
                }
                return null;
              },
            ),
                
            const SizedBox(height: SpatialTheme.spaceLG),
                
            // Email
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
                
            // Password
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
                  color: Colors.white.withOpacity(0.5),
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
              child: SpatialComponents.gradientButton(
                text: 'Đăng ký',
                onPressed: _handleRegister,
                loading: isLoading,
                icon: FontAwesomeIcons.userPlus,
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
          'Đã có tài khoản? ',
          style: SpatialTheme.textTheme.bodyMedium?.copyWith(
            color: Colors.white.withOpacity(0.7),
          ),
        ),
        TextButton(
          onPressed: () => Navigator.of(context).pushReplacement(
            MaterialPageRoute(builder: (context) => const SpatialLoginScreen()),
          ),
          child: Text(
            'Đăng nhập',
            style: TextStyle(
              color: SpatialTheme.primaryCyan,
              fontWeight: FontWeight.w600,
            ),
          ),
        ),
      ],
    );
  }
}
