import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/presentation/helpers/helpers.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_button.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_text_field.dart';
import 'dart:ui';

class EditProfileScreen extends StatefulWidget {
  const EditProfileScreen({super.key});

  @override
  State<EditProfileScreen> createState() => _EditProfileScreenState();
}

class _EditProfileScreenState extends State<EditProfileScreen> {
  late TextEditingController _fullNameController;
  late TextEditingController _phoneController;
  late TextEditingController _emailController;

  final _keyForm = GlobalKey<FormState>();

  @override
  void initState() {
    super.initState();
    _fullNameController = TextEditingController();
    _phoneController = TextEditingController();
    _emailController = TextEditingController();
    
    // Call event to fetch user information when screen is initialized
    Future.microtask(() {
      context.read<UserBloc>().add(OnFetchUserEvent());
    });
  }

  @override
  void dispose() {
    _fullNameController.dispose();
    _phoneController.dispose();
    _emailController.dispose();
    super.dispose();
  }

  // Method to display logout confirmation dialog
  void _showLogoutConfirmation(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    showDialog(
      context: context,
      barrierDismissible: false,
      builder: (context) => BackdropFilter(
        filter: ImageFilter.blur(sigmaX: 4, sigmaY: 4),
        child: AlertDialog(
          backgroundColor: isDark
              ? SpatialDesignSystem.darkSurfaceColor.withValues(alpha: 0.9)
              : SpatialDesignSystem.surfaceColor.withValues(alpha: 0.9),
          shape: RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(16),
            side: BorderSide(
              color: isDark
                  ? Colors.white.withValues(alpha: 0.1)
                  : Colors.black.withValues(alpha: 0.05),
              width: 1,
            ),
          ),
          title: Row(
            children: [
              Icon(
                Icons.logout,
                color: SpatialDesignSystem.errorColor.withValues(alpha: 0.9),
              ),
              const SizedBox(width: 10),
              Text(
                'Confirm Logout',
                style: SpatialDesignSystem.subtitleLarge.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
            ],
          ),
          content: Text(
            'Are you sure you want to log out?',
            style: SpatialDesignSystem.bodyMedium.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor
                  : SpatialDesignSystem.textSecondaryColor,
            ),
          ),
          actions: [
            TextButton(
              onPressed: () => Navigator.pop(context),
              child: Text(
                'Cancel',
                style: TextStyle(color: SpatialDesignSystem.textSecondaryColor),
              ),
            ),
            ElevatedButton(
              style: ElevatedButton.styleFrom(
                backgroundColor:
                    SpatialDesignSystem.errorColor.withValues(alpha: 0.85),
                shape: RoundedRectangleBorder(
                  borderRadius: BorderRadius.circular(8),
                ),
              ),
              onPressed: () {
                Navigator.pop(context);
                // Dispatch LogOut event to AuthBloc
                context.read<AuthBloc>().add(LogOutEvent());
                // After logout, navigate user back to login screen
                Navigator.pushNamedAndRemoveUntil(
                    context, '/login', (route) => false);
              },
              child:
                  const Text('Logout', style: TextStyle(color: Colors.white)),
            ),
          ],
        ),
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    final userBloc = BlocProvider.of<UserBloc>(context, listen: false); // Optimize: don't listen here
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return BlocListener<UserBloc, UserState>(
      listener: (context, state) {
        if (state is LoadingUserState) {
          modalLoading(context);
        } else if (state is SuccessUserState) {
          Navigator.pop(context);
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(
              content: Text(
                'Profile updated successfully',
                style: TextStyle(
                  fontSize: 14.0,
                  fontWeight: FontWeight.normal,
                ),
              ),
              backgroundColor: SpatialDesignSystem.successColor,
              duration: Duration(seconds: 2),
            ),
          );
        } else if (state is FailureUserState) {
          Navigator.pop(context);
          errorMessageSnack(context, state.error);
        }
      },
      child: Scaffold(
        backgroundColor: isDark
            ? SpatialDesignSystem.darkBackgroundColor
            : SpatialDesignSystem.backgroundColor,
        appBar: AppBar(
          backgroundColor: isDark
              ? SpatialDesignSystem.darkBackgroundColor
              : SpatialDesignSystem.backgroundColor,
          elevation: 0,
          centerTitle: true,
          title: Text(
            'Profile',
            style: SpatialDesignSystem.subtitleLarge.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkPrimaryColor
                  : SpatialDesignSystem.textPrimaryColor,
            ),
          ),
          automaticallyImplyLeading: false, // Remove back button
          actions: [
            // Logout button in top-right corner
            TextButton.icon(
              onPressed: () {
                _showLogoutConfirmation(context);
              },
              icon: Icon(
                Icons.logout,
                size: 18,
                color: SpatialDesignSystem.errorColor,
              ),
              label: Text(
                'Logout',
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: SpatialDesignSystem.errorColor,
                  fontWeight: FontWeight.bold,
                ),
              ),
              style: TextButton.styleFrom(
                padding: const EdgeInsets.symmetric(horizontal: 16),
              ),
            ),
          ],
        ),
        body: SafeArea(
          child: BlocBuilder<UserBloc, UserState>(
            builder: (context, state) {
              // If user data exists, update controllers
              if (state.user != null && _emailController.text.isEmpty) {
                // Use post frame callback to avoid rebuilds during build
                WidgetsBinding.instance.addPostFrameCallback((_) {
                  if (mounted) {
                    // Set full name
                    _fullNameController.text = state.user!.name;
                    _phoneController.text = state.user!.phone;
                    _emailController.text = state.user!.email;
                  }
                });
              }

              return Form(
                key: _keyForm,
                child: ListView(
                  physics: const BouncingScrollPhysics(),
                  padding: const EdgeInsets.symmetric(
                      horizontal: 20.0, vertical: 20.0),
                  children: [
                    // Profile Image Section
                    GlassCard(
                      padding: const EdgeInsets.all(24),
                      child: Column(
                        children: [
                          Stack(
                            children: [
                              Container(
                                width: 100,
                                height: 100,
                                decoration: BoxDecoration(
                                    shape: BoxShape.circle,
                                    color: isDark
                                        ? Colors.grey[800]
                                        : Colors.grey[200],
                                    boxShadow: [
                                      BoxShadow(
                                          color: Colors.black
                                              .withValues(alpha: 0.1),
                                          blurRadius: 10,
                                          spreadRadius: 2)
                                    ]),
                                child: state.user?.image != null &&
                                        state.user!.image.isNotEmpty
                                    ? ClipRRect(
                                        borderRadius: BorderRadius.circular(50),
                                        child: Image.network(
                                          state.user!.image,
                                          fit: BoxFit.cover,
                                          loadingBuilder: (context, child, loadingProgress) {
                                            if (loadingProgress == null) return child;
                                            return Center(
                                              child: CircularProgressIndicator(
                                                value: loadingProgress.expectedTotalBytes != null
                                                    ? loadingProgress.cumulativeBytesLoaded /
                                                        loadingProgress.expectedTotalBytes!
                                                    : null,
                                              ),
                                            );
                                          },
                                          errorBuilder: (_, __, ___) => Icon(
                                            Icons.person,
                                            size: 50,
                                            color: isDark
                                                ? Colors.grey[400]
                                                : Colors.grey[600],
                                          ),
                                        ),
                                      )
                                    : Icon(
                                        Icons.person,
                                        size: 50,
                                        color: isDark
                                            ? Colors.grey[400]
                                            : Colors.grey[600],
                                      ),
                              ),
                              Positioned(
                                bottom: 0,
                                right: 0,
                                child: Container(
                                  padding: const EdgeInsets.all(4),
                                  decoration: BoxDecoration(
                                    shape: BoxShape.circle,
                                    color: SpatialDesignSystem.primaryColor,
                                  ),
                                  child: const Icon(
                                    Icons.camera_alt,
                                    size: 18,
                                    color: Colors.white,
                                  ),
                                ),
                              )
                            ],
                          ),
                          const SizedBox(height: 16),

                          // Display username
                          Text(
                            state.user?.username != null
                                ? '@${state.user?.username}'
                                : "@faker",
                            style: SpatialDesignSystem.bodyMedium.copyWith(
                              color: SpatialDesignSystem.primaryColor
                                  .withValues(alpha: 0.8),
                              fontWeight: FontWeight.bold,
                            ),
                          ),
                        ],
                      ),
                    ),

                    const SizedBox(height: 24),

                    // Form Section
                    GlassCard(
                      padding: const EdgeInsets.all(20),
                      child: Column(
                        crossAxisAlignment: CrossAxisAlignment.start,
                        children: [
                          Row(
                            children: [
                              Icon(
                                Icons.person,
                                color: SpatialDesignSystem.primaryColor,
                                size: 20,
                              ),
                              const SizedBox(width: 8),
                              Text(
                                'Personal Information',
                                style:
                                    SpatialDesignSystem.subtitleMedium.copyWith(
                                  color: isDark
                                      ? SpatialDesignSystem.textDarkPrimaryColor
                                      : SpatialDesignSystem.textPrimaryColor,
                                ),
                              ),
                            ],
                          ),
                          const SizedBox(height: 20),

                          // Full Name field
                          SpatialTextField(
                            label: 'Full Name',
                            hint: 'Enter your full name',
                            controller: _fullNameController,
                            prefix: Icon(
                              Icons.person_outline,
                              color: SpatialDesignSystem.primaryColor,
                              size: 20,
                            ),
                            validator: (value) =>
                                value!.isEmpty ? 'Full name is required' : null,
                            isGlass: true,
                          ),
                          const SizedBox(height: 20),

                          // Phone Number field
                          SpatialTextField(
                            label: 'Phone Number',
                            hint: '000-000-0000',
                            controller: _phoneController,
                            keyboardType: TextInputType.phone,
                            prefix: Icon(
                              Icons.phone_outlined,
                              color: SpatialDesignSystem.primaryColor,
                              size: 20,
                            ),
                            validator: (value) => validatedPhoneForm(value),
                            isGlass: true,
                          ),
                          const SizedBox(height: 20),

                          // Email field (read-only)
                          SpatialTextField(
                            label: 'Email Address',
                            hint: 'abc@gmail.com',
                            controller: _emailController,
                            prefix: Icon(
                              Icons.email_outlined,
                              color: SpatialDesignSystem.primaryColor,
                              size: 20,
                            ),
                            enabled: false,
                            isGlass: true,
                          ),
                        ],
                      ),
                    ),

                    const SizedBox(height: 30),

                    // Save Button
                    SpatialButton(
                      text: 'Save Changes',
                      onPressed: () {
                        if (_keyForm.currentState!.validate() &&
                            userBloc.state.user != null) {
                          // Extract first name and last name if needed
                          final nameParts = _fullNameController.text.trim().split(' ');
                          final firstName =
                              nameParts.isNotEmpty ? nameParts.first : '';
                          final lastName = nameParts.length > 1
                              ? nameParts.sublist(1).join(' ')
                              : '';

                          // Use read instead of context.read to avoid unnecessary rebuilds
                          context.read<UserBloc>().add(OnEditUserEvent(
                              firstName, lastName, _phoneController.text.trim()));
                        }
                      },
                      iconData: Icons.save,
                      backgroundColor: SpatialDesignSystem.primaryColor
                          .withValues(alpha: 0.85),
                    ),

                    const SizedBox(height: 20),
                  ],
                ),
              );
            },
          ),
        ),
      ),
    );
  }
}
