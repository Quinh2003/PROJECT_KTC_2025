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
  late TextEditingController _nameController;
  late TextEditingController _lastNameController;
  late TextEditingController _phoneController;
  late TextEditingController _emailController;

  final _keyForm = GlobalKey<FormState>();

  @override
  void initState() {
    super.initState();
    _nameController = TextEditingController();
    _lastNameController = TextEditingController();
    _phoneController = TextEditingController();
    _emailController = TextEditingController();
  }

  @override
  void dispose() {
    _nameController.dispose();
    _lastNameController.dispose();
    _phoneController.dispose();
    _emailController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final userBloc = BlocProvider.of<UserBloc>(context);
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return BlocListener<UserBloc, UserState>(
      listener: (context, state) {
        if (state is LoadingUserState) {
          modalLoading(context);
        } else if (state is SuccessUserState) {
          Navigator.pop(context);
          modalSuccess(context, 'Profile updated successfully', () => Navigator.pop(context));
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
              ? SpatialDesignSystem.darkSurfaceColor
              : Colors.white,
          elevation: 0,
          centerTitle: true,
          title: Text(
            'Edit Profile', 
            style: SpatialDesignSystem.subtitleLarge.copyWith(
              color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
            ),
          ),
          leadingWidth: 60,
          leading: InkWell(
            onTap: () => Navigator.pop(context),
            child: Container(
              padding: const EdgeInsets.only(left: 16.0),
              child: Icon(
                Icons.arrow_back_ios_new_rounded, 
                color: SpatialDesignSystem.primaryColor, 
                size: 20
              ),
            ),
          ),
        ),
        body: SafeArea(
          child: BlocBuilder<UserBloc, UserState>(
            builder: (context, state) {
              // Nếu có user data, cập nhật vào các controllers
              if (state.user != null && _emailController.text.isEmpty) {
                // Split name into first and last name
                final nameParts = state.user!.name.split(' ');
                final firstName = nameParts.isNotEmpty ? nameParts.first : '';
                final lastName = nameParts.length > 1 ? nameParts.sublist(1).join(' ') : '';
                
                _nameController.text = firstName;
                _lastNameController.text = lastName;
                _phoneController.text = state.user!.phone;
                _emailController.text = state.user!.email;
              }

              return Form(
                key: _keyForm,
                child: ListView(
                  physics: const BouncingScrollPhysics(),
                  padding: const EdgeInsets.symmetric(horizontal: 20.0, vertical: 20.0),
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
                                      color: Colors.black.withValues(alpha: 0.1),
                                      blurRadius: 10,
                                      spreadRadius: 2
                                    )
                                  ]
                                ),
                                child: state.user?.image != null && state.user!.image.isNotEmpty
                                  ? ClipRRect(
                                      borderRadius: BorderRadius.circular(50),
                                      child: Image.network(
                                        state.user!.image,
                                        fit: BoxFit.cover,
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
                          Text(
                            state.user?.name ?? "Update Your Profile",
                            style: SpatialDesignSystem.subtitleLarge.copyWith(
                              color: isDark
                                ? SpatialDesignSystem.textDarkPrimaryColor
                                : SpatialDesignSystem.textPrimaryColor,
                              fontWeight: FontWeight.bold,
                            ),
                          ),
                          const SizedBox(height: 4),
                          Text(
                            state.user?.email ?? "",
                            style: SpatialDesignSystem.bodyMedium.copyWith(
                              color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
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
                                style: SpatialDesignSystem.subtitleMedium.copyWith(
                                  color: isDark
                                    ? SpatialDesignSystem.textDarkPrimaryColor
                                    : SpatialDesignSystem.textPrimaryColor,
                                ),
                              ),
                            ],
                          ),
                          const SizedBox(height: 20),
                          
                          // First Name field
                          SpatialTextField(
                            label: 'First Name',
                            hint: 'Enter your first name',
                            controller: _nameController,
                            prefix: Icon(
                              Icons.person_outline, 
                              color: SpatialDesignSystem.primaryColor,
                              size: 20,
                            ),
                            validator: (value) => value!.isEmpty ? 'First name is required' : null,
                            isGlass: true,
                          ),
                          const SizedBox(height: 20),
                          
                          // Last Name field
                          SpatialTextField(
                            label: 'Last Name',
                            hint: 'Enter your last name',
                            controller: _lastNameController,
                            prefix: Icon(
                              Icons.person_outline, 
                              color: SpatialDesignSystem.primaryColor,
                              size: 20,
                            ),
                            validator: (value) => value!.isEmpty ? 'Last name is required' : null,
                            isGlass: true,
                          ),
                          const SizedBox(height: 20),
                          
                          // Phone Number field
                          SpatialTextField(
                            label: 'Phone Number',
                            hint: '000-000-000',
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
                        if (_keyForm.currentState!.validate() && userBloc.state.user != null) {
                          userBloc.add(OnEditUserEvent(_nameController.text, _lastNameController.text, _phoneController.text));
                        }
                      },
                      iconData: Icons.save,
                      isGradient: true,
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
