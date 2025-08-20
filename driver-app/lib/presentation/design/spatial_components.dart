import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_design_system.dart';

/// Spatial UI Components Library
class SpatialComponents {
  
  /// Glassmorphism Container
  static Widget glassContainer({
    required Widget child,
    double? width,
    double? height,
    EdgeInsetsGeometry? padding,
    EdgeInsetsGeometry? margin,
    BorderRadius? borderRadius,
    bool elevated = false,
  }) {
    return Container(
      width: width,
      height: height,
      margin: margin,
      padding: padding ?? const EdgeInsets.all(SpatialTheme.spaceMD),
      decoration: BoxDecoration(
        gradient: LinearGradient(
          begin: Alignment.topLeft,
          end: Alignment.bottomRight,
          colors: [
            SpatialTheme.glassLight,
            SpatialTheme.glassDark,
          ],
        ),
        borderRadius: borderRadius ?? SpatialTheme.borderRadiusMedium,
        border: Border.all(
          color: SpatialTheme.glassBorder,
          width: 1,
        ),
        boxShadow: elevated ? SpatialTheme.elevatedShadow : SpatialTheme.spatialShadow,
      ),
      child: child,
    );
  }
  
  /// Spatial Card with depth
  static Widget spatialCard({
    required Widget child,
    double? width,
    double? height,
    EdgeInsetsGeometry? padding,
    EdgeInsetsGeometry? margin,
    BorderRadius? borderRadius,
    Color? backgroundColor,
    bool glowing = false,
  }) {
    return Container(
      width: width,
      height: height,
      margin: margin,
      padding: padding ?? const EdgeInsets.all(SpatialTheme.spaceMD),
      decoration: BoxDecoration(
        color: backgroundColor ?? SpatialTheme.surfaceLight,
        borderRadius: borderRadius ?? SpatialTheme.borderRadiusLarge,
        boxShadow: glowing ? SpatialTheme.glowShadow : SpatialTheme.spatialShadow,
      ),
      child: child,
    );
  }
  
  /// Gradient Button
  static Widget gradientButton({
    required String text,
    required VoidCallback onPressed,
    double? width,
    double? height,
    bool loading = false,
    bool disabled = false,
    IconData? icon,
    Gradient? gradient,
  }) {
    return Container(
      width: width,
      height: height ?? 56,
      decoration: BoxDecoration(
        gradient: disabled 
          ? LinearGradient(colors: [Colors.grey.shade300, Colors.grey.shade400])
          : (gradient ?? SpatialTheme.primaryGradient),
        borderRadius: SpatialTheme.borderRadiusMedium,
        boxShadow: disabled ? [] : SpatialTheme.spatialShadow,
      ),
      child: Material(
        color: Colors.transparent,
        child: InkWell(
          onTap: disabled || loading ? null : onPressed,
          borderRadius: SpatialTheme.borderRadiusMedium,
          child: Container(
            padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
            child: Row(
              mainAxisSize: MainAxisSize.min,
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                if (loading) ...[
                  const SizedBox(
                    width: 20,
                    height: 20,
                    child: CircularProgressIndicator(
                      strokeWidth: 2,
                      valueColor: AlwaysStoppedAnimation<Color>(Colors.white),
                    ),
                  ),
                  const SizedBox(width: 12),
                ] else if (icon != null) ...[
                  Icon(icon, color: Colors.white, size: 20),
                  const SizedBox(width: 12),
                ],
                Text(
                  text,
                  style: const TextStyle(
                    color: Colors.white,
                    fontSize: 16,
                    fontWeight: FontWeight.w600,
                  ),
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
  
  /// Spatial Input Field
  static Widget spatialTextField({
    required String label,
    String? hint,
    TextEditingController? controller,
    bool obscureText = false,
    IconData? prefixIcon,
    Widget? suffixIcon,
    String? Function(String?)? validator,
    TextInputType? keyboardType,
    bool enabled = true,
  }) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          label,
          style: const TextStyle(
            fontSize: 14,
            fontWeight: FontWeight.w500,
            color: SpatialTheme.textSecondary,
          ),
        ),
        const SizedBox(height: 8),
        Container(
          decoration: BoxDecoration(
            color: SpatialTheme.surfaceLight.withValues(alpha: 0.8),
            borderRadius: SpatialTheme.borderRadiusMedium,
            boxShadow: [
              BoxShadow(
                color: Colors.black.withValues(alpha: 0.05),
                blurRadius: 10,
                offset: const Offset(0, 2),
              ),
            ],
          ),
          child: TextFormField(
            controller: controller,
            obscureText: obscureText,
            keyboardType: keyboardType,
            validator: validator,
            enabled: enabled,
            decoration: InputDecoration(
              hintText: hint,
              prefixIcon: prefixIcon != null ? Icon(prefixIcon, color: SpatialTheme.textTertiary) : null,
              suffixIcon: suffixIcon,
              border: InputBorder.none,
              contentPadding: const EdgeInsets.symmetric(horizontal: 16, vertical: 16),
              hintStyle: const TextStyle(color: SpatialTheme.textTertiary),
            ),
          ),
        ),
      ],
    );
  }
  
  /// Status Badge
  static Widget statusBadge({
    required String text,
    required Color backgroundColor,
    Color? textColor,
    IconData? icon,
  }) {
    return Container(
      padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
      decoration: BoxDecoration(
        color: backgroundColor.withValues(alpha: 0.1),
        borderRadius: SpatialTheme.borderRadiusSmall,
        border: Border.all(color: backgroundColor.withValues(alpha: 0.3)),
      ),
      child: Row(
        mainAxisSize: MainAxisSize.min,
        children: [
          if (icon != null) ...[
            Icon(icon, size: 14, color: backgroundColor),
            const SizedBox(width: 4),
          ],
          Text(
            text,
            style: TextStyle(
              fontSize: 12,
              fontWeight: FontWeight.w500,
              color: textColor ?? backgroundColor,
            ),
          ),
        ],
      ),
    );
  }
  
  /// Floating Action Button with Spatial Design
  static Widget spatialFAB({
    required VoidCallback onPressed,
    required IconData icon,
    Color? backgroundColor,
    Color? iconColor,
    bool mini = false,
  }) {
    return Container(
      width: mini ? 48 : 56,
      height: mini ? 48 : 56,
      decoration: BoxDecoration(
        gradient: backgroundColor != null 
          ? LinearGradient(colors: [backgroundColor, backgroundColor])
          : SpatialTheme.primaryGradient,
        borderRadius: BorderRadius.circular(mini ? 24 : 28),
        boxShadow: SpatialTheme.elevatedShadow,
      ),
      child: Material(
        color: Colors.transparent,
        child: InkWell(
          onTap: onPressed,
          borderRadius: BorderRadius.circular(mini ? 24 : 28),
          child: Icon(
            icon,
            color: iconColor ?? Colors.white,
            size: mini ? 20 : 24,
          ),
        ),
      ),
    );
  }
  
  /// Loading Overlay
  static Widget loadingOverlay({
    required bool isLoading,
    required Widget child,
    String? loadingText,
  }) {
    return Stack(
      children: [
        child,
        if (isLoading)
          Container(
            color: Colors.black.withValues(alpha: 0.3),
            child: Center(
              child: glassContainer(
                padding: const EdgeInsets.all(24),
                child: Column(
                  mainAxisSize: MainAxisSize.min,
                  children: [
                    const CircularProgressIndicator(
                      valueColor: AlwaysStoppedAnimation<Color>(SpatialTheme.primaryBlue),
                    ),
                    if (loadingText != null) ...[
                      const SizedBox(height: 16),
                      Text(
                        loadingText,
                        style: const TextStyle(
                          fontSize: 14,
                          color: SpatialTheme.textSecondary,
                        ),
                      ),
                    ],
                  ],
                ),
              ),
            ),
          ),
      ],
    );
  }
  
  /// Bottom Sheet Container
  static Widget bottomSheetContainer({
    required Widget child,
    double? height,
  }) {
    return Container(
      height: height,
      decoration: const BoxDecoration(
        color: SpatialTheme.surfaceLight,
        borderRadius: BorderRadius.only(
          topLeft: Radius.circular(24),
          topRight: Radius.circular(24),
        ),
        boxShadow: [
          BoxShadow(
            color: Colors.black12,
            blurRadius: 20,
            offset: Offset(0, -4),
          ),
        ],
      ),
      child: Column(
        children: [
          const SizedBox(height: 8),
          Container(
            width: 40,
            height: 4,
            decoration: BoxDecoration(
              color: SpatialTheme.textTertiary.withValues(alpha: 0.3),
              borderRadius: BorderRadius.circular(2),
            ),
          ),
          const SizedBox(height: 16),
          Expanded(child: child),
        ],
      ),
    );
  }
  
  /// Background Container with Gradient
  static Widget backgroundContainer({
    required Widget child,
    Gradient? gradient,
  }) {
    return Container(
      decoration: BoxDecoration(
        gradient: gradient ?? SpatialTheme.backgroundGradient,
      ),
      child: child,
    );
  }
}


