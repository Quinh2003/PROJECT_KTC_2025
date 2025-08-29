import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'dart:ui';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';

// Custom TextField
class SpatialTextField extends StatelessWidget {
  final String label;
  final String? hint;
  final TextEditingController? controller;
  final bool obscureText;
  final TextInputType keyboardType;
  final Widget? prefix;
  final Widget? suffix;
  final String? Function(String?)? validator;
  final void Function(String)? onChanged;
  final bool isGlass;
  final int? maxLines;
  final bool enabled;
  
  const SpatialTextField({
    super.key,
    required this.label,
    this.hint,
    this.controller,
    this.obscureText = false,
    this.keyboardType = TextInputType.text,
    this.prefix,
    this.suffix,
    this.validator,
    this.onChanged,
    this.isGlass = false,
    this.maxLines = 1,
    this.enabled = true,
  });

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    if (isGlass) {
      return Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            label,
            style: SpatialDesignSystem.subtitleSmall.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor
                  : SpatialDesignSystem.textSecondaryColor,
            ),
          ),
          const SizedBox(height: 8),
          ClipRRect(
            borderRadius: SpatialDesignSystem.borderRadiusMedium,
            child: BackdropFilter(
              filter: ImageFilter.blur(sigmaX: 5, sigmaY: 5),
              child: TextFormField(
                controller: controller,
                obscureText: obscureText,
                keyboardType: keyboardType,
                validator: validator,
                onChanged: onChanged,
                maxLines: maxLines,
                enabled: enabled,
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
                decoration: InputDecoration(
                  hintText: hint,
                  prefixIcon: prefix,
                  suffixIcon: suffix,
                  hintStyle: SpatialDesignSystem.bodyMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor.withValues(alpha: 0.5)
                        : SpatialDesignSystem.textSecondaryColor.withValues(alpha: 0.5),
                  ),
                  filled: true,
                  fillColor: isDark
                      ? Colors.white.withValues(alpha: 0.05)
                      : Colors.white.withValues(alpha: 0.3),
                  contentPadding: const EdgeInsets.all(16),
                  border: OutlineInputBorder(
                    borderRadius: SpatialDesignSystem.borderRadiusMedium,
                    borderSide: BorderSide(
                      color: isDark
                          ? Colors.white.withValues(alpha: 0.1)
                          : Colors.white.withValues(alpha: 0.4),
                      width: 1,
                    ),
                  ),
                  enabledBorder: OutlineInputBorder(
                    borderRadius: SpatialDesignSystem.borderRadiusMedium,
                    borderSide: BorderSide(
                      color: isDark
                          ? Colors.white.withValues(alpha: 0.1)
                          : Colors.white.withValues(alpha: 0.4),
                      width: 1,
                    ),
                  ),
                  focusedBorder: OutlineInputBorder(
                    borderRadius: SpatialDesignSystem.borderRadiusMedium,
                    borderSide: BorderSide(
                      color: SpatialDesignSystem.primaryColor,
                      width: 1.5,
                    ),
                  ),
                ),
              ),
            ),
          ),
        ],
      );
    }
    
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          label,
          style: SpatialDesignSystem.subtitleSmall.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkSecondaryColor
                : SpatialDesignSystem.textSecondaryColor,
          ),
        ),
        const SizedBox(height: 8),
        TextFormField(
          controller: controller,
          obscureText: obscureText,
          keyboardType: keyboardType,
          validator: validator,
          onChanged: onChanged,
          maxLines: maxLines,
          enabled: enabled,
          style: SpatialDesignSystem.bodyMedium.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
          decoration: InputDecoration(
            hintText: hint,
            prefixIcon: prefix,
            suffixIcon: suffix,
            hintStyle: SpatialDesignSystem.bodyMedium.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor.withValues(alpha: 0.5)
                  : SpatialDesignSystem.textSecondaryColor.withValues(alpha: 0.5),
            ),
            filled: true,
            fillColor: isDark
                ? SpatialDesignSystem.darkSurfaceColor
                : SpatialDesignSystem.backgroundColor,
            contentPadding: const EdgeInsets.all(16),
            border: OutlineInputBorder(
              borderRadius: SpatialDesignSystem.borderRadiusMedium,
              borderSide: BorderSide(
                color: isDark
                    ? Colors.white.withValues(alpha: 0.1)
                    : Colors.black.withValues(alpha: 0.1),
                width: 1,
              ),
            ),
            enabledBorder: OutlineInputBorder(
              borderRadius: SpatialDesignSystem.borderRadiusMedium,
              borderSide: BorderSide(
                color: isDark
                    ? Colors.white.withValues(alpha: 0.1)
                    : Colors.black.withValues(alpha: 0.1),
                width: 1,
              ),
            ),
            focusedBorder: OutlineInputBorder(
              borderRadius: SpatialDesignSystem.borderRadiusMedium,
              borderSide: BorderSide(
                color: SpatialDesignSystem.primaryColor,
                width: 1.5,
              ),
            ),
          ),
        ),
      ],
    );
  }
}

