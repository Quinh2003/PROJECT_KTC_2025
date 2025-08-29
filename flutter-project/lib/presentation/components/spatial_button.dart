import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'dart:ui';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';

class SpatialButton extends StatelessWidget {
  final String text;
  final VoidCallback onPressed;
  final IconData? iconData;
  final Color? backgroundColor;
  final Color? textColor;
  final bool isOutlined;
  final bool isGlass;
  final bool isGradient;
  final double? width;
  final double? height;
  final EdgeInsetsGeometry? padding;
  final Gradient? gradient;
  
  const SpatialButton({
    super.key,
    required this.text,
    required this.onPressed,
    this.iconData,
    this.backgroundColor,
    this.textColor,
    this.isOutlined = false,
    this.isGlass = false,
    this.isGradient = false,
    this.width,
    this.height,
    this.padding,
    this.gradient,
  });

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    if (isOutlined) {
      return OutlinedButton(
        onPressed: onPressed,
        style: OutlinedButton.styleFrom(
          padding: padding ?? const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
          shape: RoundedRectangleBorder(
            borderRadius: SpatialDesignSystem.borderRadiusMedium,
          ),
          side: BorderSide(
            color: backgroundColor ?? SpatialDesignSystem.primaryColor,
            width: 1.5,
          ),
          minimumSize: Size(width ?? 0, height ?? 0),
        ),
        child: Row(
          mainAxisSize: MainAxisSize.min,
          children: [
            if (iconData != null) ...[
              Icon(iconData, color: textColor ?? SpatialDesignSystem.primaryColor),
              const SizedBox(width: 8),
            ],
            Text(
              text,
              style: SpatialDesignSystem.buttonText.copyWith(
                color: textColor ?? SpatialDesignSystem.primaryColor,
              ),
            ),
          ],
        ),
      );
    }
    
    if (isGlass) {
      return GestureDetector(
        onTap: onPressed,
        child: GlassCard(
          padding: padding ?? const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
          width: width,
          height: height,
          borderRadius: SpatialDesignSystem.borderRadiusMedium,
          child: Row(
            mainAxisSize: MainAxisSize.min,
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              if (iconData != null) ...[
                Icon(iconData, color: textColor ?? SpatialDesignSystem.primaryColor),
                const SizedBox(width: 8),
              ],
              Text(
                text,
                style: SpatialDesignSystem.buttonText.copyWith(
                  color: textColor ?? (isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor),
                ),
              ),
            ],
          ),
        ),
      );
    }
    
    if (isGradient) {
      return GestureDetector(
        onTap: onPressed,
        child: Container(
          width: width,
          height: height,
          padding: padding ?? const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
          decoration: BoxDecoration(
            gradient: gradient ?? SpatialDesignSystem.primaryGradient,
            borderRadius: SpatialDesignSystem.borderRadiusMedium,
            boxShadow: [
              BoxShadow(
                color: (backgroundColor ?? SpatialDesignSystem.primaryColor).withValues(alpha: 0.3),
                blurRadius: 10,
                offset: const Offset(0, 4),
              ),
            ],
          ),
          child: Row(
            mainAxisSize: MainAxisSize.min,
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              if (iconData != null) ...[
                Icon(iconData, color: textColor ?? Colors.white),
                const SizedBox(width: 8),
              ],
              Text(
                text,
                style: SpatialDesignSystem.buttonText.copyWith(
                  color: textColor ?? Colors.white,
                ),
              ),
            ],
          ),
        ),
      );
    }
    
    return ElevatedButton(
      onPressed: onPressed,
      style: ElevatedButton.styleFrom(
        padding: padding ?? const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
        backgroundColor: backgroundColor ?? SpatialDesignSystem.primaryColor,
        foregroundColor: textColor ?? Colors.white,
        shape: RoundedRectangleBorder(
          borderRadius: SpatialDesignSystem.borderRadiusMedium,
        ),
        minimumSize: Size(width ?? 0, height ?? 0),
        elevation: 4,
      ),
      child: Row(
        mainAxisSize: MainAxisSize.min,
        children: [
          if (iconData != null) ...[
            Icon(iconData, color: textColor ?? Colors.white),
            const SizedBox(width: 8),
          ],
          Text(
            text,
            style: SpatialDesignSystem.buttonText.copyWith(
              color: textColor ?? Colors.white,
            ),
          ),
        ],
      ),
    );
  }
}
