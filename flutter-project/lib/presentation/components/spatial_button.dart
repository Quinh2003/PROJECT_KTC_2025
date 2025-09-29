import 'package:flutter/material.dart';
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
    // Cache theme brightness to avoid multiple calls
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    if (isOutlined) {
      return _buildOutlinedButton(isDark);
    }
    
    if (isGlass) {
      return _buildGlassButton(isDark);
    }
    
    if (isGradient) {
      return _buildGradientButton(isDark);
    }
    
    return _buildStandardButton(isDark);
  }

  Widget _buildOutlinedButton(bool isDark) {
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

  Widget _buildGlassButton(bool isDark) {
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

  Widget _buildGradientButton(bool isDark) {
    return Container(
      width: width,
      height: height,
      decoration: BoxDecoration(
        gradient: gradient ?? LinearGradient(
          colors: [
            backgroundColor ?? SpatialDesignSystem.primaryColor,
            (backgroundColor ?? SpatialDesignSystem.primaryColor).withValues(alpha: 0.8),
          ],
          begin: Alignment.topLeft,
          end: Alignment.bottomRight,
        ),
        borderRadius: SpatialDesignSystem.borderRadiusMedium,
        boxShadow: [
          BoxShadow(
            color: (backgroundColor ?? SpatialDesignSystem.primaryColor).withValues(alpha: 0.3),
            blurRadius: 8,
            offset: const Offset(0, 4),
          ),
        ],
      ),
      child: Material(
        color: Colors.transparent,
        child: InkWell(
          onTap: onPressed,
          borderRadius: SpatialDesignSystem.borderRadiusMedium,
          child: Container(
            padding: padding ?? const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
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
        ),
      ),
    );
  }

  Widget _buildStandardButton(bool isDark) {
    return ElevatedButton(
      onPressed: onPressed,
      style: ElevatedButton.styleFrom(
        padding: padding ?? const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
        backgroundColor: backgroundColor ?? SpatialDesignSystem.primaryColor,
        shape: RoundedRectangleBorder(
          borderRadius: SpatialDesignSystem.borderRadiusMedium,
        ),
        minimumSize: Size(width ?? 0, height ?? 0),
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
