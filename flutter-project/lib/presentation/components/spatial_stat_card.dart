import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';

// StatCard Component
class StatCard extends StatelessWidget {
  final String title;
  final String value;
  final IconData icon;
  final Color? iconColor;
  final Color? backgroundColor;
  final bool showArrow;
  final bool isPositive;
  final String? changePercentage;
  
  const StatCard({
    super.key,
    required this.title,
    required this.value,
    required this.icon,
    this.iconColor,
    this.backgroundColor,
    this.showArrow = false,
    this.isPositive = true,
    this.changePercentage,
  });

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return GlassCard(
      padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 12),
      backgroundColor: backgroundColor ?? (isDark
          ? Colors.white.withValues(alpha: 0.05)
          : Colors.white.withValues(alpha: 0.3)),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        children: [
          // Title and icon row
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Expanded(
                child: Text(
                  title,
                  style: SpatialDesignSystem.subtitleSmall.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                  ),
                  overflow: TextOverflow.ellipsis,
                ),
              ),
              const SizedBox(width: 8),
              Container(
                padding: const EdgeInsets.all(6),
                decoration: BoxDecoration(
                  color: (iconColor ?? SpatialDesignSystem.primaryColor).withValues(alpha: 0.1),
                  borderRadius: BorderRadius.circular(8),
                ),
                child: Icon(
                  icon,
                  color: iconColor ?? SpatialDesignSystem.primaryColor,
                  size: 18,
                ),
              ),
            ],
          ),
          
          // Value - centered and bigger
          Center(
            child: Padding(
              padding: const EdgeInsets.symmetric(vertical: 8),
              child: FittedBox(
                fit: BoxFit.scaleDown,
                child: Text(
                  value,
                  style: SpatialDesignSystem.headingLarge.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                    fontSize: 40,
                    fontWeight: FontWeight.bold,
                    height: 1.0, // Tighter line height
                  ),
                  textAlign: TextAlign.center,
                ),
              ),
            ),
          ),
          
          // Trend indicator
          if (showArrow && changePercentage != null)
            Container(
              width: double.infinity,
              alignment: Alignment.center,
              child: Row(
                mainAxisSize: MainAxisSize.min,
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Icon(
                    isPositive ? Icons.arrow_upward : Icons.arrow_downward,
                    color: isPositive ? SpatialDesignSystem.successColor : SpatialDesignSystem.errorColor,
                    size: 12,
                  ),
                  const SizedBox(width: 2),
                  Text(
                    '$changePercentage%',
                    style: SpatialDesignSystem.captionText.copyWith(
                      color: isPositive ? SpatialDesignSystem.successColor : SpatialDesignSystem.errorColor,
                      fontWeight: FontWeight.w600,
                      fontSize: 10,
                    ),
                  ),
                  const SizedBox(width: 4),
                  Text(
                    'from last month',
                    style: SpatialDesignSystem.captionText.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor,
                      fontSize: 9,
                    ),
                    overflow: TextOverflow.ellipsis,
                  ),
                ],
              ),
            ),
        ],
      ),
    );
  }
}
