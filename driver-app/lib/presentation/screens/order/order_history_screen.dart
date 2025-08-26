import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';

class OrderHistoryScreen extends StatelessWidget {
  const OrderHistoryScreen({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Scaffold(
      backgroundColor: isDark 
          ? SpatialDesignSystem.darkBackgroundColor 
          : SpatialDesignSystem.backgroundColor,
      appBar: AppBar(
        backgroundColor: isDark 
            ? SpatialDesignSystem.darkBackgroundColor 
            : SpatialDesignSystem.backgroundColor,
        title: TextCustom(
          text: 'Order History', 
          color: isDark
              ? SpatialDesignSystem.textDarkPrimaryColor
              : SpatialDesignSystem.textPrimaryColor,
        ),
        centerTitle: true,
        elevation: 0,
        leadingWidth: 80,
        leading: InkWell(
          onTap: () => Navigator.pop(context),
          child: Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              Icon(
                Icons.arrow_back_ios_new_rounded, 
                size: 19, 
                color: SpatialDesignSystem.primaryColor 
              ),
              TextCustom(
                text: 'Back', 
                fontSize: 17, 
                color: SpatialDesignSystem.primaryColor
              ),
            ],
          ),
        ),
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            _buildActivityFeed(context),
            const SizedBox(height: 24),
            _buildCompletedOrdersSection(context),
          ],
        ),
      ),
    );
  }

  Widget _buildActivityFeed(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          "Activity Feed",
          style: SpatialDesignSystem.subtitleLarge.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        const SizedBox(height: 16),
        GlassCard(
          padding: const EdgeInsets.all(20),
          child: Column(
            children: [
              _buildActivityItem(
                context,
                "Order Delivered",
                "You've successfully delivered order #ORD-2025-08-14-034",
                "30 minutes ago",
                Icons.check_circle,
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildActivityItem(
                context,
                "New Order Assigned",
                "Order #ORD-2025-08-14-055 has been assigned to you",
                "1 hour ago",
                Icons.assignment,
                SpatialDesignSystem.primaryColor,
              ),
              const Divider(),
              _buildActivityItem(
                context,
                "Break Time",
                "You took a 15 minutes break",
                "2 hours ago",
                Icons.free_breakfast,
                SpatialDesignSystem.warningColor,
              ),
              const Divider(),
              _buildActivityItem(
                context,
                "Route Started",
                "You've started Route #RT-2025-08-14-01",
                "2 hours 45 minutes ago",
                Icons.play_circle_filled,
                SpatialDesignSystem.accentColor,
              ),
              const Divider(),
              _buildActivityItem(
                context,
                "Order Delivered",
                "You've successfully delivered order #ORD-2025-08-14-029",
                "3 hours ago",
                Icons.check_circle,
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildActivityItem(
                context,
                "Order Delivered",
                "You've successfully delivered order #ORD-2025-08-14-018",
                "4 hours ago",
                Icons.check_circle,
                SpatialDesignSystem.successColor,
              ),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildActivityItem(
    BuildContext context,
    String title,
    String description,
    String time,
    IconData icon,
    Color color,
  ) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 12),
      child: Row(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Container(
            width: 40,
            height: 40,
            decoration: BoxDecoration(
              color: color.withOpacity(0.1),
              borderRadius: BorderRadius.circular(8),
            ),
            child: Center(
              child: Icon(icon, color: color, size: 20),
            ),
          ),
          const SizedBox(width: 12),
          Expanded(
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  title,
                  style: SpatialDesignSystem.subtitleSmall.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
                const SizedBox(height: 4),
                Text(
                  description,
                  style: SpatialDesignSystem.bodySmall.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                  ),
                ),
                const SizedBox(height: 4),
                Text(
                  time,
                  style: SpatialDesignSystem.captionText.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor.withOpacity(0.7)
                        : SpatialDesignSystem.textSecondaryColor.withOpacity(0.7),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildCompletedOrdersSection(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          "Completed Orders",
          style: SpatialDesignSystem.subtitleLarge.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        const SizedBox(height: 16),
        GlassCard(
          padding: const EdgeInsets.all(20),
          child: Column(
            children: [
              _buildOrderItem(
                context,
                "ORD-2025-08-14-034",
                "123 Le Loi Street, District 1",
                "Today, 11:45 AM",
                "Completed",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildOrderItem(
                context,
                "ORD-2025-08-14-029",
                "456 Nguyen Hue Street, District 1",
                "Today, 09:30 AM",
                "Completed",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildOrderItem(
                context,
                "ORD-2025-08-14-018",
                "789 Pasteur Street, District 3",
                "Today, 08:15 AM",
                "Completed",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildOrderItem(
                context,
                "ORD-2025-08-13-095",
                "321 Nam Ky Khoi Nghia Street, District 3",
                "Yesterday, 03:20 PM",
                "Completed",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildOrderItem(
                context,
                "ORD-2025-08-13-078",
                "654 Hai Ba Trung Street, District 1",
                "Yesterday, 01:45 PM",
                "Completed",
                SpatialDesignSystem.successColor,
              ),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildOrderItem(
    BuildContext context,
    String orderId,
    String address,
    String time,
    String status,
    Color statusColor,
  ) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return GestureDetector(
      onTap: () {
        Navigator.pushNamed(context, '/order-detail', arguments: orderId);
      },
      child: Padding(
        padding: const EdgeInsets.symmetric(vertical: 8),
        child: Row(
          children: [
            Container(
              width: 50,
              height: 50,
              decoration: BoxDecoration(
                color: statusColor.withOpacity(0.1),
                borderRadius: BorderRadius.circular(8),
                border: Border.all(
                  color: statusColor.withOpacity(0.3),
                  width: 1,
                ),
              ),
              child: Center(
                child: Icon(
                  Icons.inventory_2_outlined,
                  color: statusColor,
                ),
              ),
            ),
            const SizedBox(width: 12),
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    orderId,
                    style: SpatialDesignSystem.subtitleSmall.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                  ),
                  const SizedBox(height: 4),
                  Text(
                    address,
                    style: SpatialDesignSystem.bodySmall.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor,
                    ),
                    maxLines: 1,
                    overflow: TextOverflow.ellipsis,
                  ),
                  const SizedBox(height: 4),
                  Row(
                    children: [
                      Icon(
                        Icons.access_time,
                        size: 14,
                        color: isDark
                            ? SpatialDesignSystem.textDarkSecondaryColor
                            : SpatialDesignSystem.textSecondaryColor,
                      ),
                      const SizedBox(width: 4),
                      Text(
                        time,
                        style: SpatialDesignSystem.captionText.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkSecondaryColor
                              : SpatialDesignSystem.textSecondaryColor,
                        ),
                      ),
                      const SizedBox(width: 8),
                      Container(
                        padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
                        decoration: BoxDecoration(
                          color: statusColor.withOpacity(0.1),
                          borderRadius: BorderRadius.circular(4),
                        ),
                        child: Text(
                          status,
                          style: SpatialDesignSystem.captionText.copyWith(
                            color: statusColor,
                            fontWeight: FontWeight.w600,
                          ),
                        ),
                      ),
                    ],
                  ),
                ],
              ),
            ),
            Icon(
              Icons.chevron_right,
              color: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor
                  : SpatialDesignSystem.textSecondaryColor,
            ),
          ],
        ),
      ),
    );
  }
}
