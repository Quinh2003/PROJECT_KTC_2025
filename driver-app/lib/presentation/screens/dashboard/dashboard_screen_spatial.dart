import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'dart:ui';

import '../../design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';

class DashboardScreenSpatial extends StatefulWidget {
  const DashboardScreenSpatial({Key? key}) : super(key: key);

  @override
  State<DashboardScreenSpatial> createState() => _DashboardScreenSpatialState();
}

class _DashboardScreenSpatialState extends State<DashboardScreenSpatial> {
  int _selectedIndex = 0;
  bool _isSidebarExpanded = true;
  // Thêm biến để hiển thị onboarding
  bool _showOnboarding = true; 

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    final size = MediaQuery.of(context).size;
    final isTablet = size.width > 768;

    return Scaffold(
      backgroundColor: isDark
          ? SpatialDesignSystem.darkBackgroundColor
          : SpatialDesignSystem.backgroundColor,
      body: Row(
        children: [
          // Sidebar Navigation
          if (isTablet)
            AnimatedContainer(
              duration: const Duration(milliseconds: 300),
              width: _isSidebarExpanded ? 250 : 80,
              child: _buildSidebar(context, isTablet),
            ),
          
          // Main Content
          Expanded(
            child: Column(
              children: [
                // Top App Bar
                _buildAppBar(context, isTablet),
                
                // Content Area
                Expanded(
                  child: SingleChildScrollView(
                    padding: const EdgeInsets.all(16),
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        // Hiển thị onboarding nếu cần
                        if (_showOnboarding)
                          _buildOnboarding(context),
                          
                        // Welcome Section
                        _buildWelcomeSection(context),
                        const SizedBox(height: 24),
                        
                        // Stats Grid
                        _buildStatsGrid(context, isTablet),
                        const SizedBox(height: 24),
                        
                        // Current Activity & Route
                        _buildCurrentActivitySection(context, isTablet),
                        const SizedBox(height: 24),
                        
                        // Activity Feed
                        _buildActivityFeed(context),
                      ],
                    ),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
      // Bottom Navigation for Mobile
      bottomNavigationBar: !isTablet
          ? _buildBottomNav()
          : null,
      floatingActionButton: FloatingActionButton(
        onPressed: () {
          _startTracking(context);
        },
        backgroundColor: SpatialDesignSystem.primaryColor,
        child: const Icon(Icons.location_on, color: Colors.white),
      ),
    );
  }

  Widget _buildSidebar(BuildContext context, bool isTablet) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return GlassCard(
      padding: EdgeInsets.zero,
      borderRadius: const BorderRadius.only(
        topRight: Radius.circular(16),
        bottomRight: Radius.circular(16),
      ),
      backgroundColor: isDark
          ? Colors.black.withValues(alpha: 0.3)
          : Colors.white.withValues(alpha: 0.6),
      child: Column(
        children: [
          const SizedBox(height: 24),
          // Logo
          _isSidebarExpanded
              ? Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Row(
                    children: [
                      Container(
                        width: 40,
                        height: 40,
                        decoration: BoxDecoration(
                          color: SpatialDesignSystem.primaryColor,
                          borderRadius: BorderRadius.circular(8),
                        ),
                        child: const Center(
                          child: Text(
                            "KTC",
                            style: TextStyle(
                              color: Colors.white,
                              fontWeight: FontWeight.bold,
                            ),
                          ),
                        ),
                      ),
                      const SizedBox(width: 12),
                      Text(
                        "KTC Logistics",
                        style: SpatialDesignSystem.headingSmall.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                        ),
                      ),
                    ],
                  ),
                )
              : Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Container(
                    width: 40,
                    height: 40,
                    decoration: BoxDecoration(
                      color: SpatialDesignSystem.primaryColor,
                      borderRadius: BorderRadius.circular(8),
                    ),
                    child: const Center(
                      child: Text(
                        "KTC",
                        style: TextStyle(
                          color: Colors.white,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ),
                  ),
                ),
          const SizedBox(height: 24),
          
          // Navigation Items
          _buildNavItem(context, 0, Icons.dashboard, "Dashboard"),
          _buildNavItem(context, 1, Icons.local_shipping, "My Deliveries"),
          _buildNavItem(context, 2, Icons.map, "Routes"),
          _buildNavItem(context, 3, Icons.history, "History"),
          _buildNavItem(context, 4, Icons.account_circle, "Profile"),
          
          const Spacer(),
          
          // Toggle Sidebar Size
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: GestureDetector(
              onTap: () {
                setState(() {
                  _isSidebarExpanded = !_isSidebarExpanded;
                });
              },
              child: Row(
                mainAxisAlignment: _isSidebarExpanded
                    ? MainAxisAlignment.end
                    : MainAxisAlignment.center,
                children: [
                  Icon(
                    _isSidebarExpanded
                        ? Icons.keyboard_double_arrow_left
                        : Icons.keyboard_double_arrow_right,
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                  ),
                  if (_isSidebarExpanded) ...[
                    const SizedBox(width: 8),
                    Text(
                      "Collapse",
                      style: SpatialDesignSystem.captionText.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkSecondaryColor
                            : SpatialDesignSystem.textSecondaryColor,
                      ),
                    ),
                  ],
                ],
              ),
            ),
          ),
          const SizedBox(height: 16),
        ],
      ),
    );
  }

  Widget _buildNavItem(BuildContext context, int index, IconData icon, String label) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    final isSelected = index == _selectedIndex;
    
    return GestureDetector(
      onTap: () {
        setState(() {
          _selectedIndex = index;
        });
      },
      child: Container(
        margin: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
        padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 12),
        decoration: BoxDecoration(
          color: isSelected
              ? SpatialDesignSystem.primaryColor.withValues(alpha: 0.1)
              : Colors.transparent,
          borderRadius: BorderRadius.circular(12),
          border: isSelected
              ? Border.all(color: SpatialDesignSystem.primaryColor, width: 1)
              : null,
        ),
        child: Row(
          children: [
            Icon(
              icon,
              color: isSelected
                  ? SpatialDesignSystem.primaryColor
                  : (isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor),
              size: 24,
            ),
            if (_isSidebarExpanded) ...[
              const SizedBox(width: 16),
              Text(
                label,
                style: SpatialDesignSystem.subtitleMedium.copyWith(
                  color: isSelected
                      ? SpatialDesignSystem.primaryColor
                      : (isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor),
                ),
              ),
            ],
          ],
        ),
      ),
    );
  }

  Widget _buildAppBar(BuildContext context, bool isTablet) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return GlassCard(
      padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 12),
      borderRadius: const BorderRadius.only(
        bottomLeft: Radius.circular(16),
        bottomRight: Radius.circular(16),
      ),
      backgroundColor: isDark
          ? Colors.black.withValues(alpha: 0.3)
          : Colors.white.withValues(alpha: 0.6),
      child: Row(
        children: [
          // Menu Icon for Mobile
          if (!isTablet)
            IconButton(
              icon: const Icon(Icons.menu),
              onPressed: () {
                // Open drawer or show modal bottom sheet with menu
              },
            ),
          
          // Page Title
          Text(
            "Dashboard",
            style: SpatialDesignSystem.headingSmall.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkPrimaryColor
                  : SpatialDesignSystem.textPrimaryColor,
            ),
          ),
          
          const Spacer(),
          
          // Notification Badge
          Container(
            padding: const EdgeInsets.all(8),
            decoration: BoxDecoration(
              color: isDark
                  ? Colors.white.withValues(alpha: 0.05)
                  : Colors.white.withValues(alpha: 0.3),
              borderRadius: BorderRadius.circular(12),
            ),
            child: Stack(
              children: [
                Icon(
                  Icons.notifications_outlined,
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
                Positioned(
                  right: 0,
                  top: 0,
                  child: Container(
                    width: 10,
                    height: 10,
                    decoration: BoxDecoration(
                      color: SpatialDesignSystem.errorColor,
                      shape: BoxShape.circle,
                      border: Border.all(
                        color: isDark
                            ? SpatialDesignSystem.darkBackgroundColor
                            : SpatialDesignSystem.backgroundColor,
                        width: 1.5,
                      ),
                    ),
                  ),
                ),
              ],
            ),
          ),
          
          const SizedBox(width: 16),
          
          // Avatar
          GestureDetector(
            onTap: () {
              // Navigate to profile
            },
            child: Container(
              padding: const EdgeInsets.all(2),
              decoration: BoxDecoration(
                border: Border.all(
                  color: SpatialDesignSystem.primaryColor,
                  width: 2,
                ),
                shape: BoxShape.circle,
              ),
              child: CircleAvatar(
                radius: 18,
                backgroundColor: isDark
                    ? Colors.white.withValues(alpha: 0.1)
                    : Colors.white.withValues(alpha: 0.8),
                child: Text(
                  "TN",
                  style: TextStyle(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.primaryColor,
                    fontWeight: FontWeight.bold,
                  ),
                ),
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildWelcomeSection(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Row(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Expanded(
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              // Cải thiện tiêu đề với phong cách hấp dẫn hơn
              RichText(
                text: TextSpan(
                  children: [
                    TextSpan(
                      text: "Good Morning,\n",
                      style: SpatialDesignSystem.headingMedium.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkPrimaryColor
                            : SpatialDesignSystem.textPrimaryColor,
                      ),
                    ),
                    TextSpan(
                      text: "Trần Ngọc",
                      style: SpatialDesignSystem.headingLarge.copyWith(
                        color: SpatialDesignSystem.primaryColor,
                        fontWeight: FontWeight.bold,
                      ),
                    ),
                  ],
                ),
              ),
              const SizedBox(height: 8),
              Text(
                "Your performance score is excellent! Keep up the good work.",
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              const SizedBox(height: 16),
              BlocBuilder<TrackingBloc, TrackingState>(
                builder: (context, state) {
                  if (state is TrackingActiveState) {
                    return Row(
                      children: [
                        Icon(
                          Icons.check_circle,
                          color: SpatialDesignSystem.successColor,
                          size: 20,
                        ),
                        const SizedBox(width: 8),
                        Text(
                          "Location tracking active",
                          style: SpatialDesignSystem.bodyMedium.copyWith(
                            color: SpatialDesignSystem.successColor,
                            fontWeight: FontWeight.w500,
                          ),
                        ),
                      ],
                    );
                  }
                  return Row(
                    children: [
                      Icon(
                        Icons.info_outline,
                        color: SpatialDesignSystem.warningColor,
                        size: 20,
                      ),
                      const SizedBox(width: 8),
                      Text(
                        "Location tracking inactive",
                        style: SpatialDesignSystem.bodyMedium.copyWith(
                          color: SpatialDesignSystem.warningColor,
                          fontWeight: FontWeight.w500,
                        ),
                      ),
                    ],
                  );
                },
              ),
            ],
          ),
        ),
        const SizedBox(width: 16),
        GlassCard(
          padding: const EdgeInsets.all(16),
          child: Column(
            children: [
              Container(
                width: 60,
                height: 60,
                decoration: BoxDecoration(
                  shape: BoxShape.circle,
                  gradient: SpatialDesignSystem.accentGradient,
                ),
                child: const Center(
                  child: Icon(
                    Icons.emoji_events,
                    color: Colors.white,
                    size: 30,
                  ),
                ),
              ),
              const SizedBox(height: 8),
              Text(
                "95%",
                style: SpatialDesignSystem.headingMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
              Text(
                "Delivery Score",
                style: SpatialDesignSystem.captionText.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildStatsGrid(BuildContext context, bool isTablet) {
    return GridView.count(
      crossAxisCount: isTablet ? 4 : 2,
      crossAxisSpacing: 16,
      mainAxisSpacing: 16,
      shrinkWrap: true,
      physics: const NeverScrollableScrollPhysics(),
      children: [
        StatCard(
          title: "Completed Today",
          value: "8",
          icon: Icons.check_circle_outline,
          iconColor: SpatialDesignSystem.successColor,
          showArrow: true,
          isPositive: true,
          changePercentage: "12",
        ),
        StatCard(
          title: "Pending",
          value: "3",
          icon: Icons.pending_actions,
          iconColor: SpatialDesignSystem.warningColor,
          showArrow: true,
          isPositive: false,
          changePercentage: "5",
        ),
        StatCard(
          title: "Total Distance",
          value: "42.5 km",
          icon: Icons.timeline,
          iconColor: SpatialDesignSystem.accentColor,
        ),
        StatCard(
          title: "Earnings",
          value: "\$124.50",
          icon: Icons.attach_money,
          iconColor: SpatialDesignSystem.primaryColor,
          showArrow: true,
          isPositive: true,
          changePercentage: "8",
        ),
      ],
    );
  }

  Widget _buildCurrentActivitySection(BuildContext context, bool isTablet) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          "Current Activity",
          style: SpatialDesignSystem.subtitleLarge.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        const SizedBox(height: 16),
        isTablet
            ? Row(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Expanded(child: _buildCurrentRouteCard(context)),
                  const SizedBox(width: 16),
                  Expanded(child: _buildNextDeliveriesCard(context)),
                ],
              )
            : Column(
                children: [
                  _buildCurrentRouteCard(context),
                  const SizedBox(height: 16),
                  _buildNextDeliveriesCard(context),
                ],
              ),
      ],
    );
  }

  Widget _buildCurrentRouteCard(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return GlassCard(
      padding: const EdgeInsets.all(20),
      gradient: LinearGradient(
        colors: [
          SpatialDesignSystem.primaryColor.withValues(alpha: 0.1),
          SpatialDesignSystem.accentColor.withValues(alpha: 0.05),
        ],
        begin: Alignment.topLeft,
        end: Alignment.bottomRight,
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Text(
                "Active Route",
                style: SpatialDesignSystem.subtitleMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
                decoration: BoxDecoration(
                  color: SpatialDesignSystem.successColor.withValues(alpha: 0.1),
                  borderRadius: BorderRadius.circular(12),
                  border: Border.all(
                    color: SpatialDesignSystem.successColor.withValues(alpha: 0.3),
                    width: 1,
                  ),
                ),
                child: Text(
                  "In Progress",
                  style: SpatialDesignSystem.captionText.copyWith(
                    color: SpatialDesignSystem.successColor,
                    fontWeight: FontWeight.w600,
                  ),
                ),
              ),
            ],
          ),
          const SizedBox(height: 16),
          Row(
            children: [
              const Icon(Icons.route, color: SpatialDesignSystem.primaryColor),
              const SizedBox(width: 8),
              Text(
                "Route #RT-2025-08-14-01",
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
            ],
          ),
          const SizedBox(height: 12),
          Row(
            children: [
              const Icon(Icons.timer_outlined, color: SpatialDesignSystem.accentColor),
              const SizedBox(width: 8),
              Text(
                "Started at 08:30 AM (2h 45m elapsed)",
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
            ],
          ),
          const SizedBox(height: 12),
          Row(
            children: [
              const Icon(Icons.local_shipping_outlined, color: SpatialDesignSystem.warningColor),
              const SizedBox(width: 8),
              Text(
                "Vehicle: Delivery Van #KTC-2025",
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
            ],
          ),
          const SizedBox(height: 20),
          Row(
            children: [
              Expanded(
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      "8/11",
                      style: SpatialDesignSystem.headingSmall.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkPrimaryColor
                            : SpatialDesignSystem.textPrimaryColor,
                      ),
                    ),
                    Text(
                      "Deliveries Completed",
                      style: SpatialDesignSystem.captionText.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkSecondaryColor
                            : SpatialDesignSystem.textSecondaryColor,
                      ),
                    ),
                  ],
                ),
              ),
              Expanded(
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      "23.4 km",
                      style: SpatialDesignSystem.headingSmall.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkPrimaryColor
                            : SpatialDesignSystem.textPrimaryColor,
                      ),
                    ),
                    Text(
                      "Traveled Distance",
                      style: SpatialDesignSystem.captionText.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkSecondaryColor
                            : SpatialDesignSystem.textSecondaryColor,
                      ),
                    ),
                  ],
                ),
              ),
            ],
          ),
          const SizedBox(height: 20),
          SpatialButton(
            text: "View Route Map",
            onPressed: () {
              Navigator.pushNamed(context, '/route-map', arguments: 'RT-2025-08-14-01');
            },
            iconData: Icons.map,
            isGlass: true,
            width: double.infinity,
          ),
        ],
      ),
    );
  }

  Widget _buildNextDeliveriesCard(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return GlassCard(
      padding: const EdgeInsets.all(20),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Text(
                "Next Deliveries",
                style: SpatialDesignSystem.subtitleMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
              Text(
                "3 Remaining",
                style: SpatialDesignSystem.captionText.copyWith(
                  color: SpatialDesignSystem.warningColor,
                  fontWeight: FontWeight.w600,
                ),
              ),
            ],
          ),
          const SizedBox(height: 16),
          _buildDeliveryItem(
            context,
            "ORD-2025-08-14-042",
            "123 Nguyen Hue St, District 1",
            "12:30 PM",
            "Next",
          ),
          const Divider(),
          _buildDeliveryItem(
            context,
            "ORD-2025-08-14-055",
            "456 Le Loi St, District 1",
            "1:15 PM",
            "Pending",
          ),
          const Divider(),
          _buildDeliveryItem(
            context,
            "ORD-2025-08-14-063",
            "789 Vo Van Tan St, District 3",
            "2:00 PM",
            "Pending",
          ),
          const SizedBox(height: 16),
          SpatialButton(
            text: "View All Deliveries",
            onPressed: () {
              // Navigate to deliveries page
              setState(() {
                _selectedIndex = 1;
              });
            },
            iconData: Icons.view_list,
            isOutlined: true,
            width: double.infinity,
          ),
        ],
      ),
    );
  }

  Widget _buildDeliveryItem(
    BuildContext context,
    String orderId,
    String address,
    String time,
    String status,
  ) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    final isNext = status == "Next";
    
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
                color: isNext
                    ? SpatialDesignSystem.primaryColor.withValues(alpha: 0.1)
                    : (isDark
                        ? Colors.white.withValues(alpha: 0.05)
                        : Colors.black.withValues(alpha: 0.05)),
                borderRadius: BorderRadius.circular(8),
                border: isNext
                    ? Border.all(
                        color: SpatialDesignSystem.primaryColor,
                        width: 1,
                      )
                    : null,
              ),
              child: Center(
                child: Icon(
                  Icons.shopping_bag_outlined,
                  color: isNext
                      ? SpatialDesignSystem.primaryColor
                      : (isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor),
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
                        color: isNext
                            ? SpatialDesignSystem.primaryColor
                            : (isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor),
                      ),
                      const SizedBox(width: 4),
                      Text(
                        time,
                        style: SpatialDesignSystem.captionText.copyWith(
                          color: isNext
                              ? SpatialDesignSystem.primaryColor
                              : (isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor),
                        ),
                      ),
                      const SizedBox(width: 8),
                      Container(
                        padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
                        decoration: BoxDecoration(
                          color: isNext
                              ? SpatialDesignSystem.primaryColor.withValues(alpha: 0.1)
                              : SpatialDesignSystem.warningColor.withValues(alpha: 0.1),
                          borderRadius: BorderRadius.circular(4),
                        ),
                        child: Text(
                          status,
                          style: SpatialDesignSystem.captionText.copyWith(
                            color: isNext
                                ? SpatialDesignSystem.primaryColor
                                : SpatialDesignSystem.warningColor,
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
              const SizedBox(height: 16),
              TextButton(
                onPressed: () {
                  // Navigate to activity history
                  setState(() {
                    _selectedIndex = 3;
                  });
                },
                child: Text(
                  "View All Activity",
                  style: SpatialDesignSystem.buttonText.copyWith(
                    color: SpatialDesignSystem.primaryColor,
                    fontSize: 14,
                  ),
                ),
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
              color: color.withValues(alpha: 0.1),
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
                        ? SpatialDesignSystem.textDarkSecondaryColor.withValues(alpha: 0.7)
                        : SpatialDesignSystem.textSecondaryColor.withValues(alpha: 0.7),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildBottomNav() {
    return BottomNavigationBar(
      currentIndex: _selectedIndex,
      onTap: (index) {
        setState(() {
          _selectedIndex = index;
        });
      },
      type: BottomNavigationBarType.fixed,
      backgroundColor: Theme.of(context).colorScheme.surface,
      selectedItemColor: SpatialDesignSystem.primaryColor,
      unselectedItemColor: SpatialDesignSystem.textSecondaryColor,
      items: const [
        BottomNavigationBarItem(
          icon: Icon(Icons.dashboard),
          label: "Dashboard",
        ),
        BottomNavigationBarItem(
          icon: Icon(Icons.local_shipping),
          label: "Deliveries",
        ),
        BottomNavigationBarItem(
          icon: Icon(Icons.map),
          label: "Routes",
        ),
        BottomNavigationBarItem(
          icon: Icon(Icons.history),
          label: "History",
        ),
        BottomNavigationBarItem(
          icon: Icon(Icons.account_circle),
          label: "Profile",
        ),
      ],
    );
  }

  // Thêm phương thức để hiển thị onboarding
  Widget _buildOnboarding(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Container(
      width: double.infinity,
      margin: const EdgeInsets.only(bottom: 24),
      padding: const EdgeInsets.all(24),
      decoration: BoxDecoration(
        gradient: LinearGradient(
          begin: Alignment.topLeft,
          end: Alignment.bottomRight,
          colors: isDark
              ? [
                  Color(0xFF1E1E2C),
                  Color(0xFF2D2D44),
                ]
              : [
                  Color(0xFF6A11CB),
                  Color(0xFF2575FC),
                ],
        ),
        borderRadius: BorderRadius.circular(24),
        boxShadow: [
          BoxShadow(
            color: Colors.black.withOpacity(0.2),
            blurRadius: 20,
            offset: Offset(0, 10),
          ),
        ],
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              Container(
                padding: const EdgeInsets.all(12),
                decoration: BoxDecoration(
                  color: Colors.white.withOpacity(0.2),
                  borderRadius: BorderRadius.circular(16),
                ),
                child: Icon(
                  Icons.waving_hand_rounded,
                  color: Colors.white,
                  size: 28,
                ),
              ),
              const SizedBox(width: 16),
              Expanded(
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      'Welcome to KTC Logistics',
                      style: TextStyle(
                        color: Colors.white70,
                        fontSize: 16,
                        fontWeight: FontWeight.w500,
                      ),
                    ),
                    const SizedBox(height: 4),
                    Text(
                      'Your Delivery Partner',
                      style: TextStyle(
                        color: Colors.white,
                        fontSize: 24,
                        fontWeight: FontWeight.bold,
                      ),
                      overflow: TextOverflow.ellipsis,
                    ),
                  ],
                ),
              ),
            ],
          ),
          const SizedBox(height: 24),
          Text(
            'Get ready for efficient and fast delivery routes',
            style: TextStyle(
              color: Colors.white.withOpacity(0.9),
              fontSize: 16,
              fontWeight: FontWeight.w500,
            ),
          ),
          const SizedBox(height: 32),
          ElevatedButton(
            onPressed: () {
              setState(() {
                _showOnboarding = false;
              });
            },
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.white,
              foregroundColor: isDark ? Color(0xFF2D2D44) : Color(0xFF6A11CB),
              padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 12),
              shape: RoundedRectangleBorder(
                borderRadius: BorderRadius.circular(12),
              ),
            ),
            child: const Text(
              'Get Started',
              style: TextStyle(
                fontWeight: FontWeight.bold,
                fontSize: 16,
              ),
            ),
          ),
        ],
      ),
    );
  }

  void _startTracking(BuildContext context) {
    final trackingBloc = BlocProvider.of<TrackingBloc>(context);
    
    trackingBloc.add(StartTrackingEvent(
      driverId: "driver_123",
      vehicleId: "vehicle_456",
      routeId: "RT-2025-08-14-01",
    ));
    
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Row(
          children: [
            const Icon(Icons.location_on, color: Colors.white),
            const SizedBox(width: 10),
            const Text('Location tracking started'),
          ],
        ),
        backgroundColor: SpatialDesignSystem.successColor,
        duration: const Duration(seconds: 2),
      ),
    );
  }
}
