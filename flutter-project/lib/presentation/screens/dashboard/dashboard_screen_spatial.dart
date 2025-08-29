import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'dart:ui';

import '../../design/spatial_ui.dart';
import '../../components/spatial_stat_card.dart';
import '../../components/spatial_button.dart';
import '../../components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import '../../components/delivery_charts.dart';
import '../../../services/googlemaps_service.dart';
import '../../../services/tracking_service.dart';

class DashboardScreenSpatial extends StatefulWidget {
  const DashboardScreenSpatial({super.key});

  @override
  State<DashboardScreenSpatial> createState() => _DashboardScreenSpatialState();
}

class _DashboardScreenSpatialState extends State<DashboardScreenSpatial> {
  int _selectedIndex = 0;
  bool _isSidebarExpanded = true;

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
                        // Welcome Section
                        _buildWelcomeSection(context),
                        const SizedBox(height: 24),

                        // Current Activity & Route
                        _buildCurrentActivitySection(context, isTablet),
                        const SizedBox(height: 24),

                        Text(
                          "Delivery Analytics",
                          style: SpatialDesignSystem.subtitleLarge.copyWith(
                            color: isDark
                                ? SpatialDesignSystem.textDarkPrimaryColor
                                : SpatialDesignSystem.textPrimaryColor,
                          ),
                        ),

                        // Stats Grid
                        _buildStatsGrid(context, isTablet),
                        const SizedBox(height: 24),

                        // Analytics Charts
                        _buildAnalyticsCharts(context, isTablet),
                        const SizedBox(height: 24),
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
      bottomNavigationBar: !isTablet ? _buildBottomNav() : null,
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
          _buildNavItem(context, 1, Icons.local_shipping, "Deliveries"),
          _buildNavItem(context, 2, Icons.account_circle, "Profile"),

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

  Widget _buildNavItem(
      BuildContext context, int index, IconData icon, String label) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    final isSelected = index == _selectedIndex;

    return GestureDetector(
      onTap: () {
        setState(() {
          _selectedIndex = index;
        });

        // Thêm điều hướng đến trang tương ứng
        switch (index) {
          case 0: // Dashboard - đã ở trang hiện tại
            break;
          case 1: // Deliveries
            Navigator.pushNamed(context, '/deliveries');
            break;
          case 2: // Profile
            Navigator.pushNamed(context, '/profile');
            break;
        }
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
          GestureDetector(
            onTap: () {
              // Mở trang thông báo
              ScaffoldMessenger.of(context).showSnackBar(SnackBar(
                content: Text('Tính năng thông báo đang được phát triển'),
                duration: Duration(seconds: 2),
              ));
            },
            child: Container(
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
          ),

          // Add space
          const SizedBox(width: 16),

          // Avatar
          GestureDetector(
            onTap: () {
              // Navigate to profile
              Navigator.pushNamed(context, '/profile');
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
              // Cải thiện tiêu đề với phong cách hấp dẫn hơn và giảm kích thước
              RichText(
                text: TextSpan(
                  children: [
                    TextSpan(
                      text: "Good Morning,\n",
                      style: SpatialDesignSystem.headingSmall.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkPrimaryColor
                            : SpatialDesignSystem.textPrimaryColor,
                      ),
                    ),
                    TextSpan(
                      text: "Việt Hùng",
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
      crossAxisSpacing: 12,
      mainAxisSpacing: 12,
      childAspectRatio: 1.2,
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
          showArrow: true,
          isPositive: false,
          changePercentage: "3",
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
        _buildCurrentRouteCard(context),
      ],
    );
  }

  Widget _buildAnalyticsCharts(BuildContext context, bool isTablet) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        GlassCard(
          padding: const EdgeInsets.all(20),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                "Weekly Delivery Trend",
                style: SpatialDesignSystem.subtitleMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
              const SizedBox(height: 16),
              DeliveryAreaChart(isDark: isDark),
              const SizedBox(height: 16),
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Expanded(
                    child: Container(
                      padding: const EdgeInsets.all(8),
                      decoration: BoxDecoration(
                        color: SpatialDesignSystem.primaryColor
                            .withValues(alpha: 0.1),
                        borderRadius: BorderRadius.circular(8),
                        border: Border.all(
                          color: SpatialDesignSystem.primaryColor
                              .withValues(alpha: 0.3),
                          width: 1,
                        ),
                      ),
                      child: Column(
                        children: [
                          Text(
                            "Average",
                            style: SpatialDesignSystem.captionText.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                            ),
                          ),
                          Text(
                            "23 deliveries",
                            style: SpatialDesignSystem.bodyMedium.copyWith(
                              fontWeight: FontWeight.bold,
                              color: isDark
                                  ? SpatialDesignSystem.textDarkPrimaryColor
                                  : SpatialDesignSystem.textPrimaryColor,
                            ),
                          ),
                        ],
                      ),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: Container(
                      padding: const EdgeInsets.all(8),
                      decoration: BoxDecoration(
                        color: SpatialDesignSystem.successColor
                            .withValues(alpha: 0.1),
                        borderRadius: BorderRadius.circular(8),
                        border: Border.all(
                          color: SpatialDesignSystem.successColor
                              .withValues(alpha: 0.3),
                          width: 1,
                        ),
                      ),
                      child: Column(
                        children: [
                          Text(
                            "Growth",
                            style: SpatialDesignSystem.captionText.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                            ),
                          ),
                          Text(
                            "+12.5%",
                            style: SpatialDesignSystem.bodyMedium.copyWith(
                              fontWeight: FontWeight.bold,
                              color: SpatialDesignSystem.successColor,
                            ),
                          ),
                        ],
                      ),
                    ),
                  ),
                ],
              ),
            ],
          ),
        ),
        const SizedBox(height: 16),
        isTablet
            ? Row(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Expanded(
                    child: _buildPieChartCard(context),
                  ),
                  const SizedBox(width: 16),
                  Expanded(
                    child: _buildScatterChartCard(context),
                  ),
                ],
              )
            : Column(
                children: [
                  _buildPieChartCard(context),
                  const SizedBox(height: 16),
                  _buildScatterChartCard(context),
                ],
              ),
      ],
    );
  }

  Widget _buildPieChartCard(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return GlassCard(
      padding: const EdgeInsets.all(20),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            "Delivery Types",
            style: SpatialDesignSystem.subtitleMedium.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkPrimaryColor
                  : SpatialDesignSystem.textPrimaryColor,
            ),
          ),
          const SizedBox(height: 16),
          DeliveryTypePieChart(isDark: isDark),
          const SizedBox(height: 16),
          Row(
            children: [
              _buildPieChartLegend(
                context,
                SpatialDesignSystem.primaryColor,
                "Regular",
              ),
              const SizedBox(width: 8),
              _buildPieChartLegend(
                context,
                SpatialDesignSystem.accentColor,
                "Express",
              ),
              const SizedBox(width: 8),
              _buildPieChartLegend(
                context,
                SpatialDesignSystem.warningColor,
                "Overnight",
              ),
              const SizedBox(width: 8),
              _buildPieChartLegend(
                context,
                SpatialDesignSystem.successColor,
                "Premium",
              ),
            ],
          ),
        ],
      ),
    );
  }

  Widget _buildPieChartLegend(BuildContext context, Color color, String label) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Expanded(
      child: Row(
        children: [
          Container(
            width: 12,
            height: 12,
            decoration: BoxDecoration(
              color: color,
              shape: BoxShape.circle,
            ),
          ),
          const SizedBox(width: 4),
          Expanded(
            child: Text(
              label,
              overflow: TextOverflow.ellipsis,
              style: SpatialDesignSystem.captionText.copyWith(
                color: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildScatterChartCard(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return GlassCard(
      padding: const EdgeInsets.all(20),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            "Delivery Distribution",
            style: SpatialDesignSystem.subtitleMedium.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkPrimaryColor
                  : SpatialDesignSystem.textPrimaryColor,
            ),
          ),
          const SizedBox(height: 8),
          Text(
            "Time vs. Distance correlation",
            style: SpatialDesignSystem.captionText.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor
                  : SpatialDesignSystem.textSecondaryColor,
            ),
          ),
          const SizedBox(height: 16),
          DeliveryScatterChart(isDark: isDark),
        ],
      ),
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
                padding:
                    const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
                decoration: BoxDecoration(
                  color:
                      SpatialDesignSystem.successColor.withValues(alpha: 0.1),
                  borderRadius: BorderRadius.circular(12),
                  border: Border.all(
                    color:
                        SpatialDesignSystem.successColor.withValues(alpha: 0.3),
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
              const Icon(Icons.timer_outlined,
                  color: SpatialDesignSystem.accentColor),
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
              const Icon(Icons.local_shipping_outlined,
                  color: SpatialDesignSystem.warningColor),
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
          // Row(
          //   children: [
          //     Expanded(
          //       child: Column(
          //         crossAxisAlignment: CrossAxisAlignment.start,
          //         children: [
          //           Text(
          //             "8/11",
          //             style: SpatialDesignSystem.headingSmall.copyWith(
          //               color: isDark
          //                   ? SpatialDesignSystem.textDarkPrimaryColor
          //                   : SpatialDesignSystem.textPrimaryColor,
          //             ),
          //           ),
          //           Text(
          //             "Deliveries Completed",
          //             style: SpatialDesignSystem.captionText.copyWith(
          //               color: isDark
          //                   ? SpatialDesignSystem.textDarkSecondaryColor
          //                   : SpatialDesignSystem.textSecondaryColor,
          //             ),
          //           ),
          //         ],
          //       ),
          //     ),
          //     Expanded(
          //       child: Column(
          //         crossAxisAlignment: CrossAxisAlignment.start,
          //         children: [
          //           Text(
          //             "23.4 km",
          //             style: SpatialDesignSystem.headingSmall.copyWith(
          //               color: isDark
          //                   ? SpatialDesignSystem.textDarkPrimaryColor
          //                   : SpatialDesignSystem.textPrimaryColor,
          //             ),
          //           ),
          //           Text(
          //             "Traveled Distance",
          //             style: SpatialDesignSystem.captionText.copyWith(
          //               color: isDark
          //                   ? SpatialDesignSystem.textDarkSecondaryColor
          //                   : SpatialDesignSystem.textSecondaryColor,
          //             ),
          //           ),
          //         ],
          //       ),
          //     ),
          //   ],
          // ),
          // const SizedBox(height: 20),
          LayoutBuilder(
            builder: (context, constraints) {
              return Row(
                children: [
                  SizedBox(
                    width: (constraints.maxWidth - 16) / 2,
                    child: SpatialButton(
                      text: "See Details",
                      textColor: SpatialDesignSystem.primaryColor,
                      onPressed: () {
                        Navigator.pushNamed(context, '/order-detail',
                            arguments: 'ORD-2025-08-14-042');
                      },
                      iconData: Icons.info_outline,
                      isGlass: false,
                      backgroundColor: SpatialDesignSystem.primaryColor
                          .withValues(alpha: 0.5),
                      isOutlined: true,
                      padding: const EdgeInsets.symmetric(
                          horizontal: 12, vertical: 16),
                    ),
                  ),
                  const SizedBox(width: 16),
                  SizedBox(
                    width: (constraints.maxWidth - 16) / 2,
                    child: SpatialButton(
                      text: "Navigation",
                      textColor: SpatialDesignSystem.successColor,
                      iconData: Icons.directions,
                      isGlass: false,
                      backgroundColor: SpatialDesignSystem.successColor
                          .withValues(alpha: 0.8),
                      isOutlined: true,
                      padding: const EdgeInsets.symmetric(
                          horizontal: 12, vertical: 16),
                      onPressed: () async {
                        try {
                          // Show loading indicator
                          ScaffoldMessenger.of(context).showSnackBar(
                            const SnackBar(
                              content: Row(
                                children: [
                                  SizedBox(
                                      width: 20,
                                      height: 20,
                                      child: CircularProgressIndicator(
                                          strokeWidth: 2, color: Colors.white)),
                                  SizedBox(width: 16),
                                  Text('Preparing navigation route...'),
                                ],
                              ),
                              duration: Duration(seconds: 2),
                            ),
                          );

                          final mapsService = GoogleMapsService();

                          // Start background location service for tracking
                          await LocationService()
                              .startBackgroundLocationService();

                          // Get route data for Google Maps
                          final routeData =
                              await mapsService.getDummyRouteData();

                          // Print the data types for debugging
                          print(
                              "PickupLocation type: ${routeData['pickupLocation'].runtimeType}");
                          print(
                              "DeliveryLocation type: ${routeData['deliveryLocation'].runtimeType}");

                          // Open Google Maps with route
                          final result =
                              await mapsService.openGoogleMapsWithRoute(
                            context: context,
                            pickupLocation: routeData['pickupLocation'],
                            transitPoints: routeData['transitPoints'],
                            deliveryLocation: routeData['deliveryLocation'],
                          );

                          if (!result && context.mounted) {
                            ScaffoldMessenger.of(context).showSnackBar(
                              const SnackBar(
                                content: Text(
                                    'Failed to open Google Maps. Please make sure it is installed.'),
                                backgroundColor: Colors.red,
                              ),
                            );
                          }
                        } catch (e) {
                          if (context.mounted) {
                            print("Error opening navigation: $e");
                            ScaffoldMessenger.of(context).showSnackBar(
                              SnackBar(
                                content: Text('Error opening navigation: $e'),
                                backgroundColor: Colors.red,
                              ),
                            );
                          }
                        }
                      },
                    ),
                  ),
                ],
              );
            },
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

  Widget _buildBottomNav() {
    return BottomNavigationBar(
      currentIndex: _selectedIndex,
      onTap: (index) {
        setState(() {
          _selectedIndex = index;
        });

        // Thêm điều hướng đến trang tương ứng
        switch (index) {
          case 0: // Dashboard - đã ở trang hiện tại
            break;
          case 1: // Deliveries - chuyển hướng đến màn hình gộp mới
            Navigator.pushNamed(context, '/deliveries');
            break;
          case 2: // Profile
            Navigator.pushNamed(context, '/profile');
            break;
        }
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
          icon: Icon(Icons.account_circle),
          label: "Profile",
        ),
      ],
    );
  }
}
