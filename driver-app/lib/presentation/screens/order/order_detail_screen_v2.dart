import 'package:flutter/material.dart';
import 'package:timeline_tile/timeline_tile.dart';
import 'dart:ui';

import '../../design/spatial_ui.dart';

class OrderDetailScreen extends StatefulWidget {
  final String orderId;
  
  const OrderDetailScreen({
    Key? key,
    required this.orderId,
  }) : super(key: key);

  @override
  State<OrderDetailScreen> createState() => _OrderDetailScreenState();
}

class _OrderDetailScreenState extends State<OrderDetailScreen> with SingleTickerProviderStateMixin {
  late TabController _tabController;
  
  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: 3, vsync: this);
    _tabController.addListener(() {
      setState(() {});
    });
  }
  
  @override
  void dispose() {
    _tabController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Scaffold(
      backgroundColor: isDark
          ? SpatialDesignSystem.darkBackgroundColor
          : SpatialDesignSystem.backgroundColor,
      appBar: AppBar(
        title: Text(
          "Order #${widget.orderId}",
          style: SpatialDesignSystem.subtitleLarge.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        backgroundColor: Colors.transparent,
        elevation: 0,
        leading: IconButton(
          icon: Icon(
            Icons.arrow_back,
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
          onPressed: () => Navigator.pop(context),
        ),
        actions: [
          IconButton(
            icon: const Icon(Icons.help_outline),
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
            onPressed: () {
              // Show help
            },
          ),
        ],
      ),
      body: Column(
        children: [
          // Status Card
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: _buildStatusCard(),
          ),
          
          // Tab Bar
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16),
            child: GlassCard(
              padding: const EdgeInsets.all(8),
              borderRadius: SpatialDesignSystem.borderRadiusMedium,
              child: TabBar(
                controller: _tabController,
                indicator: BoxDecoration(
                  borderRadius: SpatialDesignSystem.borderRadiusMedium,
                  color: SpatialDesignSystem.primaryColor,
                ),
                labelColor: Colors.white,
                unselectedLabelColor: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
                tabs: const [
                  Tab(text: "Details"),
                  Tab(text: "Route"),
                  Tab(text: "Timeline"),
                ],
              ),
            ),
          ),
          
          // Tab Content
          Expanded(
            child: TabBarView(
              controller: _tabController,
              children: [
                _buildDetailsTab(),
                _buildRouteTab(),
                _buildTimelineTab(),
              ],
            ),
          ),
        ],
      ),
      bottomNavigationBar: _buildBottomBar(),
    );
  }
  
  Widget _buildStatusCard() {
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
        children: [
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    widget.orderId,
                    style: SpatialDesignSystem.subtitleLarge.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                  ),
                  const SizedBox(height: 4),
                  Text(
                    "Estimated delivery: 12:30 PM",
                    style: SpatialDesignSystem.bodyMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor,
                    ),
                  ),
                ],
              ),
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
                decoration: BoxDecoration(
                  color: SpatialDesignSystem.warningColor.withValues(alpha: 0.1),
                  borderRadius: BorderRadius.circular(12),
                  border: Border.all(
                    color: SpatialDesignSystem.warningColor.withValues(alpha: 0.3),
                    width: 1,
                  ),
                ),
                child: Text(
                  "In Transit",
                  style: SpatialDesignSystem.captionText.copyWith(
                    color: SpatialDesignSystem.warningColor,
                    fontWeight: FontWeight.w600,
                  ),
                ),
              ),
            ],
          ),
          const SizedBox(height: 20),
          LinearProgressIndicator(
            value: 0.65,
            backgroundColor: isDark
                ? Colors.white.withValues(alpha: 0.1)
                : Colors.black.withValues(alpha: 0.05),
            valueColor: AlwaysStoppedAnimation<Color>(SpatialDesignSystem.primaryColor),
            borderRadius: BorderRadius.circular(10),
          ),
          const SizedBox(height: 10),
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Text(
                "Out for delivery",
                style: SpatialDesignSystem.captionText.copyWith(
                  color: SpatialDesignSystem.primaryColor,
                  fontWeight: FontWeight.w600,
                ),
              ),
              Text(
                "65% complete",
                style: SpatialDesignSystem.captionText.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
            ],
          ),
        ],
      ),
    );
  }
  
  Widget _buildDetailsTab() {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          // Customer Info
          GlassCard(
            padding: const EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  "Customer Information",
                  style: SpatialDesignSystem.subtitleMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
                const SizedBox(height: 16),
                _buildInfoRow(
                  Icons.person_outline,
                  "Customer Name",
                  "Nguyen Van A",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.phone_outlined,
                  "Phone Number",
                  "+84 123 456 789",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.email_outlined,
                  "Email Address",
                  "customer@example.com",
                ),
              ],
            ),
          ),
          
          const SizedBox(height: 16),
          
          // Delivery Info
          GlassCard(
            padding: const EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  "Delivery Information",
                  style: SpatialDesignSystem.subtitleMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
                const SizedBox(height: 16),
                _buildInfoRow(
                  Icons.location_on_outlined,
                  "Delivery Address",
                  "123 Nguyen Hue St, District 1, Ho Chi Minh City",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.access_time,
                  "Estimated Delivery Time",
                  "Today, 12:30 PM",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.notes_outlined,
                  "Delivery Instructions",
                  "Please call when you arrive. Ring doorbell twice.",
                ),
              ],
            ),
          ),
          
          const SizedBox(height: 16),
          
          // Order Details
          GlassCard(
            padding: const EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  "Order Details",
                  style: SpatialDesignSystem.subtitleMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
                const SizedBox(height: 16),
                _buildInfoRow(
                  Icons.inventory_2_outlined,
                  "Package Type",
                  "Standard Package",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.scale_outlined,
                  "Package Weight",
                  "2.5 kg",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.payments_outlined,
                  "Payment Method",
                  "Credit Card (Paid)",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.receipt_long_outlined,
                  "Order Date",
                  "August 14, 2025 | 09:15 AM",
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
  
  Widget _buildInfoRow(IconData icon, String label, String value) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 8),
      child: Row(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Icon(
            icon,
            size: 20,
            color: SpatialDesignSystem.primaryColor,
          ),
          const SizedBox(width: 12),
          Expanded(
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  label,
                  style: SpatialDesignSystem.captionText.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                  ),
                ),
                const SizedBox(height: 4),
                Text(
                  value,
                  style: SpatialDesignSystem.bodyMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
  
  Widget _buildRouteTab() {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          // Map Preview
          GlassCard(
            padding: const EdgeInsets.all(0),
            child: ClipRRect(
              borderRadius: SpatialDesignSystem.borderRadiusLarge,
              child: AspectRatio(
                aspectRatio: 16 / 9,
                child: Stack(
                  children: [
                    Image.asset(
                      "assets/google-map.png",
                      fit: BoxFit.cover,
                      width: double.infinity,
                    ),
                    Positioned(
                      bottom: 16,
                      right: 16,
                      child: SpatialButton(
                        text: "Navigate",
                        onPressed: () {
                          Navigator.pushNamed(
                            context,
                            '/route-map',
                            arguments: 'RT-2025-08-14-01',
                          );
                        },
                        iconData: Icons.navigation,
                        isGlass: true,
                      ),
                    ),
                  ],
                ),
              ),
            ),
          ),
          
          const SizedBox(height: 16),
          
          // Route Details
          GlassCard(
            padding: const EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  "Route Details",
                  style: SpatialDesignSystem.subtitleMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
                const SizedBox(height: 16),
                _buildInfoRow(
                  Icons.route,
                  "Route ID",
                  "RT-2025-08-14-01",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.timelapse,
                  "Estimated Travel Time",
                  "25 minutes",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.straighten,
                  "Distance",
                  "5.7 km",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.trending_up,
                  "Traffic Condition",
                  "Moderate",
                ),
              ],
            ),
          ),
          
          const SizedBox(height: 16),
          
          // Delivery Instructions
          GlassCard(
            padding: const EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  "Delivery Instructions",
                  style: SpatialDesignSystem.subtitleMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
                const SizedBox(height: 16),
                Container(
                  padding: const EdgeInsets.all(12),
                  decoration: BoxDecoration(
                    color: isDark
                        ? Colors.white.withValues(alpha: 0.05)
                        : Colors.black.withValues(alpha: 0.02),
                    borderRadius: BorderRadius.circular(8),
                    border: Border.all(
                      color: isDark
                          ? Colors.white.withValues(alpha: 0.1)
                          : Colors.black.withValues(alpha: 0.05),
                    ),
                  ),
                  child: Text(
                    "1. Navigate to the address\n"
                    "2. Call the customer upon arrival\n"
                    "3. Ring doorbell twice\n"
                    "4. Verify customer ID\n"
                    "5. Take photo of delivery\n"
                    "6. Get customer signature\n"
                    "7. Mark as delivered",
                    style: SpatialDesignSystem.bodyMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                      height: 1.5,
                    ),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
  
  Widget _buildTimelineTab() {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return ListView(
      padding: const EdgeInsets.all(16),
      children: [
        // Timeline
        GlassCard(
          padding: const EdgeInsets.all(16),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                "Delivery Timeline",
                style: SpatialDesignSystem.subtitleMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
              const SizedBox(height: 16),
              _buildTimelineTile(
                "Order Placed",
                "August 14, 2025 | 09:15 AM",
                "Order has been received and confirmed",
                true,
                isFirst: true,
              ),
              _buildTimelineTile(
                "Order Processed",
                "August 14, 2025 | 09:30 AM",
                "Order has been processed and prepared for shipping",
                true,
              ),
              _buildTimelineTile(
                "Out for Delivery",
                "August 14, 2025 | 11:45 AM",
                "Package is on the delivery vehicle and en route",
                true,
              ),
              _buildTimelineTile(
                "Arriving Soon",
                "Estimated: 12:30 PM",
                "Package is in your area and will be delivered soon",
                false,
              ),
              _buildTimelineTile(
                "Delivered",
                "Pending",
                "Package has been delivered successfully",
                false,
                isLast: true,
              ),
            ],
          ),
        ),
        
        const SizedBox(height: 16),
        
        // Delivery Notes
        GlassCard(
          padding: const EdgeInsets.all(16),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                "Delivery Notes",
                style: SpatialDesignSystem.subtitleMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
              const SizedBox(height: 16),
              SpatialTextField(
                label: "Add a note",
                hint: "Enter a note for this delivery",
                maxLines: 3,
                isGlass: true,
              ),
              const SizedBox(height: 16),
              Row(
                mainAxisAlignment: MainAxisAlignment.end,
                children: [
                  SpatialButton(
                    text: "Save Note",
                    onPressed: () {
                      // Save note logic
                    },
                    iconData: Icons.save,
                  ),
                ],
              ),
            ],
          ),
        ),
      ],
    );
  }
  
  Widget _buildTimelineTile(
    String title,
    String time,
    String description,
    bool isCompleted, {
    bool isFirst = false,
    bool isLast = false,
  }) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return TimelineTile(
      alignment: TimelineAlign.start,
      isFirst: isFirst,
      isLast: isLast,
      indicatorStyle: IndicatorStyle(
        width: 24,
        height: 24,
        indicator: Container(
          decoration: BoxDecoration(
            color: isCompleted
                ? SpatialDesignSystem.primaryColor
                : isDark
                    ? Colors.white.withValues(alpha: 0.1)
                    : Colors.black.withValues(alpha: 0.1),
            shape: BoxShape.circle,
            border: Border.all(
              color: isCompleted
                  ? SpatialDesignSystem.primaryColor
                  : Colors.transparent,
              width: 2,
            ),
          ),
          child: isCompleted
              ? const Icon(
                  Icons.check,
                  color: Colors.white,
                  size: 16,
                )
              : null,
        ),
      ),
      beforeLineStyle: LineStyle(
        color: isCompleted
            ? SpatialDesignSystem.primaryColor
            : isDark
                ? Colors.white.withValues(alpha: 0.1)
                : Colors.black.withValues(alpha: 0.1),
        thickness: 2,
      ),
      afterLineStyle: LineStyle(
        color: isDark
            ? Colors.white.withValues(alpha: 0.1)
            : Colors.black.withValues(alpha: 0.1),
        thickness: 2,
      ),
      endChild: Padding(
        padding: const EdgeInsets.only(left: 16, bottom: 24),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              title,
              style: SpatialDesignSystem.subtitleSmall.copyWith(
                color: isCompleted
                    ? SpatialDesignSystem.primaryColor
                    : (isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor),
                fontWeight: FontWeight.w600,
              ),
            ),
            const SizedBox(height: 4),
            Text(
              time,
              style: SpatialDesignSystem.captionText.copyWith(
                color: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
              ),
            ),
            const SizedBox(height: 4),
            Text(
              description,
              style: SpatialDesignSystem.bodySmall.copyWith(
                color: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor.withValues(alpha: 0.8)
                    : SpatialDesignSystem.textSecondaryColor.withValues(alpha: 0.8),
              ),
            ),
          ],
        ),
      ),
    );
  }
  
  Widget _buildBottomBar() {
    return SafeArea(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Row(
          children: [
            Expanded(
              child: SpatialButton(
                text: "Call Customer",
                onPressed: () {
                  // Call customer logic
                },
                iconData: Icons.phone,
                isOutlined: true,
              ),
            ),
            const SizedBox(width: 16),
            Expanded(
              child: SpatialButton(
                text: "Mark as Delivered",
                onPressed: () {
                  // Mark as delivered logic
                  _showDeliveryConfirmationDialog();
                },
                iconData: Icons.check_circle,
                isGradient: true,
                gradient: SpatialDesignSystem.successGradient,
              ),
            ),
          ],
        ),
      ),
    );
  }
  
  void _showDeliveryConfirmationDialog() {
    showDialog(
      context: context,
      builder: (context) => Dialog(
        backgroundColor: Colors.transparent,
        child: GlassCard(
          padding: const EdgeInsets.all(24),
          child: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Icon(
                Icons.check_circle,
                color: SpatialDesignSystem.successColor,
                size: 64,
              ),
              const SizedBox(height: 16),
              Text(
                "Confirm Delivery",
                style: SpatialDesignSystem.headingSmall,
              ),
              const SizedBox(height: 8),
              Text(
                "Are you sure you want to mark this order as delivered?",
                style: SpatialDesignSystem.bodyMedium,
                textAlign: TextAlign.center,
              ),
              const SizedBox(height: 24),
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  SpatialButton(
                    text: "Cancel",
                    onPressed: () {
                      Navigator.pop(context);
                    },
                    isOutlined: true,
                  ),
                  SpatialButton(
                    text: "Confirm",
                    onPressed: () {
                      Navigator.pop(context);
                      Navigator.pop(context);
                    },
                    isGradient: true,
                    gradient: SpatialDesignSystem.successGradient,
                  ),
                ],
              ),
            ],
          ),
        ),
      ),
    );
  }
}
