import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/delivery_detail_screen.dart';

class DeliveriesScreen extends StatefulWidget {
  const DeliveriesScreen({super.key});

  @override
  State<DeliveriesScreen> createState() => _DeliveriesScreenState();
}

class _DeliveriesScreenState extends State<DeliveriesScreen>
    with SingleTickerProviderStateMixin {
  late TabController _tabController;

  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: 2, vsync: this);
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
        backgroundColor: isDark
            ? SpatialDesignSystem.darkBackgroundColor
            : SpatialDesignSystem.backgroundColor,
        title: Text(
          'Deliveries',
          style: SpatialDesignSystem.subtitleLarge.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        centerTitle: true,
        elevation: 0,
        automaticallyImplyLeading: false, // Remove back button
        bottom: TabBar(
          controller: _tabController,
          indicatorColor: SpatialDesignSystem.primaryColor,
          labelColor: SpatialDesignSystem.primaryColor,
          unselectedLabelColor: isDark
              ? SpatialDesignSystem.textDarkSecondaryColor
              : SpatialDesignSystem.textSecondaryColor,
          tabs: const [
            Tab(text: "Upcoming"),
            Tab(text: "History"),
          ],
        ),
      ),
      body: TabBarView(
        controller: _tabController,
        children: const [
          UpcomingDeliveriesTab(),
          DeliveryHistoryTab(),
        ],
      ),
    );
  }
}

class UpcomingDeliveriesTab extends StatelessWidget {
  const UpcomingDeliveriesTab({super.key});

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          _buildNextDeliveriesCard(context),
        ],
      ),
    );
  }

  Widget _buildNextDeliveriesCard(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          "Upcoming Deliveries",
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
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              // Row(
              //   mainAxisAlignment: MainAxisAlignment.spaceBetween,
              //   children: [
              //     Text(
              //       "Upcoming Deliveries",
              //       style: SpatialDesignSystem.subtitleMedium.copyWith(
              //         color: isDark
              //             ? SpatialDesignSystem.textDarkPrimaryColor
              //             : SpatialDesignSystem.textPrimaryColor,
              //       ),
              //     ),
              //     Text(
              //       "3 Remaining",
              //       style: SpatialDesignSystem.captionText.copyWith(
              //         color: SpatialDesignSystem.warningColor,
              //         fontWeight: FontWeight.w600,
              //       ),
              //     ),
              //   ],
              // ),
              // const SizedBox(height: 16),
              _buildDeliveryItem(
                context,
                "DEL-2025-09-01-042",
                "123 Nguyen Hue St, District 1",
                "12:30 PM",
                "Next",
              ),
              const Divider(),
              _buildDeliveryItem(
                context,
                "DEL-2025-09-01-055",
                "456 Le Loi St, District 1",
                "1:15 PM",
                "Pending",
              ),
              const Divider(),
              _buildDeliveryItem(
                context,
                "DEL-2025-09-01-063",
                "789 Vo Van Tan St, District 3",
                "2:00 PM",
                "Pending",
              ),
              const Divider(),
              _buildDeliveryItem(
                context,
                "DEL-2025-09-01-068",
                "258 Dinh Tien Hoang St, District 1",
                "3:30 PM",
                "Pending",
              ),
              const Divider(),
              _buildDeliveryItem(
                context,
                "DEL-2025-09-01-072",
                "147 Nguyen Dinh Chieu St, District 3",
                "4:15 PM",
                "Pending",
              ),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildDeliveryItem(
    BuildContext context,
    String deliveryId,
    String address,
    String time,
    String status,
  ) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    final isNext = status == "Next";

    return GestureDetector(
      onTap: () {
        // Navigate to delivery details with mock data
        Navigator.push(
          context,
          MaterialPageRoute(
            builder: (context) => DeliveryDetailScreen(
              deliveryId: deliveryId,
            ),
          ),
        );
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
                    ? SpatialDesignSystem.primaryColor.withOpacity(0.1)
                    : (isDark
                        ? Colors.white.withOpacity(0.05)
                        : Colors.black.withOpacity(0.05)),
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
                  Icons.local_shipping_outlined,
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
                    deliveryId,
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
                        padding: const EdgeInsets.symmetric(
                            horizontal: 8, vertical: 2),
                        decoration: BoxDecoration(
                          color: isNext
                              ? SpatialDesignSystem.primaryColor
                                  .withOpacity(0.1)
                              : SpatialDesignSystem.warningColor
                                  .withOpacity(0.1),
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
}

class DeliveryHistoryTab extends StatelessWidget {
  const DeliveryHistoryTab({super.key});

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          _buildCompletedDeliveriesSection(context),
        ],
      ),
    );
  }

  Widget _buildCompletedDeliveriesSection(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          "Completed Deliveries",
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
              _buildDeliveryItem(
                context,
                "DEL-2025-08-14-034",
                "123 Le Loi Street, District 1",
                "Today, 11:45 AM",
                "Done",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildDeliveryItem(
                context,
                "DEL-2025-08-14-029",
                "456 Nguyen Hue Street, District 1",
                "Today, 09:30 AM",
                "Done",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildDeliveryItem(
                context,
                "DEL-2025-08-14-018",
                "789 Pasteur Street, District 3",
                "Today, 08:15 AM",
                "Done",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildDeliveryItem(
                context,
                "DEL-2025-08-13-095",
                "321 Nam Ky Khoi Nghia Street, District 3",
                "Yesterday, 03:20 PM",
                "Failed",
                SpatialDesignSystem.errorColor,
              ),
              const Divider(),
              _buildDeliveryItem(
                context,
                "DEL-2025-08-13-078",
                "654 Hai Ba Trung Street, District 1",
                "Yesterday, 01:45 PM",
                "Done",
                SpatialDesignSystem.successColor,
              ),
            ],
          ),
        ),
      ],
    );
  }
  
  Widget _buildDeliveryItem(
    BuildContext context,
    String deliveryId,
    String destination,
    String timestamp,
    String status,
    Color statusColor,
  ) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return GestureDetector(
      onTap: () {
        // Navigate to delivery details with mock data
        Navigator.push(
          context,
          MaterialPageRoute(
            builder: (context) => DeliveryDetailScreen(
              deliveryId: deliveryId,
            ),
          ),
        );
      },
      child: Padding(
        padding: const EdgeInsets.symmetric(vertical: 8),
        child: Row(
          children: [
            Container(
              width: 50,
              height: 50,
              decoration: BoxDecoration(
                color: isDark
                    ? Colors.grey.shade800
                    : Colors.grey.shade200,
                borderRadius: BorderRadius.circular(8),
              ),
              child: Center(
                child: Icon(
                  Icons.local_shipping_outlined,
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                  size: 20,
                ),
              ),
            ),
            const SizedBox(width: 12),
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    deliveryId,
                    style: SpatialDesignSystem.bodyMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                      fontWeight: FontWeight.w600,
                    ),
                  ),
                  const SizedBox(height: 4),
                  Text(
                    destination,
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
                        timestamp,
                        style: SpatialDesignSystem.captionText.copyWith(
                          color: isDark
                              ? Colors.grey.shade400
                              : Colors.grey.shade600,
                        ),
                      ),
                      const SizedBox(width: 8),
                      Container(
                        padding: const EdgeInsets.symmetric(
                          horizontal: 8,
                          vertical: 2,
                        ),
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
                  ? Colors.grey.shade400
                  : Colors.grey.shade600,
            ),
          ],
        ),
      ),
    );
  }
}
