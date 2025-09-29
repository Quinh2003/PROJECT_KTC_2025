import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/delivery.dart';
import 'package:ktc_logistics_driver/presentation/blocs/delivery/delivery_bloc.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/delivery_detail_screen.dart';
import 'package:intl/intl.dart';

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

    // Load deliveries using DeliveryBloc
    context.read<DeliveryBloc>().add(LoadDeliveriesEvent());
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
          style: SpatialDesignSystem.headingMedium.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        centerTitle: true,
        elevation: 0,
        automaticallyImplyLeading: false, // Remove back button
        actions: [
          IconButton(
            icon: Icon(
              Icons.refresh,
              color: isDark
                  ? SpatialDesignSystem.textDarkPrimaryColor
                  : SpatialDesignSystem.textPrimaryColor,
            ),
            onPressed: () {
              print('🔄 UI: Delivery refresh button pressed!');
              // Reload deliveries from API
              context.read<DeliveryBloc>().add(LoadDeliveriesEvent());
              print('🔄 UI: LoadDeliveriesEvent dispatched');
              
              // Show loading indicator
              ScaffoldMessenger.of(context).showSnackBar(
                const SnackBar(
                  content: Text('Refreshing deliveries...'),
                  duration: Duration(seconds: 1),
                ),
              );
            },
            tooltip: 'Reload',
          ),
        ],
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
      child: BlocBuilder<DeliveryBloc, DeliveryState>(
        builder: (context, state) {
          if (state is DeliveryLoading) {
            return const Center(child: CircularProgressIndicator());
          } else if (state is DeliveriesLoadedState) {
            return Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                _buildNextDeliveriesCard(context, state.deliveries),
              ],
            );
          } else if (state is DeliveriesEmptyState) {
            return Center(
              child: Column(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  const Icon(Icons.inbox_outlined,
                      size: 80, color: Colors.grey),
                  const SizedBox(height: 16),
                  Text(
                    'No deliveries found',
                    style: SpatialDesignSystem.subtitleLarge,
                  ),
                  const SizedBox(height: 8),
                  Text(
                    'There are no deliveries assigned to you yet',
                    style: SpatialDesignSystem.bodyMedium,
                    textAlign: TextAlign.center,
                  ),
                ],
              ),
            );
          } else if (state is DeliveryError) {
            return Center(child: Text(state.message));
          } else {
            // Initial state or unhandled state
            return const Center(child: Text('No deliveries available'));
          }
        },
      ),
    );
  }

  Widget _buildNextDeliveriesCard(
      BuildContext context, List<Delivery> deliveries) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    // Filter upcoming deliveries - kiểm tra theo statusDisplay thay vì status
    final upcomingDeliveries = deliveries
        .where((delivery) =>
            delivery.statusDisplay != 'Completed' &&
            delivery.statusDisplay != 'Failed' &&
            delivery.statusDisplay != 'Cancelled')
        .toList();

    // Debug để kiểm tra dữ liệu
    debugPrint('Total deliveries: ${deliveries.length}');
    debugPrint('Upcoming deliveries: ${upcomingDeliveries.length}');

    // Sort by scheduled time if available
    upcomingDeliveries.sort((a, b) {
      // Ưu tiên sử dụng scheduledTime, nếu không có thì dùng scheduleDeliveryTime
      final aTime = a.scheduledTime ?? a.scheduleDeliveryTime;
      final bTime = b.scheduledTime ?? b.scheduleDeliveryTime;

      if (aTime == null) return 1;
      if (bTime == null) return -1;
      return aTime.compareTo(bTime);
    });

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
          child: upcomingDeliveries.isEmpty
              ? const Center(child: Text('No upcoming deliveries'))
              : Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    for (int i = 0; i < upcomingDeliveries.length; i++) ...[
                      if (i > 0) const Divider(),
                      _buildDeliveryItem(
                        context,
                        "DEL-${upcomingDeliveries[i].id}",
                        _getDeliveryAddress(upcomingDeliveries[i]),
                        _formatDeliveryTime(
                            upcomingDeliveries[i].scheduledTime ??
                                upcomingDeliveries[i].scheduleDeliveryTime),
                        i == 0 ? "Next" : "Pending",
                      ),
                    ],
                  ],
                ),
        ),
      ],
    );
  }

  String _getDeliveryAddress(Delivery delivery) {
    // Sử dụng deliveryAddress field từ API response mới
    if (delivery.deliveryAddress != null &&
        delivery.deliveryAddress!.isNotEmpty) {
      return delivery.deliveryAddress!;
    }
    return 'No address available';
  }

  // Phương thức định dạng thời gian giao hàng
  String _formatDeliveryTime(String? timeString) {
    if (timeString == null) return 'No time';

    try {
      final dateTime = DateTime.parse(timeString);
      final format = DateFormat('h:mm a');
      return format.format(dateTime);
    } catch (e) {
      return timeString;
    }
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

    // Only "Next" deliveries have colored borders and icons
    final bool hasBorder = isNext;
    final Color iconColor = isNext 
        ? SpatialDesignSystem.primaryColor 
        : (isDark
            ? SpatialDesignSystem.textDarkSecondaryColor
            : SpatialDesignSystem.textSecondaryColor);

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
        padding: const EdgeInsets.symmetric(vertical: 10), // Tăng padding
        child: Row(
          children: [
            Container(
              width: 50,
              height: 70, // Tăng chiều cao để phù hợp với 3 dòng
              decoration: BoxDecoration(
                color: isNext
                    ? SpatialDesignSystem.primaryColor.withOpacity(0.1)
                    : (isDark
                        ? Colors.white.withOpacity(0.05)
                        : Colors.black.withOpacity(0.05)),
                borderRadius: BorderRadius.circular(8),
                border: hasBorder 
                    ? Border.all(
                        color: SpatialDesignSystem.primaryColor,
                        width: 2,
                      )
                    : null,
              ),
              child: Center(
                child: Icon(
                  Icons.local_shipping_outlined,
                  size: 30, // Tăng kích thước icon
                  color: iconColor,
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
                  const SizedBox(height: 6), // Tăng khoảng cách
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
                  const SizedBox(height: 6), // Tăng khoảng cách
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
      child: BlocBuilder<DeliveryBloc, DeliveryState>(
        builder: (context, state) {
          if (state is DeliveryLoading) {
            return const Center(child: CircularProgressIndicator());
          } else if (state is DeliveriesLoadedState) {
            return Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                _buildCompletedDeliveriesSection(context, state.deliveries),
              ],
            );
          } else if (state is DeliveryError) {
            return Center(child: Text(state.message));
          } else {
            // Initial state or unhandled state
            return const Center(child: Text('No delivery history available'));
          }
        },
      ),
    );
  }

  Widget _buildCompletedDeliveriesSection(
      BuildContext context, List<Delivery> deliveries) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    // Filter completed deliveries
    final completedDeliveries = deliveries
        .where((delivery) =>
            delivery.statusDisplay == 'Completed' ||
            delivery.statusDisplay == 'Failed' ||
            delivery.statusDisplay == 'Cancelled')
        .toList();

    // Sort by actual delivery time if available
    completedDeliveries.sort((a, b) {
      if (a.actualDeliveryTime == null) return 1;
      if (b.actualDeliveryTime == null) return -1;
      return b.actualDeliveryTime!.compareTo(a.actualDeliveryTime!);
    });

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
          child: completedDeliveries.isEmpty
              ? const Center(child: Text('No completed deliveries'))
              : Column(
                  children: [
                    for (int i = 0; i < completedDeliveries.length; i++) ...[
                      if (i > 0) const Divider(),
                      _buildDeliveryItem(
                        context,
                        "DEL-${completedDeliveries[i].id}",
                        _getDeliveryAddress(completedDeliveries[i]),
                        _formatDeliveryTimeWithDay(
                            completedDeliveries[i].actualDeliveryTime),
                        completedDeliveries[i].statusDisplay == 'Completed'
                            ? "Done"
                            : completedDeliveries[i].statusDisplay == 'Failed'
                                ? "Failed"
                                : "Cancelled",
                        completedDeliveries[i].statusDisplay == 'Completed'
                            ? SpatialDesignSystem.successColor
                            : SpatialDesignSystem.errorColor,
                      ),
                    ],
                  ],
                ),
        ),
      ],
    );
  }

  String _formatDeliveryTimeWithDay(String? timeString) {
    if (timeString == null) return 'No time';

    try {
      final dateTime = DateTime.parse(timeString);
      final now = DateTime.now();

      if (dateTime.year == now.year && dateTime.month == now.month) {
        if (dateTime.day == now.day) {
          return 'Today, ${DateFormat('h:mm a').format(dateTime)}';
        } else if (dateTime.day == now.day - 1) {
          return 'Yesterday, ${DateFormat('h:mm a').format(dateTime)}';
        }
      }

      return DateFormat('MMM dd, h:mm a').format(dateTime);
    } catch (e) {
      return timeString;
    }
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

    // Only show colored borders for completed or failed/cancelled items
    Border? border;
    Color iconColor;
    
    if (status == "Done") {
      // Green border for completed
      border = Border.all(
        color: SpatialDesignSystem.successColor,
        width: 2,
      );
      iconColor = SpatialDesignSystem.successColor;
    } else if (status == "Failed" || status == "Cancelled") {
      // Red border for failed or cancelled
      border = Border.all(
        color: SpatialDesignSystem.errorColor,
        width: 2,
      );
      iconColor = SpatialDesignSystem.errorColor;
    } else {
      // No border for pending items
      border = null;
      iconColor = isDark
          ? SpatialDesignSystem.textDarkSecondaryColor
          : SpatialDesignSystem.textSecondaryColor;
    }

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
        padding: const EdgeInsets.symmetric(vertical: 10), // Tăng padding
        child: Row(
          children: [
            Container(
              width: 50,
              height: 70, // Tăng chiều cao để phù hợp với 3 dòng
              decoration: BoxDecoration(
                color: isDark ? Colors.grey.shade800 : Colors.grey.shade200,
                borderRadius: BorderRadius.circular(8),
                border: border,
              ),
              child: Center(
                child: Icon(
                  Icons.local_shipping_outlined,
                  size: 26, // Tăng kích thước icon
                  color: iconColor,
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
                  const SizedBox(height: 6), // Tăng khoảng cách
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
                  const SizedBox(height: 6), // Tăng khoảng cách
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
              color: isDark ? Colors.grey.shade400 : Colors.grey.shade600,
            ),
          ],
        ),
      ),
    );
  }

  String _getDeliveryAddress(Delivery delivery) {
    // Sử dụng deliveryAddress field từ API response mới
    if (delivery.deliveryAddress != null &&
        delivery.deliveryAddress!.isNotEmpty) {
      return delivery.deliveryAddress!;
    }
    return 'No address available';
  }
}
