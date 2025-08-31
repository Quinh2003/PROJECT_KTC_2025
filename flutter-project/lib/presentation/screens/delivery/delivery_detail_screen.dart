import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_button.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_text_field.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/screens/order/order_detail_screen.dart';
import 'package:timeline_tile/timeline_tile.dart';
import 'dart:ui';

// Tab chứa dữ liệu cấu hình
class DeliveryTab {
  final String text;
  final Widget Function() contentBuilder;

  DeliveryTab({required this.text, required this.contentBuilder});
}

class DeliveryDetailScreen extends StatefulWidget {
  final String deliveryId;

  const DeliveryDetailScreen({
    super.key,
    required this.deliveryId,
  });

  @override
  State<DeliveryDetailScreen> createState() => _DeliveryDetailScreenState();
}

class _DeliveryDetailScreenState extends State<DeliveryDetailScreen>
    with SingleTickerProviderStateMixin {
  late TabController _tabController;
  late List<DeliveryTab> _tabs;
  String _selectedStatus = 'Assigned'; // Trạng thái mặc định
  final _noteController = TextEditingController();
  bool _isUpdatingStatus = false;

  // Danh sách các trạng thái có thể có của một chuyến giao hàng
  final List<String> _statusOptions = [
    'Assigned',
    'Started',
    'In Progress',
    'Completed',
    'Cancelled',
  ];

  @override
  void initState() {
    super.initState();

    // Khởi tạo danh sách tab một lần duy nhất
    _tabs = [
      DeliveryTab(text: "Overview", contentBuilder: _buildOverviewTab),
      DeliveryTab(text: "Orders", contentBuilder: _buildOrdersTab),
    ];

    _tabController = TabController(length: _tabs.length, vsync: this);
    _tabController.addListener(() {
      setState(() {});
    });

    // TODO: Lấy dữ liệu chi tiết của chuyến giao hàng từ API
    _loadDeliveryDetails();
  }

  Future<void> _loadDeliveryDetails() async {
    // TODO: Implement API call to get delivery details
    setState(() {
      // Cập nhật trạng thái từ dữ liệu API
      _selectedStatus = 'In Progress'; // Ví dụ
    });
  }

  @override
  void dispose() {
    _tabController.dispose();
    _noteController.dispose();
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
          "Delivery #${widget.deliveryId}",
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
            child: TabBar(
              controller: _tabController,
              indicator: UnderlineTabIndicator(
                borderSide: BorderSide(
                  color: SpatialDesignSystem.primaryColor,
                ),
                insets: const EdgeInsets.symmetric(horizontal: 16.0),
              ),
              labelStyle: SpatialDesignSystem.bodyMedium.copyWith(
                fontWeight: FontWeight.w600,
              ),
              padding: const EdgeInsets.symmetric(vertical: 4),
              labelColor: SpatialDesignSystem.primaryColor,
              unselectedLabelColor: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor
                  : SpatialDesignSystem.textSecondaryColor,
              isScrollable: false,
              tabs: _tabs
                  .map((tab) => Tab(
                        text: tab.text,
                      ))
                  .toList(),
            ),
          ),

          // Tab Content
          Expanded(
            child: TabBarView(
              controller: _tabController,
              children: _tabs.map((tab) => tab.contentBuilder()).toList(),
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
            crossAxisAlignment: CrossAxisAlignment.center,
            children: [
              Text(
                "KTC Logistics",
                style: SpatialDesignSystem.headingSmall.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
              Container(
                padding:
                    const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
                decoration: BoxDecoration(
                  color: _getStatusColor().withOpacity(0.2),
                  borderRadius: BorderRadius.circular(20),
                ),
                child: Text(
                  _selectedStatus,
                  style: SpatialDesignSystem.bodySmall.copyWith(
                    color: _getStatusColor(),
                    fontWeight: FontWeight.w600,
                  ),
                ),
              ),
            ],
          ),
          const SizedBox(height: 8),
          Text(
            "Route #RT-${widget.deliveryId}",
            style: SpatialDesignSystem.bodyMedium.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor
                  : SpatialDesignSystem.textSecondaryColor,
            ),
          ),
          const SizedBox(height: 20),
          LinearProgressIndicator(
            value: _getProgressValue(),
            backgroundColor: isDark
                ? Colors.white.withValues(alpha: 0.1)
                : Colors.black.withValues(alpha: 0.05),
            valueColor:
                AlwaysStoppedAnimation<Color>(SpatialDesignSystem.primaryColor),
            borderRadius: BorderRadius.circular(10),
          ),
          const SizedBox(height: 10),
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Text(
                _getStatusDescription(),
                style: SpatialDesignSystem.captionText.copyWith(
                  fontWeight: FontWeight.w600,
                ),
              ),
              Text(
                "${(_getProgressValue() * 100).toInt()}% complete",
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

  Color _getStatusColor() {
    switch (_selectedStatus) {
      case 'Assigned':
        return Colors.blue;
      case 'Started':
        return Colors.orange;
      case 'In Progress':
        return SpatialDesignSystem.primaryColor;
      case 'Completed':
        return Colors.green;
      case 'Cancelled':
        return Colors.red;
      default:
        return Colors.grey;
    }
  }

  double _getProgressValue() {
    switch (_selectedStatus) {
      case 'Assigned':
        return 0.2;
      case 'Started':
        return 0.4;
      case 'In Progress':
        return 0.65;
      case 'Completed':
        return 1.0;
      case 'Cancelled':
        return 0.0;
      default:
        return 0.0;
    }
  }

  String _getStatusDescription() {
    switch (_selectedStatus) {
      case 'Assigned':
        return "Waiting to start";
      case 'Started':
        return "Journey started";
      case 'In Progress':
        return "In transit";
      case 'Completed':
        return "Delivery completed";
      case 'Cancelled':
        return "Delivery cancelled";
      default:
        return "Unknown status";
    }
  }

  Widget _buildOverviewTab() {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
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
                  Icons.calendar_today,
                  "Date",
                  "August 31, 2025",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.schedule,
                  "Estimated Time",
                  "2.5 hours",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.location_on,
                  "Starting Point",
                  "KTC Warehouse, 123 Industrial Zone, District 9",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.location_city,
                  "Destination Area",
                  "District 1, Ho Chi Minh City",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.route,
                  "Total Distance",
                  "25.3 km",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.shopping_bag,
                  "Total Orders",
                  "8 orders",
                ),
              ],
            ),
          ),

          const SizedBox(height: 16),

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
                  "Assigned to Driver",
                  "August 31, 2025 | 08:00 AM",
                  "Delivery route assigned to Nguyen Van Driver",
                  true,
                  isFirst: true,
                ),
                _buildTimelineTile(
                  "Started Journey",
                  "August 31, 2025 | 08:15 AM",
                  "Driver has started the delivery route",
                  true,
                ),
                _buildTimelineTile(
                  "In Progress",
                  "August 31, 2025 | 08:30 AM",
                  "Currently delivering orders in District 1",
                  true,
                ),
                _buildTimelineTile(
                  "Completed",
                  "Estimated August 31, 2025 | 11:00 AM",
                  "All orders delivered successfully",
                  false,
                  isLast: true,
                ),
              ],
            ),
          ),

          const SizedBox(height: 16),

          // Status Update
          GlassCard(
            padding: const EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  "Update Status",
                  style: SpatialDesignSystem.subtitleMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
                const SizedBox(height: 16),
                DropdownButtonFormField<String>(
                  value: _selectedStatus,
                  decoration: InputDecoration(
                    labelText: 'Status',
                    border: OutlineInputBorder(
                      borderRadius: BorderRadius.circular(10),
                    ),
                    filled: true,
                    fillColor: isDark
                        ? Colors.black.withValues(alpha: 0.2)
                        : Colors.white.withValues(alpha: 0.8),
                  ),
                  items: _statusOptions.map((status) {
                    return DropdownMenuItem<String>(
                      value: status,
                      child: Text(status),
                    );
                  }).toList(),
                  onChanged: (newValue) {
                    setState(() {
                      _selectedStatus = newValue!;
                    });
                  },
                ),
                const SizedBox(height: 16),
                SpatialTextField(
                  controller: _noteController,
                  label: "Notes",
                  hint: "Add notes about status change",
                  maxLines: 3,
                  isGlass: true,
                ),
                const SizedBox(height: 16),
                SpatialButton(
                  text: "Update Status",
                  onPressed: _isUpdatingStatus
                      ? () {}
                      : () {
                          setState(() {
                            _isUpdatingStatus = true;
                          });

                          // TODO: Implement API call to update status
                          Future.delayed(const Duration(seconds: 1)).then((_) {
                            if (mounted) {
                              ScaffoldMessenger.of(context).showSnackBar(
                                SnackBar(
                                  content: Text(
                                      'Status updated to $_selectedStatus'),
                                  backgroundColor:
                                      SpatialDesignSystem.successColor,
                                ),
                              );

                              setState(() {
                                _isUpdatingStatus = false;
                              });
                            }
                          }).catchError((e) {
                            if (mounted) {
                              ScaffoldMessenger.of(context).showSnackBar(
                                SnackBar(
                                  content: Text('Failed to update status: $e'),
                                  backgroundColor: Colors.red,
                                ),
                              );

                              setState(() {
                                _isUpdatingStatus = false;
                              });
                            }
                          });
                        },
                  isGlass: true,
                  width: double.infinity,
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
                color: isDark
                    ? SpatialDesignSystem.textDarkPrimaryColor
                    : SpatialDesignSystem.textPrimaryColor,
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
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildOrdersTab() {
    // TODO: Implement orders list from API
    // Mock data for now
    final mockOrders = [
      {
        'id': '1001',
        'customer': 'Nguyen Van A',
        'status': 'Pending',
        'amount': '150,000 VND'
      },
      {
        'id': '1002',
        'customer': 'Tran Thi B',
        'status': 'Delivered',
        'amount': '275,000 VND'
      },
      {
        'id': '1003',
        'customer': 'Le Van C',
        'status': 'In Transit',
        'amount': '320,000 VND'
      },
      {
        'id': '1004',
        'customer': 'Pham Thi D',
        'status': 'Pending',
        'amount': '180,000 VND'
      },
      {
        'id': '1005',
        'customer': 'Hoang Van E',
        'status': 'Pending',
        'amount': '430,000 VND'
      },
    ];

    final isDark = Theme.of(context).brightness == Brightness.dark;

    return ListView.builder(
      padding: const EdgeInsets.all(16),
      itemCount: mockOrders.length,
      itemBuilder: (context, index) {
        final order = mockOrders[index];
        return GestureDetector(
          onTap: () {
            Navigator.push(
              context,
              MaterialPageRoute(
                builder: (context) => OrderDetailScreen(orderId: order['id']!),
              ),
            );
          },
          child: Container(
            margin: const EdgeInsets.only(bottom: 16),
            child: GlassCard(
              padding: const EdgeInsets.all(16),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Row(
                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                    children: [
                      Text(
                        "Order #${order['id']}",
                        style: SpatialDesignSystem.subtitleSmall.copyWith(
                          color: SpatialDesignSystem.primaryColor,
                          fontWeight: FontWeight.w600,
                        ),
                      ),
                      Container(
                        padding: const EdgeInsets.symmetric(
                            horizontal: 8, vertical: 4),
                        decoration: BoxDecoration(
                          color: _getOrderStatusColor(order['status']!)
                              .withOpacity(0.2),
                          borderRadius: BorderRadius.circular(20),
                        ),
                        child: Text(
                          order['status']!,
                          style: SpatialDesignSystem.captionText.copyWith(
                            color: _getOrderStatusColor(order['status']!),
                            fontWeight: FontWeight.w600,
                          ),
                        ),
                      ),
                    ],
                  ),
                  const Divider(),
                  Row(
                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                    children: [
                      Row(
                        children: [
                          const Icon(Icons.person_outline, size: 16),
                          const SizedBox(width: 4),
                          Text(
                            "Customer:",
                            style: SpatialDesignSystem.captionText.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                            ),
                          ),
                        ],
                      ),
                      Text(
                        order['customer']!,
                        style: SpatialDesignSystem.bodyMedium.copyWith(
                          fontWeight: FontWeight.w500,
                        ),
                      ),
                    ],
                  ),
                  const SizedBox(height: 8),
                  Row(
                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                    children: [
                      Row(
                        children: [
                          const Icon(Icons.attach_money, size: 16),
                          const SizedBox(width: 4),
                          Text(
                            "Amount:",
                            style: SpatialDesignSystem.captionText.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                            ),
                          ),
                        ],
                      ),
                      Text(
                        order['amount']!,
                        style: SpatialDesignSystem.bodyMedium.copyWith(
                          fontWeight: FontWeight.w500,
                        ),
                      ),
                    ],
                  ),
                ],
              ),
            ),
          ),
        );
      },
    );
  }

  Color _getOrderStatusColor(String status) {
    switch (status) {
      case 'Delivered':
        return Colors.green;
      case 'In Transit':
        return SpatialDesignSystem.primaryColor;
      case 'Pending':
        return Colors.orange;
      default:
        return Colors.grey;
    }
  }

  Widget _buildBottomBar() {
    return SafeArea(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Row(
          children: [
            Expanded(
              child: SpatialButton(
                text: "Start Navigation",
                iconData: Icons.navigation,
                onPressed: () {
                  // TODO: Implement navigation logic
                  _navigateToRouteMap();
                },
                isGlass: true,
              ),
            ),
          ],
        ),
      ),
    );
  }

  void _navigateToRouteMap() async {
    try {
      // Show loading indicator
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Row(
            children: [
              SizedBox(
                width: 20,
                height: 20,
                child: CircularProgressIndicator(strokeWidth: 2),
              ),
              SizedBox(width: 16),
              Text('Preparing navigation route...'),
            ],
          ),
          duration: Duration(seconds: 2),
        ),
      );

      // TODO: Navigate to map screen
      // Mock data for now
      await Future.delayed(const Duration(seconds: 2));
      if (mounted) {
        // Giả lập dữ liệu đơn hàng để truyền vào map screen
        // Trong thực tế, dữ liệu này sẽ được lấy từ API
        Navigator.push(
          context,
          MaterialPageRoute(
            builder: (context) => const Scaffold(
              body: Center(
                child: Text("Map view would show here"),
              ),
            ),
          ),
        );
      }
    } catch (e) {
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Error opening navigation: $e'),
            backgroundColor: Colors.red,
          ),
        );
      }
    }
  }
}
