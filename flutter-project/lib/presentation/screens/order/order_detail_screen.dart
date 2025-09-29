import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_details_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_status_update.dart';
import 'package:ktc_logistics_driver/domain/models/delivery_proof/delivery_proof.dart';
import 'package:ktc_logistics_driver/services/orders_services.dart';
import 'package:ktc_logistics_driver/services/delivery_proof_service.dart';

import '../../design/spatial_ui.dart';
import '../../components/spatial_button.dart';
import '../../components/spatial_glass_card.dart';
import '../../components/status_update_modal.dart';
import '../../components/delivery_proof_manager.dart';
import '../../helpers/url_launcher_frave.dart';
import '../../../services/googlemaps_services.dart';
import 'package:intl/intl.dart';

// Tab containing configuration data
class OrderTab {
  final String text;
  final Widget Function() contentBuilder;

  OrderTab({required this.text, required this.contentBuilder});
}

class OrderDetailScreen extends StatefulWidget {
  final String orderId;

  const OrderDetailScreen({
    super.key,
    required this.orderId,
  });

  @override
  State<OrderDetailScreen> createState() => _OrderDetailScreenState();
}

class _OrderDetailScreenState extends State<OrderDetailScreen>
    with SingleTickerProviderStateMixin {
  late TabController _tabController;
  late List<OrderTab> _tabs;

  // For order status
  bool _isUpdatingStatus = false;
  String _selectedStatus = 'PENDING';

  // For API integration
  bool _isLoading = true;
  String _errorMessage = '';
  OrderDetailsResponse? _orderDetails;

  // For delivery proof
  final DeliveryProofService _deliveryProofService = DeliveryProofService();
  List<DeliveryProof> _deliveryProofs = [];

  @override
  void initState() {
    super.initState();

    // Initialize tab list once
    _tabs = [
      OrderTab(text: "Overview", contentBuilder: _buildOverviewTab),
      OrderTab(text: "Details", contentBuilder: _buildItemsTab),
    ];

    _tabController = TabController(length: _tabs.length, vsync: this);
    _tabController.addListener(() {
      setState(() {});
    });

    // Load order details from API
    _loadOrderDetails();

    // Note: Delivery proofs will be loaded as part of order details now
    // No need to call _loadDeliveryProofs() separately
  }

  @override
  void dispose() {
    _tabController.dispose();
    super.dispose();
  }

  // Load delivery proofs for the current order
  Future<void> _loadDeliveryProofs() async {
    if (!mounted) return;

    final orderId = int.tryParse(widget.orderId);
    if (orderId == null) return;

    try {
      final proofs = await _deliveryProofService.getDeliveryProofs(orderId);

      if (!mounted) return;

      setState(() {
        _deliveryProofs = proofs;
      });
    } catch (e) {
      if (!mounted) return;
      print('Error loading delivery proofs: $e');
    }
  }

  // No status mapping function - we directly use the statuses from database

  // Load order details from API
  Future<void> _loadOrderDetails() async {
    if (!mounted) return;

    setState(() {
      _isLoading = true;
      _errorMessage = '';
    });

    try {
      final orderId = int.tryParse(widget.orderId);
      if (orderId == null) {
        if (!mounted) return;
        setState(() {
          _isLoading = false;
          _errorMessage = 'Invalid order ID format';
        });
        return;
      }

      final orderDetails = await ordersServices.getDriverOrderDetail(orderId);

      if (!mounted) return;

      if (orderDetails != null) {
        // Debug print the address information
        if (orderDetails.address != null) {
          debugPrint('=== ADDRESS PARSING DEBUG ===');
          debugPrint('Address from API: ${orderDetails.address!.address}');
          debugPrint('Address ID: ${orderDetails.address!.id}');
          debugPrint('Address Type: ${orderDetails.address!.addressType}');
          debugPrint('Parsed Latitude: ${orderDetails.address!.latitude}');
          debugPrint('Parsed Longitude: ${orderDetails.address!.longitude}');
          debugPrint('Contact Name: ${orderDetails.address!.contactName}');
          debugPrint('Contact Phone: ${orderDetails.address!.contactPhone}');
          debugPrint('=============================');
        } else {
          debugPrint('=== ADDRESS PARSING DEBUG ===');
          debugPrint('No address found in orderDetails');
          debugPrint('=============================');
        }

        // Process delivery data if available
        if (orderDetails.deliveryResponse?.data != null) {
          final deliveryData = orderDetails.deliveryResponse!.data!;
          debugPrint('Delivery data: $deliveryData');

          // Check for orderDate
          if (deliveryData.containsKey('orderDate') &&
              deliveryData['orderDate'] != null) {
            debugPrint('Found orderDate in API: ${deliveryData['orderDate']}');
            final orderDate =
                DateTime.tryParse(deliveryData['orderDate'].toString());
            debugPrint('Parsed orderDate: $orderDate');
          }

          // Debug orderDate if available
          if (deliveryData.containsKey('orderDate') &&
              deliveryData['orderDate'] != null) {
            debugPrint('Found orderDate in API: ${deliveryData['orderDate']}');
            final orderDate =
                DateTime.tryParse(deliveryData['orderDate'].toString());
            debugPrint('Parsed orderDate: $orderDate');
          }

          // Log start and destination locations for debugging purposes only
          if (deliveryData.containsKey('startLocation') &&
              deliveryData['startLocation'] is Map<String, dynamic>) {
            final startLoc =
                deliveryData['startLocation'] as Map<String, dynamic>;
            if (startLoc.containsKey('lat') && startLoc.containsKey('lng')) {
              debugPrint(
                  'Start location from API: ${startLoc['lat']},${startLoc['lng']}');
            }
          }

          if (deliveryData.containsKey('destinationLocation') &&
              deliveryData['destinationLocation'] is Map<String, dynamic>) {
            final destLoc =
                deliveryData['destinationLocation'] as Map<String, dynamic>;
            if (destLoc.containsKey('lat') && destLoc.containsKey('lng')) {
              debugPrint(
                  'Destination location from API: ${destLoc['lat']},${destLoc['lng']}');
            }
          }
        }

        if (!mounted) return;
        setState(() {
          _orderDetails = orderDetails;
          _isLoading = false;

          // Set delivery proofs from order details response
          if (orderDetails.deliveryProofs != null) {
            _deliveryProofs = orderDetails.deliveryProofs!;
            debugPrint(
                '📦 Loaded ${_deliveryProofs.length} delivery proofs from order details');
          } else {
            _deliveryProofs = [];
            debugPrint('📦 No delivery proofs in order details response');
          }

          // Update status from API response
          if (orderDetails.status != null) {
            // Use status directly from API without any mapping
            _selectedStatus = orderDetails.status!.toUpperCase().trim();

            // Debug print
            debugPrint('=== STATUS DEBUG ===');
            debugPrint(
                'Order ID: ${widget.orderId} - Raw status from API: "${orderDetails.status}"');
            debugPrint('After toUpperCase().trim(): "$_selectedStatus"');
            debugPrint('Status length: ${_selectedStatus.length}');
            debugPrint('Status bytes: ${_selectedStatus.codeUnits}');
            debugPrint('==================');
          } else {
            // Default status if none provided
            _selectedStatus = 'PENDING';
            debugPrint('=== STATUS DEBUG ===');
            debugPrint('No status from API, using default: PENDING');
            debugPrint('Order ID: ${orderDetails.id}');
            debugPrint('Order description: ${orderDetails.description}');
            debugPrint('==================');
          }
        });
      } else {
        if (!mounted) return;
        setState(() {
          _isLoading = false;
          _errorMessage = 'Failed to load order details';
        });
      }
    } catch (e) {
      if (!mounted) return;
      setState(() {
        _isLoading = false;
        _errorMessage = 'Error: $e';
      });
    }
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
            icon: const Icon(Icons.refresh),
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
            onPressed: _loadOrderDetails,
          ),
          IconButton(
            icon: const Icon(Icons.phone),
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
            onPressed: () {
              _callCustomer();
            },
          ),
        ],
      ),
      body: _isLoading
          ? _buildLoadingView()
          : _errorMessage.isNotEmpty
              ? _buildErrorView()
              : _buildContentView(),
      bottomNavigationBar:
          _isLoading || _errorMessage.isNotEmpty ? null : _buildBottomBar(),
    );
  }

  // Loading view
  Widget _buildLoadingView() {
    return const Center(
      child: Column(
        mainAxisSize: MainAxisSize.min,
        children: [
          CircularProgressIndicator(),
          SizedBox(height: 16),
          Text('Loading order details...'),
        ],
      ),
    );
  }

  // Error view
  Widget _buildErrorView() {
    return Center(
      child: Column(
        mainAxisSize: MainAxisSize.min,
        children: [
          const Icon(
            Icons.error_outline,
            color: Colors.red,
            size: 48,
          ),
          const SizedBox(height: 16),
          Text(_errorMessage),
          const SizedBox(height: 24),
          SpatialButton(
            text: 'Retry',
            onPressed: _loadOrderDetails,
            iconData: Icons.refresh,
          ),
        ],
      ),
    );
  }

  // Content view
  Widget _buildContentView() {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Column(
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
                width: 3.0,
                color: SpatialDesignSystem.primaryColor,
              ),
              insets: const EdgeInsets.symmetric(horizontal: 16.0),
            ),
            // Improve text styling
            labelStyle: SpatialDesignSystem.bodyMedium.copyWith(
              fontWeight: FontWeight.w600,
            ),
            // Add outer padding to adjust the total height of the TabBar
            padding: const EdgeInsets.symmetric(vertical: 4),
            labelColor: SpatialDesignSystem.primaryColor,
            unselectedLabelColor: isDark
                ? SpatialDesignSystem.textDarkSecondaryColor
                : SpatialDesignSystem.textSecondaryColor,
            isScrollable: false,
            // Sử dụng tab tùy chỉnh với padding bên trong thay vì chỉ dùng text
            tabs: _tabs
                .map((tab) => Tab(
                      child: Container(
                        padding: const EdgeInsets.symmetric(vertical: 8),
                        // Đảm bảo tab có chiều rộng tối thiểu để tránh quá sát với viền
                        width: MediaQuery.of(context).size.width / 3.5,
                        alignment: Alignment.center,
                        child: Text(
                          tab.text,
                          style: const TextStyle(
                            fontWeight: FontWeight.w600,
                          ),
                        ),
                      ),
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
    );
  }

  // The shared buildStatusCard method
  Widget _buildStatusCard() {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    // Get estimated delivery time from order details or use placeholder
    String estimatedDelivery = "Not specified";
    if (_orderDetails?.deliveryResponse?.data != null) {
      final deliveryData = _orderDetails!.deliveryResponse!.data!;

      // Try to get scheduled delivery time from API response
      if (deliveryData.containsKey('scheduleDeliveryTime') &&
          deliveryData['scheduleDeliveryTime'] != null) {
        final deliveryTime =
            DateTime.tryParse(deliveryData['scheduleDeliveryTime'].toString());
        if (deliveryTime != null) {
          estimatedDelivery =
              DateFormat('MMM dd, yyyy | hh:mm a').format(deliveryTime);
        }
      } else if (deliveryData.containsKey('scheduledTime') &&
          deliveryData['scheduledTime'] != null) {
        final scheduledTime =
            DateTime.tryParse(deliveryData['scheduledTime'].toString());
        if (scheduledTime != null) {
          estimatedDelivery =
              DateFormat('MMM dd, yyyy | hh:mm a').format(scheduledTime);
        }
      } else if (deliveryData.containsKey('orderDate') &&
          deliveryData['orderDate'] != null) {
        // If no delivery time, show order date + estimated delivery time (e.g., 24h)
        final orderDate =
            DateTime.tryParse(deliveryData['orderDate'].toString());
        debugPrint('Found orderDate in API: ${deliveryData['orderDate']}');
        debugPrint('Parsed DateTime: $orderDate');
        if (orderDate != null) {
          final estimatedDeliveryDate =
              orderDate.add(const Duration(hours: 24));
          estimatedDelivery = DateFormat('MMM dd, yyyy | hh:mm a')
              .format(estimatedDeliveryDate);
          debugPrint('Calculated estimatedDelivery: $estimatedDelivery');
        }
      }
    }

    // Calculate progress based on status
    double progressValue = _getStatusProgress(_selectedStatus);

    // Debug status in build method
    debugPrint('=== UI BUILD DEBUG ===');
    debugPrint('Current _selectedStatus in UI: "$_selectedStatus"');
    debugPrint(
        'Status display text: "${_selectedStatus.replaceAll('_', ' ')}"');
    debugPrint('=====================');

    return GlassCard(
      padding: const EdgeInsets.all(20),
      gradient: LinearGradient(
        colors: [
          SpatialDesignSystem.primaryColor.withOpacity(0.1),
          SpatialDesignSystem.accentColor.withOpacity(0.05),
        ],
        begin: Alignment.topLeft,
        end: Alignment.bottomRight,
      ),
      child: Column(
        children: [
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Text(
                "Order #${widget.orderId}",
                style: SpatialDesignSystem.subtitleLarge.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
              ),
              Container(
                padding:
                    const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
                decoration: BoxDecoration(
                  color: _getStatusColor(_selectedStatus).withOpacity(0.1),
                  borderRadius: BorderRadius.circular(12),
                  border: Border.all(
                    color: _getStatusColor(_selectedStatus).withOpacity(0.3),
                    width: 1,
                  ),
                ),
                child: Text(
                  _selectedStatus.replaceAll('_', ' '),
                  style: SpatialDesignSystem.captionText.copyWith(
                    color: _getStatusColor(_selectedStatus),
                    fontWeight: FontWeight.w600,
                  ),
                ),
              ),
            ],
          ),
          const SizedBox(height: 8),
          Text(
            "Estimated delivery: $estimatedDelivery",
            style: SpatialDesignSystem.bodyMedium.copyWith(
              color: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor
                  : SpatialDesignSystem.textSecondaryColor,
            ),
          ),
          const SizedBox(height: 20),
          LinearProgressIndicator(
            value: progressValue,
            backgroundColor: isDark
                ? Colors.white.withOpacity(0.1)
                : Colors.black.withOpacity(0.05),
            valueColor: AlwaysStoppedAnimation<Color>(Colors.white),
            borderRadius: BorderRadius.circular(10),
            minHeight: 8,
          ),
          const SizedBox(height: 10),
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Text(
                _getStatusDescription(_selectedStatus),
                style: SpatialDesignSystem.captionText.copyWith(
                  color: SpatialDesignSystem.primaryColor,
                  fontWeight: FontWeight.w600,
                ),
              ),
              Text(
                "${(progressValue * 100).toInt()}% complete",
                style: SpatialDesignSystem.captionText.copyWith(
                  color: progressValue >= 0.9
                      ? Colors.green
                      : (isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor),
                  fontWeight: progressValue >= 0.9
                      ? FontWeight.w600
                      : FontWeight.normal,
                ),
              ),
            ],
          ),

          // Add Update Status button below the progress bar
          const SizedBox(height: 16),
          SpatialButton(
            text: _isUpdatingStatus ? "Updating..." : "Update Status",
            onPressed: _isUpdatingStatus ? () {} : _showOrderStatusUpdateDialog,
            iconData: _isUpdatingStatus
                ? Icons.hourglass_empty
                : Icons.track_changes_outlined,
            isGlass: true,
            textColor: Colors.white,
            padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
            width: double.infinity,
          ),
        ],
      ),
    );
  }

  // Get progress value based on status
  double _getStatusProgress(String status) {
    switch (status.toUpperCase()) {
      case 'PENDING':
        return 0.1;
      case 'PROCESSING':
        return 0.4;
      case 'SHIPPING':
        return 0.6;
      case 'DELIVERED':
        return 0.8;
      case 'COMPLETED':
        return 1.0;
      case 'CANCELLED':
        return 0.0;
      default:
        debugPrint('Unknown status for progress: $status');
        return 0.1;
    }
  }

  // Get status description
  String _getStatusDescription(String status) {
    switch (status.toUpperCase()) {
      case 'PENDING':
        return "Awaiting processing";
      case 'PROCESSING':
        return "Being prepared";
      case 'SHIPPING':
        return "In transit";
      case 'DELIVERED':
        return "Delivered";
      case 'COMPLETED':
        return "Completed";
      case 'CANCELLED':
        return "Cancelled";
      default:
        debugPrint('Unknown status for description: $status');
        return "Status: $status";
    }
  }

  Widget _buildOverviewTab() {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    // Get customer name from order details or use placeholder
    String customerName = "Not specified";
    String customerPhone = "Not specified";
    String customerEmail = "Not specified";

    // Get address information
    String deliveryAddress = "Not specified";
    String deliveryTime = "Not specified";
    String deliveryInstructions = "Not specified";

    // Get order information
    String orderDate = "Not specified";
    String packageType = "Standard Package";
    String weight = "Not specified";
    String paymentMethod = "Not specified";

    if (_orderDetails != null) {
      // Customer information
      if (_orderDetails!.address?.contactName != null) {
        customerName = _orderDetails!.address!.contactName!;
      }

      if (_orderDetails!.address?.contactPhone != null) {
        customerPhone = _orderDetails!.address!.contactPhone!;
      }

      if (_orderDetails!.address?.contactEmail != null) {
        customerEmail = _orderDetails!.address!.contactEmail!;
      }

      // Delivery information
      if (_orderDetails!.address != null) {
        // Use the address field directly from the API as shown in the example
        if (_orderDetails!.address!.address.isNotEmpty) {
          deliveryAddress = _orderDetails!.address!.address;
          debugPrint('Using address from API: $deliveryAddress');
        } else {
          deliveryAddress = "Address not available";
        }
      }

      // Delivery time from API
      if (_orderDetails!.deliveryResponse?.data != null) {
        final deliveryData = _orderDetails!.deliveryResponse!.data!;

        // Try different delivery time fields in order of preference
        if (deliveryData.containsKey('scheduleDeliveryTime') &&
            deliveryData['scheduleDeliveryTime'] != null) {
          final deliveryTimeDate = DateTime.tryParse(
              deliveryData['scheduleDeliveryTime'].toString());
          if (deliveryTimeDate != null) {
            deliveryTime =
                DateFormat('MMM dd, yyyy | hh:mm a').format(deliveryTimeDate);
          }
        } else if (deliveryData.containsKey('scheduledTime') &&
            deliveryData['scheduledTime'] != null) {
          final scheduledTime =
              DateTime.tryParse(deliveryData['scheduledTime'].toString());
          if (scheduledTime != null) {
            deliveryTime =
                DateFormat('MMM dd, yyyy | hh:mm a').format(scheduledTime);
          }
        } else if (deliveryData.containsKey('orderDate') &&
            deliveryData['orderDate'] != null) {
          // If no delivery time is specified, estimate 24 hours after order date
          final orderDate =
              DateTime.tryParse(deliveryData['orderDate'].toString());
          if (orderDate != null) {
            final estimatedDelivery = orderDate.add(const Duration(hours: 24));
            deliveryTime =
                "${DateFormat('MMM dd, yyyy | hh:mm a').format(estimatedDelivery)} (Estimated)";
            debugPrint(
                'Using orderDate to calculate delivery time: $deliveryTime');
          }
        }
      }

      // Special instructions from notes
      if (_orderDetails!.notes != null && _orderDetails!.notes!.isNotEmpty) {
        deliveryInstructions = _orderDetails!.notes!;
      }

      // Order date - get from API
      if (_orderDetails!.deliveryResponse?.data != null) {
        final deliveryData = _orderDetails!.deliveryResponse!.data!;

        // Check for orderDate in the delivery data
        if (deliveryData.containsKey('orderDate') &&
            deliveryData['orderDate'] != null) {
          final orderDateTime =
              DateTime.tryParse(deliveryData['orderDate'].toString());
          if (orderDateTime != null) {
            orderDate =
                DateFormat('MMMM dd, yyyy | hh:mm a').format(orderDateTime);
            debugPrint('Found orderDate in API: $orderDate');
          }
        } else {
          // Fallback: calculate order date from estimated delivery time if available
          if (deliveryData.containsKey('estimatedDeliveryTime')) {
            final deliveryTimeDate = DateTime.tryParse(
                deliveryData['estimatedDeliveryTime'].toString());
            if (deliveryTimeDate != null) {
              final calculatedOrderDate =
                  deliveryTimeDate.subtract(const Duration(days: 3));
              orderDate = DateFormat('MMMM dd, yyyy | hh:mm a')
                  .format(calculatedOrderDate);
              debugPrint(
                  'Calculated orderDate from estimatedDeliveryTime: $orderDate');
            }
          }
        }
      }

      // Weight information if available in order items
      double totalWeight = 0;
      bool hasWeight = false;

      if (_orderDetails!.orderItems != null &&
          _orderDetails!.orderItems!.isNotEmpty) {
        for (var item in _orderDetails!.orderItems!) {
          if (item.weight != null) {
            totalWeight += item.weight!;
            hasWeight = true;
          }
        }

        if (hasWeight) {
          weight = "$totalWeight kg";
        }
      }

      // Payment information from store or mock
      if (_orderDetails!.store != null) {
        paymentMethod = "Credit Card (Paid)"; // Mock payment method
      }
    }

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
                  Icons.person,
                  "Name",
                  customerName,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.phone,
                  "Phone",
                  customerPhone,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.email,
                  "Email",
                  customerEmail,
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
                  Icons.location_on,
                  "Delivery Address",
                  deliveryAddress,
                  allowMultiLine: true,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.access_time,
                  "Delivery Time",
                  deliveryTime,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.note,
                  "Special Instructions",
                  deliveryInstructions,
                ),
              ],
            ),
          ),

          const SizedBox(height: 16),

          // Order Information
          GlassCard(
            padding: const EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  "Order Information",
                  style: SpatialDesignSystem.subtitleMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),
                const SizedBox(height: 16),
                _buildInfoRow(
                  Icons.calendar_today,
                  "Order Date",
                  orderDate,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.category,
                  "Package Type",
                  packageType,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.scale,
                  "Weight",
                  weight,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.payment,
                  "Payment Method",
                  paymentMethod,
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildInfoRow(IconData icon, String label, String value,
      {bool allowMultiLine = false}) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 8),
      child: Row(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Container(
            padding: const EdgeInsets.all(8),
            decoration: BoxDecoration(
              color: SpatialDesignSystem.primaryColor.withOpacity(0.1),
              borderRadius: BorderRadius.circular(8),
            ),
            child: Icon(
              icon,
              size: 18,
              color: SpatialDesignSystem.primaryColor,
            ),
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
                  maxLines: allowMultiLine ? null : 2,
                  overflow: allowMultiLine
                      ? TextOverflow.visible
                      : TextOverflow.ellipsis,
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildItemsTab() {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    // Order items
    List<Widget> orderItemWidgets = [];
    double subtotal = 0;
    double deliveryFee = 0;
    double discount = 0;
    double total = 0;

    if (_orderDetails != null && _orderDetails!.orderItems != null) {
      // Add order items
      for (var item in _orderDetails!.orderItems!) {
        double itemPrice = item.unitPrice ?? 0;
        double itemSubtotal = item.subtotal ?? (itemPrice * item.quantity);
        subtotal += itemSubtotal;

        orderItemWidgets.add(
          _buildOrderItem(
            item.productName,
            "${item.quantity}x",
            NumberFormat.currency(
                    locale: 'vi_VN', symbol: '₫', decimalDigits: 0)
                .format(itemSubtotal),
            item.productImage ?? "https://via.placeholder.com/60",
            shippingFee: item.shippingFee != null
                ? NumberFormat.currency(
                        locale: 'vi_VN', symbol: '₫', decimalDigits: 0)
                    .format(item.shippingFee!)
                : null,
          ),
        );

        if (item != _orderDetails!.orderItems!.last) {
          orderItemWidgets.add(const Divider());
        }
      }

      // Calculate order totals
      deliveryFee = 0; // Initialize to zero

      // Calculate total shipping fee from all order items
      for (var item in _orderDetails!.orderItems!) {
        if (item.shippingFee != null) {
          deliveryFee += item.shippingFee!;
        }
      }

      // If no shipping fee found, use default
      if (deliveryFee == 0) {
        deliveryFee = 20000; // Default fee as fallback
      }

      // Mock discount
      discount = 10000;

      // Calculate total
      total = subtotal + deliveryFee - discount;
    } else {
      // Show empty state instead of placeholder items
      orderItemWidgets = [
        Container(
          width: double.infinity, // Force full width
          child: Column(
            children: [
              const SizedBox(height: 20),
              Icon(
                Icons.inventory_2_outlined,
                size: 48,
                color: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                        .withOpacity(0.5)
                    : SpatialDesignSystem.textSecondaryColor.withOpacity(0.5),
              ),
              const SizedBox(height: 16),
              Text(
                "No items found",
                style: SpatialDesignSystem.bodyLarge.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                  fontWeight: FontWeight.w500,
                ),
              ),
              const SizedBox(height: 8),
              Text(
                "Order items will be displayed here when available",
                style: SpatialDesignSystem.captionText.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                          .withOpacity(0.7)
                      : SpatialDesignSystem.textSecondaryColor.withOpacity(0.7),
                ),
                textAlign: TextAlign.center,
              ),
              const SizedBox(height: 20),
            ],
          ),
        ),
      ];

      // Set realistic totals when no order items
      subtotal = 0;
      deliveryFee = 0;
      discount = 0;
      total = 0;
    }

    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          // Order Items
          SizedBox(
            width: double.infinity,
            child: GlassCard(
              padding: const EdgeInsets.all(16),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    "Order Items",
                    style: SpatialDesignSystem.subtitleMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                  ),
                  const SizedBox(height: 16),
                  ...orderItemWidgets,
                ],
              ),
            ),
          ),

          const SizedBox(height: 16),

          // Order Summary
          SizedBox(
            width: double.infinity,
            child: GlassCard(
              padding: const EdgeInsets.all(16),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    "Order Summary",
                    style: SpatialDesignSystem.subtitleMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                  ),
                  const SizedBox(height: 16),
                  // Show empty state if all totals are 0, otherwise show normal summary
                  if (subtotal == 0 &&
                      deliveryFee == 0 &&
                      discount == 0 &&
                      total == 0) ...[
                    Container(
                      width: double.infinity, // Force full width
                      child: Column(
                        children: [
                          const SizedBox(height: 20),
                          Icon(
                            Icons.receipt_outlined,
                            size: 36,
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                    .withOpacity(0.5)
                                : SpatialDesignSystem.textSecondaryColor
                                    .withOpacity(0.5),
                          ),
                          const SizedBox(height: 12),
                          Text(
                            "No pricing data available",
                            style: SpatialDesignSystem.bodyMedium.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                            ),
                          ),
                          const SizedBox(height: 20),
                        ],
                      ),
                    ),
                  ] else ...[
                    _buildSummaryRow(
                        "Subtotal",
                        NumberFormat.currency(
                                locale: 'vi_VN', symbol: '₫', decimalDigits: 0)
                            .format(subtotal)),
                    const Divider(),
                    _buildSummaryRow(
                        "Delivery Fee",
                        NumberFormat.currency(
                                locale: 'vi_VN', symbol: '₫', decimalDigits: 0)
                            .format(deliveryFee)),
                    const Divider(),
                    _buildSummaryRow("Discount",
                        "-${NumberFormat.currency(locale: 'vi_VN', symbol: '₫', decimalDigits: 0).format(discount)}"),
                    const Divider(thickness: 1.5),
                    _buildSummaryRow(
                        "Total",
                        NumberFormat.currency(
                                locale: 'vi_VN', symbol: '₫', decimalDigits: 0)
                            .format(total),
                        isTotal: true),
                  ],
                ],
              ),
            ),
          ),
          // Delivery Proof Section (show for delivered and completed orders)
          if (_selectedStatus == 'DELIVERED' || _selectedStatus == 'COMPLETED')
            DeliveryProofManager(
              orderId: int.parse(widget.orderId),
              orderStatus: _selectedStatus,
              canUpload: _selectedStatus == 'DELIVERED',
              initialProofs: _deliveryProofs,
              onProofUpdated: () {
                // Refresh delivery proofs
                _loadDeliveryProofs();
              },
            ),
        ],
      ),
    );
  }

  Widget _buildOrderItem(
      String name, String quantity, String price, String imageUrl,
      {String? shippingFee}) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 8),
      child: Row(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          // Column 1: Icon and quantity
          Column(
            children: [
              Container(
                width: 50,
                height: 50,
                decoration: BoxDecoration(
                  color: isDark
                      ? Colors.black.withOpacity(0.3)
                      : Colors.grey.withOpacity(0.2),
                  borderRadius: BorderRadius.circular(8),
                ),
                child: Center(
                  child: Icon(
                    Icons.inventory_2_outlined,
                    color: SpatialDesignSystem.primaryColor,
                    size: 25,
                  ),
                ),
              ),
              const SizedBox(height: 4),
              Text(
                quantity,
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: SpatialDesignSystem.primaryColor,
                  fontWeight: FontWeight.w600,
                  fontSize: 12,
                ),
              ),
            ],
          ),

          const SizedBox(width: 16),

          // Column 2: Product details
          Expanded(
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                // Row 1: Product name
                Text(
                  name,
                  style: SpatialDesignSystem.subtitleMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
                ),

                const SizedBox(height: 8),

                // Row 2: Price information
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Row(
                      children: [
                        Icon(
                          Icons.shopping_bag_outlined,
                          size: 14,
                          color: isDark
                              ? SpatialDesignSystem.textDarkSecondaryColor
                              : SpatialDesignSystem.textSecondaryColor,
                        ),
                        const SizedBox(width: 4),
                        Text(
                          "Subtotal: ",
                          style: SpatialDesignSystem.captionText.copyWith(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                          ),
                        ),
                        Text(
                          price,
                          style: SpatialDesignSystem.bodySmall.copyWith(
                            color: isDark
                                ? SpatialDesignSystem.textDarkPrimaryColor
                                : SpatialDesignSystem.textPrimaryColor,
                            fontWeight: FontWeight.w600,
                          ),
                        ),
                      ],
                    ),
                    if (shippingFee != null)
                      Row(
                        children: [
                          Icon(
                            Icons.local_shipping_outlined,
                            size: 14,
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                          ),
                          const SizedBox(width: 4),
                          Text(
                            "Shipping: ",
                            style: SpatialDesignSystem.captionText.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                            ),
                          ),
                          Text(
                            shippingFee,
                            style: SpatialDesignSystem.bodySmall.copyWith(
                              color: SpatialDesignSystem.primaryColor,
                              fontWeight: FontWeight.w600,
                            ),
                          ),
                        ],
                      ),
                  ],
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildSummaryRow(String label, String value, {bool isTotal = false}) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 8),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        children: [
          Text(
            label,
            style: isTotal
                ? SpatialDesignSystem.subtitleSmall.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                    fontWeight: FontWeight.w600,
                  )
                : SpatialDesignSystem.bodyMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                  ),
          ),
          Text(
            value,
            style: isTotal
                ? SpatialDesignSystem.subtitleSmall.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                    fontWeight: FontWeight.w600,
                  )
                : SpatialDesignSystem.bodyMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                  ),
          ),
        ],
      ),
    );
  }

  Widget _buildBottomBar() {
    // Use Container instead of SafeArea+Padding for more consistent layout
    return Container(
      padding: const EdgeInsets.all(16),
      decoration: BoxDecoration(
        color: Theme.of(context).colorScheme.surface,
        boxShadow: [
          BoxShadow(
            color: Colors.black.withOpacity(0.1),
            blurRadius: 10,
            offset: const Offset(0, -2),
          ),
        ],
      ),
      child: SafeArea(
        child: Row(
          children: [
            // Navigation Button - Full width
            Expanded(
              child: ElevatedButton.icon(
                onPressed: () => _navigateToRouteMap(),
                icon:
                    const Icon(Icons.navigation, color: Colors.white, size: 24),
                label: const Text(
                  'Navigate',
                  style: TextStyle(
                      color: Colors.white,
                      fontSize: 18,
                      fontWeight: FontWeight.w500),
                ),
                style: ElevatedButton.styleFrom(
                  backgroundColor: SpatialDesignSystem.primaryColor,
                  padding: const EdgeInsets.symmetric(vertical: 10),
                  elevation: 3,
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  // Navigate to map screen
  void _navigateToRouteMap() async {
    try {
      // Check if widget is still mounted before accessing context
      if (!mounted) return;

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

      final mapsService = GoogleMapsService();

      // Get current position for navigation (ensures we start from actual current location)
      final currentPositionData = await mapsService.getCurrentPositionData();

      // Check if widget is still mounted after async operation
      if (!mounted) return;

      // Default destination coordinates if not available from API
      Map<String, dynamic> deliveryLocation = {
        'latitude': 10.7756,
        'longitude': 106.7019,
        'address': 'Destination Address'
      };

      // Get real coordinates from orderDetail.address if available
      if (_orderDetails != null && _orderDetails!.address != null) {
        // Debug the actual address object first
        debugPrint('=== ADDRESS DEBUG ===');
        debugPrint('Address object: ${_orderDetails!.address}');
        debugPrint('Address ID: ${_orderDetails!.address!.id}');
        debugPrint('Address string: ${_orderDetails!.address!.address}');
        debugPrint('Raw latitude: ${_orderDetails!.address!.latitude}');
        debugPrint('Raw longitude: ${_orderDetails!.address!.longitude}');
        debugPrint('Latitude type: ${_orderDetails!.address!.latitude.runtimeType}');
        debugPrint('Longitude type: ${_orderDetails!.address!.longitude.runtimeType}');
        debugPrint('=====================');
        
        // Check if we have latitude and longitude from orderDetail.address
        if (_orderDetails!.address!.latitude != null &&
            _orderDetails!.address!.longitude != null) {
          // Use coordinates directly (already converted to double in Address model)
          double latitude = _orderDetails!.address!.latitude!;
          double longitude = _orderDetails!.address!.longitude!;
          
          deliveryLocation = {
            'latitude': latitude,
            'longitude': longitude,
            'address': _orderDetails!.address!.address
          };
          
          // Debug coordinates information
          debugPrint('=== NAVIGATION DEBUG ===');
          debugPrint('Using orderDetail.address coordinates:');
          debugPrint('Latitude: $latitude');
          debugPrint('Longitude: $longitude');
          debugPrint('Address: ${_orderDetails!.address!.address}');
          debugPrint('========================');
        } else {
          // Fallback: use address string if no coordinates
          deliveryLocation = {
            'latitude': 10.7756, // Default coordinates
            'longitude': 106.7019,
            'address': _orderDetails!.address!.address
          };
          
          debugPrint('=== NAVIGATION DEBUG ===');
          debugPrint('No coordinates found in orderDetail.address');
          debugPrint('Latitude value: ${_orderDetails!.address!.latitude}');
          debugPrint('Longitude value: ${_orderDetails!.address!.longitude}');
          debugPrint('Using default coordinates and address: ${_orderDetails!.address!.address}');
          debugPrint('========================');
        }
      } else {
        debugPrint('=== NAVIGATION DEBUG ===');
        debugPrint('_orderDetails: $_orderDetails');
        debugPrint('_orderDetails?.address: ${_orderDetails?.address}');
        debugPrint('No orderDetail.address found, using default coordinates');
        debugPrint('========================');
      }

      // Check if widget is still mounted before accessing context
      if (!mounted) return;

      // Open Google Maps with direct route from current location to destination
      final result = await mapsService.openGoogleMapsWithRoute(
        context: context,
        pickupLocation:
            currentPositionData, // Use actual current position from device
        transitPoints: [], // No transit points
        deliveryLocation: deliveryLocation,
      );

      if (!result && mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text(
                'Failed to open Google Maps. Please make sure it is installed.'),
            backgroundColor: Colors.red,
          ),
        );
      }
    } catch (e) {
      if (mounted) {
        debugPrint("Error opening navigation: $e");
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Error opening navigation: $e'),
            backgroundColor: Colors.red,
          ),
        );
      }
    }
  }

  void _callCustomer() {
    // Check if widget is still mounted before proceeding
    if (!mounted) return;

    // Extract the phone number from the order details
    String phoneNumber = "+84 123 456 789"; // Default number

    if (_orderDetails != null &&
        _orderDetails!.address != null &&
        _orderDetails!.address!.contactPhone != null) {
      phoneNumber = _orderDetails!.address!.contactPhone!;
    }

    // Remove spaces from the phone number
    phoneNumber = phoneNumber.replaceAll(' ', '');

    // Launch the phone dialer with the number
    urlLauncherFrave.makePhoneCall('tel:$phoneNumber');
  }

  // Show dialog to update order status
  void _showOrderStatusUpdateDialog() {
    // Check if widget is still mounted before proceeding
    if (!mounted) return;

    // List of status options with their IDs and names for ORDER type
    // Thứ tự hiển thị: Pending -> Processing -> Shipping -> Delivered -> Completed -> Cancelled
    // ID theo database và OrderStatusId constants
    final List<Map<String, dynamic>> statusOptions = [
      {"id": OrderStatusId.PENDING, "name": "PENDING", "display": "Pending"},
      {
        "id": OrderStatusId.PROCESSING,
        "name": "PROCESSING",
        "display": "Processing"
      },
      {"id": OrderStatusId.SHIPPING, "name": "SHIPPING", "display": "Shipping"},
      {
        "id": OrderStatusId.DELIVERED,
        "name": "DELIVERED",
        "display": "Delivered"
      },
      {
        "id": OrderStatusId.COMPLETED,
        "name": "COMPLETED",
        "display": "Completed"
      },
      {
        "id": OrderStatusId.CANCELLED,
        "name": "CANCELLED",
        "display": "Cancelled"
      }
    ];

    showStatusUpdateModal(
      context: context,
      title: "Update Order Status",
      itemId: "Order #${widget.orderId}",
      currentStatus: _selectedStatus,
      statusOptions: statusOptions,
      getStatusColor: _getStatusColor,
      onUpdateStatus: (String status, String notes) {
        // Create status update object
        final OrderStatusUpdate statusUpdate = OrderStatusUpdate(
          statusId: OrderStatusId.fromStatusName(status),
          notes: notes.isNotEmpty ? notes : null,
        );

        // Call update function
        _updateOrderStatus(statusUpdate);
      },
    );
  }

  // Update order status via API
  Future<void> _updateOrderStatus(OrderStatusUpdate statusUpdate) async {
    try {
      // Show loading indicator
      if (!mounted) return;
      setState(() {
        _isUpdatingStatus = true;
      });

      // Call API service using OrdersServices instead of DeliveryServices
      final success = await ordersServices.updateOrderStatus(
        driverId: null, // Will use the one from secure storage
        orderId: int.parse(widget.orderId),
        statusUpdate: statusUpdate,
      );

      // Check if widget is still mounted before updating state
      if (!mounted) return;

      // Hide loading indicator
      setState(() {
        _isUpdatingStatus = false;
      });

      // Show success or error message
      if (success) {
        // Check if widget is still mounted before accessing context
        if (!mounted) return;

        // Show success message
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text("Order status updated successfully"),
            backgroundColor: Colors.green,
          ),
        );

        // Reload order details from API to get the latest data
        _loadOrderDetails();

        // If status was changed to DELIVERED or COMPLETED, refresh delivery proofs
        if (statusUpdate.statusId == OrderStatusId.DELIVERED ||
            statusUpdate.statusId == OrderStatusId.COMPLETED) {
          _loadDeliveryProofs();
        }
      } else {
        // Check if widget is still mounted before accessing context
        if (!mounted) return;

        // Try to auto-refresh and update again in 1 second
        Future.delayed(Duration(seconds: 1), () {
          if (mounted) {
            // Show message about token refresh attempt
            ScaffoldMessenger.of(context).showSnackBar(
              const SnackBar(
                content: Text("Retrying update with fresh authentication..."),
                backgroundColor: Colors.orange,
              ),
            );

            // Try again automatically
            _updateOrderStatus(statusUpdate);
            return;
          }
        });

        // Show error message
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text("Failed to update order status"),
            backgroundColor: Colors.red,
          ),
        );
      }
    } catch (e) {
      // Check if widget is still mounted before updating state
      if (!mounted) return;

      // Hide loading indicator
      setState(() {
        _isUpdatingStatus = false;
      });

      String errorMessage = "Error occurred during update";
      // Check for specific error types
      if (e.toString().contains('401') ||
          e.toString().contains('Unauthorized')) {
        errorMessage =
            "Session expired. Try updating again or log out and log in again.";

        // Try to automatically retry the update after 1 second
        Future.delayed(Duration(seconds: 1), () {
          if (mounted) {
            // Thử cập nhật lại
            _updateOrderStatus(statusUpdate);
          }
        });
      } else {
        errorMessage = "Error: ${e.toString()}";
      }

      // Show error message
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text(errorMessage),
          backgroundColor: Colors.red,
        ),
      );
    }
  }

  // Get color for status
  Color _getStatusColor(String status) {
    switch (status.toUpperCase()) {
      case 'PENDING':
        return Colors.blue.shade700; // Blue for awaiting processing
      case 'PROCESSING':
        return Colors.amber.shade600; // Orange/Yellow for processing
      case 'SHIPPING':
        return Colors.deepPurple; // Purple for shipping/in transit
      case 'DELIVERED':
        return Colors.orange; // Orange for delivered but not paid
      case 'COMPLETED':
        return Colors.green.shade700; // Deep green for completed
      case 'CANCELLED':
        return Colors.red.shade700; // Red for cancelled
      default:
        debugPrint('Unknown status for color: $status');
        return SpatialDesignSystem.primaryColor; // Default to primary color
    }
  }
}
