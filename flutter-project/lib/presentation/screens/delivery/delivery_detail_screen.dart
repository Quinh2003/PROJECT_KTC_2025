import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/delivery_detail_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_status_update.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_button.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/components/status_update_modal.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/screens/order/order_detail_screen.dart';
import 'package:ktc_logistics_driver/services/delivery_services.dart';
import 'package:ktc_logistics_driver/services/orders_services.dart';
import 'package:ktc_logistics_driver/services/googlemaps_services.dart';
import 'package:ktc_logistics_driver/services/tracking_services.dart';
import 'package:ktc_logistics_driver/presentation/blocs/tracking/simple_tracking_bloc.dart';
import 'package:timeline_tile/timeline_tile.dart';
import 'dart:ui';
import 'dart:async';
import 'package:intl/intl.dart';

// Tab containing configuration data
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
  String _selectedStatus = 'PENDING'; // Default status sử dụng ORDER status
  final _noteController = TextEditingController();
  bool _isLoading = true;
  String _errorMessage = '';
  bool _isTrackingLoading =
      false; // Thêm biến theo dõi trạng thái loading khi tracking
  StreamSubscription?
      _trackingSubscription; // Theo dõi sự thay đổi trạng thái tracking

  DeliveryDetailResponse? _deliveryDetail;

  // List of possible statuses for a delivery - sử dụng 6 status từ database ORDER
  final List<String> _statusOptions = [
    'PENDING',
    'PROCESSING', 
    'SHIPPING',
    'DELIVERED',
    'COMPLETED',
    'CANCELLED',
  ];

  @override
  void initState() {
    super.initState();

    // Initialize tab list once
    _tabs = [
      DeliveryTab(text: "Overview", contentBuilder: _buildOverviewTab),
      DeliveryTab(text: "Orders", contentBuilder: _buildOrdersTab),
    ];

    _tabController = TabController(length: _tabs.length, vsync: this);
    _tabController.addListener(() {
      setState(() {});
    });

    // Ensure selected status is valid
    if (!_statusOptions.contains(_selectedStatus)) {
      _selectedStatus = _statusOptions.first;
    }

    // Listen to tracking state changes
    _trackingSubscription =
        BlocProvider.of<SimpleTrackingBloc>(context).stream.listen((state) {
      print('SimpleTrackingBloc state changed: $state');
      // Only update loading state if we're currently in loading state
      if (_isTrackingLoading) {
        setState(() {
          _isTrackingLoading =
              false; // Reset loading state when tracking state changes
        });
      }
    });

    // Get detailed data of the delivery from API
    _loadDeliveryDetails();
  }

  Future<void> _loadDeliveryDetails() async {
    setState(() {
      _isLoading = true;
      _errorMessage = '';
    });

    // Declare variable outside try block to access from both try and catch
    int? deliveryId;
    String deliveryIdStr = widget.deliveryId;

    try {
      // Check if ID is a large number (like 85797) then use directly
      if (int.tryParse(deliveryIdStr) != null &&
          int.parse(deliveryIdStr) > 10000) {
        deliveryId = int.parse(deliveryIdStr);
        debugPrint('Using numeric ID directly: $deliveryId');
      }
      // If not a large number, try to handle "DEL-26009" format
      else if (deliveryIdStr.contains('-')) {
        // Extract the number after the dash
        final parts = deliveryIdStr.split('-');
        if (parts.length > 1 && int.tryParse(parts[1]) != null) {
          deliveryId = int.parse(parts[1]); // Get "26009" from "DEL-26009"
          debugPrint('Extracted numeric ID from format: $deliveryId');
        } else {
          throw FormatException('Cannot parse delivery code: $deliveryIdStr');
        }
      } else {
        // If no dash, try to parse directly
        deliveryId = int.parse(deliveryIdStr);
        debugPrint('Parsed ID as number: $deliveryId');
      }

      debugPrint('Calling delivery service with ID: $deliveryId');
      final detail = await deliveryServices.getDeliveryDetail(deliveryId);

      if (detail != null) {
        setState(() {
          _deliveryDetail = detail;
          _isLoading = false;

          // Update status from API data
          if (detail.status != null) {
            _selectedStatus = _mapApiStatusToUiStatus(detail.status!);
            // Ensure the selected status is in the options list
            if (!_statusOptions.contains(_selectedStatus)) {
              _selectedStatus = _statusOptions.first;
            }
          }
        });
      } else {
        setState(() {
          _isLoading = false;
          _errorMessage =
              'Unable to load order details for delivery $deliveryId';
        });
      }
    } catch (e) {
      setState(() {
        _isLoading = false;
        _errorMessage = 'Error: $e';
      });
    }
  }

  // Convert status from API to ORDER status names
  String _mapApiStatusToUiStatus(String apiStatus) {
    String mappedStatus;

    switch (apiStatus.toUpperCase()) {
      case 'ASSIGNED':
      case 'CREATED':
        mappedStatus = 'PENDING';
        break;
      case 'STARTED':
      case 'PROCESSING':
      case 'IN_PROGRESS':
        mappedStatus = 'PROCESSING';
        break;
      case 'SHIPPING':
      case 'IN_TRANSIT':
        mappedStatus = 'SHIPPING';
        break;
      case 'DELIVERED':
        mappedStatus = 'DELIVERED';
        break;
      case 'COMPLETED':
        mappedStatus = 'COMPLETED';
        break;
      case 'CANCELLED':
        mappedStatus = 'CANCELLED';
        break;
      default:
        mappedStatus = 'PENDING'; // Default to PENDING
        break;
    }

    // Ensure the mapped status is in the valid options list
    if (!_statusOptions.contains(mappedStatus)) {
      mappedStatus = _statusOptions.first; // Default to first status if invalid
    }

    return mappedStatus;
  }

  @override
  void dispose() {
    _tabController.dispose();
    _noteController.dispose();
    _trackingSubscription?.cancel();
    // Don't stop tracking service - it should continue in the background
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    // Display loading if data is being loaded
    if (_isLoading) {
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
        body: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const CircularProgressIndicator(),
              const SizedBox(height: 16),
              Text(
                "Loading data...",
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              )
            ],
          ),
        ),
      );
    }

    // Display error if any
    if (_errorMessage.isNotEmpty) {
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
        body: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Icon(Icons.error_outline, size: 48, color: Colors.red),
              const SizedBox(height: 16),
              Text(
                _errorMessage,
                textAlign: TextAlign.center,
                style: SpatialDesignSystem.bodyMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              const SizedBox(height: 16),
              SpatialButton(
                text: "Retry",
                onPressed: _loadDeliveryDetails,
                isGlass: true,
              ),
            ],
          ),
        ),
      );
    }

    return Scaffold(
      backgroundColor: isDark
          ? SpatialDesignSystem.darkBackgroundColor
          : SpatialDesignSystem.backgroundColor,
      appBar: AppBar(
        title: Text(
          "Delivery #${_deliveryDetail?.deliveryCode ?? widget.deliveryId}",
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
            onPressed: _loadDeliveryDetails,
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

  Widget _buildBottomBar() {
    if (_deliveryDetail == null) return const SizedBox.shrink();

    return Container(
      padding: const EdgeInsets.all(16),
      decoration: BoxDecoration(
        color: Theme.of(context).colorScheme.surface,
        boxShadow: [
          BoxShadow(
            color: Colors.black.withValues(alpha: 0.1),
            blurRadius: 10,
            offset: const Offset(0, -2),
          ),
        ],
      ),
      child: SafeArea(
        child: Row(
          children: [
            // Navigation Button
            Expanded(
              child: ElevatedButton.icon(
                onPressed: () => _openNavigation(),
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
                  backgroundColor:
                      SpatialDesignSystem.primaryColor, // Xanh lá cây
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
            "Route #${_deliveryDetail?.routeName ?? 'RT-${widget.deliveryId}'}",
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
                AlwaysStoppedAnimation<Color>(Colors.white),
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

          // Add tracking control button
          const SizedBox(height: 16),
          _buildTrackingControlButton(),
        ],
      ),
    );
  }

  Color _getStatusColor() {
    switch (_selectedStatus) {
      case 'PENDING':
        return Colors.blue.shade700;
      case 'PROCESSING':
        return Colors.amber.shade600;
      case 'SHIPPING':
        return Colors.deepPurple;
      case 'DELIVERED':
        return Colors.orange;
      case 'COMPLETED':
        return Colors.green.shade700;
      case 'CANCELLED':
        return Colors.red.shade700;
      default:
        return Colors.grey;
    }
  }

  double _getProgressValue() {
    switch (_selectedStatus) {
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
        return 0.0;
    }
  }

  String _getStatusDescription() {
    switch (_selectedStatus) {
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
        return "Unknown status";
    }
  }

  Widget _buildTrackingControlButton() {
    // Sử dụng Bloc để kiểm tra trạng thái tracking hiện tại
    return BlocBuilder<SimpleTrackingBloc, SimpleTrackingState>(
      builder: (context, state) {
        // Trích xuất deliveryId từ widget.deliveryId có thể ở định dạng "DEL-85797"
        int? currentDeliveryId;
        try {
          if (widget.deliveryId.contains('-')) {
            final parts = widget.deliveryId.split('-');
            if (parts.length > 1 && int.tryParse(parts[1]) != null) {
              currentDeliveryId = int.parse(parts[1]);
            }
          } else if (int.tryParse(widget.deliveryId) != null) {
            currentDeliveryId = int.parse(widget.deliveryId);
          }
        } catch (e) {
          print('Error parsing delivery ID: $e');
        }

        final isCurrentlyTracking = state is TrackingActiveState &&
            currentDeliveryId != null &&
            state.deliveryId == currentDeliveryId;

        return SpatialButton(
          text: _isTrackingLoading
              ? "Processing..."
              : (isCurrentlyTracking ? "Stop Tracking" : "Start Tracking"),
          iconData: _isTrackingLoading
              ? Icons.hourglass_top
              : (isCurrentlyTracking ? Icons.location_off : Icons.location_on),
          onPressed: _isTrackingLoading
              ? () {} // No-op function when disabled
              : () {
                  setState(() {
                    _isTrackingLoading = true;
                  });

                  print(
                      'Tracking button pressed. Current tracking state: ${isCurrentlyTracking ? 'active' : 'inactive'}');

                  if (isCurrentlyTracking) {
                    // Stop tracking - gọi tới bloc
                    context.read<SimpleTrackingBloc>().add(StopTrackingEvent());

                    print('Stopped tracking');

                    ScaffoldMessenger.of(context).showSnackBar(
                      const SnackBar(
                        content: Text('Background location tracking stopped'),
                        backgroundColor: Colors.blue,
                      ),
                    );
                  } else {
                    // Start tracking
                    if (_deliveryDetail == null) {
                      ScaffoldMessenger.of(context).showSnackBar(
                        const SnackBar(
                          content: Text(
                              'Cannot start tracking - delivery details not available'),
                          backgroundColor: Colors.red,
                        ),
                      );
                      setState(() {
                        _isTrackingLoading = false;
                      });
                      return;
                    }

                    try {
                      int deliveryId;

                      // Kiểm tra nếu ID có định dạng "DEL-85797"
                      if (widget.deliveryId.contains('-')) {
                        final parts = widget.deliveryId.split('-');
                        if (parts.length > 1 &&
                            int.tryParse(parts[1]) != null) {
                          deliveryId =
                              int.parse(parts[1]); // Lấy "85797" từ "DEL-85797"
                        } else {
                          throw FormatException(
                              'Không thể parse delivery ID: ${widget.deliveryId}');
                        }
                      } else {
                        deliveryId = int.parse(widget.deliveryId);
                      }

                      final vehicleId = _deliveryDetail!.vehicle?.id;

                      if (vehicleId == null) {
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(
                            content:
                                Text('Vehicle ID not found for this delivery'),
                            backgroundColor: Colors.red,
                          ),
                        );
                        setState(() {
                          _isTrackingLoading = false;
                        });
                        return;
                      }

                      // Start tracking using SimpleTrackingBloc
                      context.read<SimpleTrackingBloc>().add(
                            StartTrackingEvent(deliveryId,
                                vehicleId: vehicleId),
                          );

                      print(
                          'Started tracking for delivery #$deliveryId with vehicle #$vehicleId');

                      ScaffoldMessenger.of(context).showSnackBar(
                        const SnackBar(
                          content: Text('Background location tracking started'),
                          backgroundColor: Colors.green,
                        ),
                      );
                    } catch (e) {
                      ScaffoldMessenger.of(context).showSnackBar(
                        SnackBar(
                          content: Text('Error starting tracking: $e'),
                          backgroundColor: Colors.red,
                        ),
                      );
                      setState(() {
                        _isTrackingLoading = false;
                      });
                    }
                  }

                  // Note: We don't reset loading state here.
                  // The loading state will be reset automatically when the tracking state changes
                  // via the StreamSubscription in initState
                },
          width: double.infinity,
          height: 45, // Giảm chiều cao một chút
          padding: SpatialDesignSystem.paddingS, // Padding nhỏ hơn
          isGlass: true, // Sử dụng hiệu ứng kính (glass effect)
          textColor: _isTrackingLoading
              ? Colors.grey
              : (isCurrentlyTracking ? Colors.red : Colors.green),
          backgroundColor: _isTrackingLoading
              ? Colors.grey.withOpacity(0.05)
              : (isCurrentlyTracking
                  ? Colors.red.withOpacity(0.15)
                  : Colors.green.withOpacity(0.15)),
        );
      },
    );
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
                  _formatDate(_deliveryDetail?.scheduledTime ??
                      _deliveryDetail?.createdAt),
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.schedule,
                  "Estimated Time",
                  _deliveryDetail?.estimatedDuration ?? "Not available",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.location_on,
                  "Starting Point",
                  _deliveryDetail?.pickupAddress ?? "KTC Warehouse",
                  allowMultiLine: true,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.location_city,
                  "Destination Area",
                  _getDestinationArea(),
                  allowMultiLine: true,
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.route,
                  "Total Distance",
                  _deliveryDetail?.estimatedDistance ?? "Not available",
                ),
                const Divider(),
                _buildInfoRow(
                  Icons.shopping_bag,
                  "Total Orders",
                  "${_deliveryDetail?.orders.length ?? 0} orders",
                ),
              ],
            ),
          ),

          const SizedBox(height: 16),

          // // Status Update for All Orders in this Delivery
          // GlassCard(
          //   padding: const EdgeInsets.all(16),
          //   child: Column(
          //     crossAxisAlignment: CrossAxisAlignment.start,
          //     children: [
          //       Text(
          //         "Update Status",
          //         style: SpatialDesignSystem.subtitleMedium.copyWith(
          //           color: isDark
          //               ? SpatialDesignSystem.textDarkPrimaryColor
          //               : SpatialDesignSystem.textPrimaryColor,
          //         ),
          //       ),
          //       const SizedBox(height: 16),
          //       DropdownButtonFormField<String>(
          //         value: _selectedStatus,
          //         decoration: InputDecoration(
          //           labelText: 'Status',
          //           border: OutlineInputBorder(
          //             borderRadius: BorderRadius.circular(10),
          //           ),
          //           filled: true,
          //           fillColor: isDark
          //               ? Colors.black.withValues(alpha: 0.2)
          //               : Colors.white.withValues(alpha: 0.8),
          //         ),
          //         items: _statusOptions.map((status) {
          //           return DropdownMenuItem<String>(
          //             value: status,
          //             child: Text(status),
          //           );
          //         }).toList(),
          //         onChanged: (newValue) {
          //           if (newValue != null) {
          //             setState(() {
          //               _selectedStatus = newValue;
          //             });
          //           }
          //         },
          //       ),
          //       const SizedBox(height: 16),
          //       SpatialTextField(
          //         controller: _noteController,
          //         label: "Notes",
          //         hint: "Add notes about status change",
          //         maxLines: 3,
          //         isGlass: true,
          //       ),
          //       const SizedBox(height: 16),
          //       SpatialButton(
          //         text: "Update Status",
          //         onPressed: () => _updateDeliveryStatus(_selectedStatus),
          //         isGlass: true,
          //         padding:
          //             const EdgeInsets.symmetric(horizontal: 16, vertical: 10),
          //         height: 40,
          //         width: double.infinity,
          //       ),
          //     ],
          //   ),
          // ),

          // const SizedBox(height: 16),

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
                // Build timeline with statuses in order
                _buildTimelineTile(
                  "Assigned to Driver",
                  _formatDate(_deliveryDetail?.createdAt),
                  "Delivery route assigned to ${_deliveryDetail?.driver?.fullName ?? 'Driver'}",
                  _isStatusCompleted("Assigned"),
                  isFirst: true,
                ),
                _buildTimelineTile(
                  "Started Journey",
                  _getTimelineDate("Started"),
                  "Driver has started the delivery route",
                  _isStatusCompleted("Started"),
                ),
                _buildTimelineTile(
                  "In Progress",
                  _getTimelineDate("In Progress"),
                  "Currently delivering orders",
                  _isStatusCompleted("In Progress"),
                ),
                _buildTimelineTile(
                  "Completed",
                  _deliveryDetail?.actualDeliveryTime != null
                      ? _formatDate(_deliveryDetail?.actualDeliveryTime)
                      : "Estimated ${_formatDate(_deliveryDetail?.scheduledTime, addHours: 2)}",
                  "All orders delivered successfully",
                  _isStatusCompleted("Completed"),
                  isLast: true,
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
                  maxLines: allowMultiLine ? null : 1,
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
        color: isCompleted
            ? SpatialDesignSystem
                .primaryColor // Use blue color for completed part
            : isDark
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
    final isDark = Theme.of(context).brightness == Brightness.dark;

    // If no data from API, display message
    if (_deliveryDetail == null || _deliveryDetail!.orders.isEmpty) {
      return Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Icon(
              Icons.shopping_bag_outlined,
              size: 64,
              color: isDark
                  ? SpatialDesignSystem.textDarkSecondaryColor
                  : SpatialDesignSystem.textSecondaryColor,
            ),
            const SizedBox(height: 16),
            Text(
              "No orders available",
              style: SpatialDesignSystem.subtitleMedium.copyWith(
                color: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
              ),
            ),
          ],
        ),
      );
    }

    // Use data from API to display order list
    return ListView.builder(
      padding: const EdgeInsets.all(16),
      itemCount: _deliveryDetail!.orders.length,
      itemBuilder: (context, index) {
        final order = _deliveryDetail!.orders[index];
        return GestureDetector(
          onTap: () {
            Navigator.push(
              context,
              MaterialPageRoute(
                builder: (context) =>
                    OrderDetailScreen(orderId: order.id.toString()),
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
                        "Order #${order.orderCode ?? order.id}",
                        style: SpatialDesignSystem.subtitleSmall.copyWith(
                          color: SpatialDesignSystem.primaryColor,
                          fontWeight: FontWeight.w600,
                        ),
                      ),
                      Container(
                        padding: const EdgeInsets.symmetric(
                            horizontal: 8, vertical: 4),
                        decoration: BoxDecoration(
                          color: _getOrderStatusColor(order.status ?? 'Pending')
                              .withOpacity(0.2),
                          borderRadius: BorderRadius.circular(20),
                        ),
                        child: Text(
                          order.status ?? 'Pending',
                          style: SpatialDesignSystem.captionText.copyWith(
                            color:
                                _getOrderStatusColor(order.status ?? 'Pending'),
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
                        order.recipientName ?? 'Not available',
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
                        _formatCurrency(order.totalAmount),
                        style: SpatialDesignSystem.bodyMedium.copyWith(
                          fontWeight: FontWeight.w500,
                        ),
                      ),
                    ],
                  ),
                  const SizedBox(height: 16),

                  // Add order status update section
                  Row(
                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                    children: [
                      Expanded(
                        child: SpatialButton(
                          text: "View Details",
                          onPressed: () {
                            Navigator.push(
                              context,
                              MaterialPageRoute(
                                builder: (context) => OrderDetailScreen(
                                    orderId: order.id.toString()),
                              ),
                            );
                          },
                          isGlass: true,
                          padding: const EdgeInsets.symmetric(
                              horizontal: 10, vertical: 8),
                          height: 50,
                        ),
                      ),
                      const SizedBox(width: 8),
                      Expanded(
                        child: SpatialButton(
                          text: "Update Status",
                          textColor: SpatialDesignSystem.textPrimaryColor,
                          onPressed: () {
                            _showOrderStatusUpdateDialog(order);
                          },
                          backgroundColor:
                              SpatialDesignSystem.surfaceColor.withOpacity(0.9),
                          padding: const EdgeInsets.symmetric(
                              horizontal: 10, vertical: 8),
                          height: 50,
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

  // Helper methods

  // Format date from API string to readable format
  String _formatDate(String? dateString, {int addHours = 0}) {
    if (dateString == null || dateString.isEmpty) {
      return "Not available";
    }

    try {
      final date = DateTime.parse(dateString).add(Duration(hours: addHours));
      final formatter = DateFormat('dd/MM/yyyy | HH:mm');
      return formatter.format(date);
    } catch (e) {
      return dateString;
    }
  }

  // Get the destination area from the store address
  String _getDestinationArea() {
    if (_deliveryDetail == null || _deliveryDetail!.orders.isEmpty) {
      return "Not available";
    }

    // Prioritize using store address
    if (_deliveryDetail!.store != null &&
        _deliveryDetail!.store!.address != null) {
      return _deliveryDetail!.store!.address!;
    }

    // If no store information, use address from first order
    String? address = _deliveryDetail!.orders.first.deliveryAddress;
    if (address == null || address.isEmpty) {
      return "Address not available";
    }

    // Display full address without truncation
    return address;
  }

  // Check if a status should be shown as completed based on current status
  bool _isStatusCompleted(String status) {
    // List of statuses in progression order
    const statusOrder = ['Assigned', 'Started', 'In Progress', 'Completed'];
    final currentIndex = statusOrder.indexOf(_selectedStatus);
    final statusIndex = statusOrder.indexOf(status);

    // If current status has reached or passed the status being checked
    if (currentIndex >= statusIndex && currentIndex >= 0 && statusIndex >= 0) {
      return true;
    }

    // Special case: If completed, all statuses are completed
    if (_selectedStatus == 'Completed') {
      return true;
    }

    return false;
  }

  // Get date for timeline item
  String _getTimelineDate(String status) {
    switch (status) {
      case 'Started':
        return "Estimated ${_formatDate(_deliveryDetail?.scheduledTime, addHours: -1)}";
      case 'In Progress':
        return "Estimated ${_formatDate(_deliveryDetail?.scheduledTime)}";
      default:
        return "Not available";
    }
  }

  // Format currency from number to string
  String _formatCurrency(double? amount) {
    if (amount == null) {
      return "0 VND";
    }

    // Format with thousands separator
    final formatter = NumberFormat("#,###", "vi_VN");
    return "${formatter.format(amount)} VND";
  }

  // Get color for order status
  Color _getOrderStatusColor(String status) {
    switch (status.toUpperCase()) {
      case 'PENDING':
        return SpatialDesignSystem.primaryColor; // Blue for awaiting processing
      case 'PROCESSING':
        return SpatialDesignSystem.warningColor; // Orange/Yellow for processing
      case 'SHIPPING':
        return Colors.deepPurple; // Purple for shipping/in transit
      case 'DELIVERED':
        return Colors.orange; // Orange for delivered but not paid
      case 'COMPLETED':
        return SpatialDesignSystem.successColor; // Green for completed
      case 'CANCELLED':
        return SpatialDesignSystem.errorColor; // Red for cancelled
      default:
        return Colors.grey;
    }
  }

  // Show dialog to update order status
  void _showOrderStatusUpdateDialog(dynamic order) {
    // List of available order statuses for delivery
    // Thứ tự hiển thị: Pending -> Processing -> Shipping -> Delivered -> Completed -> Cancelled
    // ID theo database và OrderStatusId constants
    final List<Map<String, dynamic>> statusOptions = [
      {"id": OrderStatusId.PENDING, "name": "PENDING", "display": "Pending"},
      {"id": OrderStatusId.PROCESSING, "name": "PROCESSING", "display": "Processing"},
      {"id": OrderStatusId.SHIPPING, "name": "SHIPPING", "display": "Shipping"},
      {"id": OrderStatusId.DELIVERED, "name": "DELIVERED", "display": "Delivered"},
      {"id": OrderStatusId.COMPLETED, "name": "COMPLETED", "display": "Completed"},
      {"id": OrderStatusId.CANCELLED, "name": "CANCELLED", "display": "Cancelled"}
    ];

    // Convert API status to status name
    String currentStatus = _mapApiStatusToStatusName(order.status ?? 'PENDING');

    showStatusUpdateModal(
      context: context,
      title: "Update Order Status",
      itemId: "Order #${order.orderCode ?? order.id}",
      currentStatus: currentStatus,
      statusOptions: statusOptions,
      getStatusColor: _getStatusColorFromName,
      onUpdateStatus: (String status, String notes) async {
        // Create the update model using OrderStatusId.fromStatusName for consistency
        final statusUpdate = OrderStatusUpdate(
          statusId: OrderStatusId.fromStatusName(status),
          notes: notes.isNotEmpty ? notes : null,
        );

        // Call API service to update order status
        final success = await ordersServices.updateOrderStatus(
          orderId: order.id,
          statusUpdate: statusUpdate,
        );

        if (success) {
          // Show success message
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(
              content: Text('Order status updated to $status'),
              backgroundColor: SpatialDesignSystem.successColor,
            ),
          );

          // Refresh delivery details to show updated status
          _loadDeliveryDetails();
        } else {
          // Show error message
          ScaffoldMessenger.of(context).showSnackBar(
            const SnackBar(
              content: Text('Failed to update order status'),
              backgroundColor: Colors.red,
            ),
          );
        }
      },
    );
  }

  // Helper method to map API status to ORDER status name
  String _mapApiStatusToStatusName(String apiStatus) {
    switch (apiStatus.toUpperCase()) {
      // Map delivery statuses to order statuses
      case 'ASSIGNED':
      case 'CREATED':
        return 'PENDING';
      case 'STARTED':
        return 'PROCESSING';
      case 'IN_PROGRESS':
      case 'INPROGRESS':
      case 'IN_TRANSIT':
        return 'SHIPPING';
      // Direct mappings for standard order statuses
      case 'PENDING':
        return 'PENDING';
      case 'PROCESSING':
        return 'PROCESSING';
      case 'SHIPPING':
        return 'SHIPPING';
      case 'DELIVERED':
        return 'DELIVERED';
      case 'COMPLETED':
        return 'COMPLETED';
      case 'CANCELLED':
        return 'CANCELLED';
      default:
        return 'PENDING';
    }
  }

  // Helper method to get status color from name
  Color _getStatusColorFromName(String statusName) {
    switch (statusName.toUpperCase()) {
      case 'PENDING':
        return SpatialDesignSystem.primaryColor; // Blue for awaiting processing
      case 'PROCESSING':
        return SpatialDesignSystem.warningColor; // Orange/Yellow for processing
      case 'SHIPPING':
        return Colors.deepPurple; // Purple for shipping/in transit
      case 'DELIVERED':
        return Colors.orange; // Orange for delivered but not paid
      case 'COMPLETED':
        return SpatialDesignSystem.successColor; // Green for completed
      case 'CANCELLED':
        return SpatialDesignSystem.errorColor; // Red for cancelled
      default:
        return SpatialDesignSystem.primaryColor; // Default to primary color
    }
  }

  // Update delivery status via API

  /// Open navigation to pickup/delivery location
  void _openNavigation() {
    if (_deliveryDetail == null) return;

    // Use the existing navigation method
    _navigateToRouteMap();
  }

  /// Start tracking for this specific delivery

  /// Stop tracking

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

      // Check if we have the delivery detail data
      if (_deliveryDetail == null) {
        throw Exception('Delivery details not available');
      }

      // Check if there are any orders
      if (_deliveryDetail!.orders.isEmpty) {
        throw Exception('No delivery orders found');
      }

      final googleMapsService = GoogleMapsService();

      // Get current position for navigation (ensures we start from actual current location)
      final currentPositionData =
          await googleMapsService.getCurrentPositionData();

      // Default destination coordinates if not available from API
      Map<String, dynamic> storeLocation = {
        'latitude': 10.7756,
        'longitude': 106.7019,
        'address': 'Destination Address'
      };

      // Get real coordinates from the first order in delivery detail
      if (_deliveryDetail!.orders.isNotEmpty &&
          _deliveryDetail!.orders.first.store != null) {
        final store = _deliveryDetail!.orders.first.store!;

        if (store.latitude != null && store.longitude != null) {
          // Convert to double if needed (similar to order detail screen)
          double latitude = store.latitude!.toDouble();
          double longitude = store.longitude!.toDouble();
          
          // Use store coordinates for navigation (pickup location)
          storeLocation = {
            'latitude': latitude,
            'longitude': longitude,
            'address': store.address ?? 'Store Address'
          };
          
          // Debug coordinates information
          debugPrint('=== DELIVERY NAVIGATION DEBUG ===');
          debugPrint('Using store coordinates:');
          debugPrint('Latitude: $latitude');
          debugPrint('Longitude: $longitude');
          debugPrint('Address: ${store.address}');
          debugPrint('=================================');
        } else if (store.address != null && store.address!.isNotEmpty) {
          // If no coordinates available, use address for Google Maps search
          storeLocation = {
            'latitude': 0.0, // Google Maps will search based on address
            'longitude': 0.0,
            'address': store.address!
          };
          
          debugPrint('=== DELIVERY NAVIGATION DEBUG ===');
          debugPrint('No store coordinates found, using address search');
          debugPrint('Address: ${store.address}');
          debugPrint('=================================');
        }
      } else {
        debugPrint('=== DELIVERY NAVIGATION DEBUG ===');
        debugPrint('No store information found, using default coordinates');
        debugPrint('=================================');
      }

      // Start tracking service
      final driverId = _deliveryDetail?.driver?.id ??
          20696; // Default driver ID if not available
      final deliveryId = _deliveryDetail?.id ?? int.parse(widget.deliveryId);
      final vehicleId = _deliveryDetail?.vehicle?.id ??
          1; // Default vehicle ID if not available
      final statusId = OrderStatusId.fromStatusName(_selectedStatus);

      // Start background tracking service
      final locationService = LocationService();
      if (!locationService.isTracking) {
        await locationService.startDeliveryTracking(
          driverId: driverId,
          deliveryId: deliveryId,
          vehicleId: vehicleId,
          statusId: statusId,
        );

        // Show a message that tracking has started
        if (context.mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            const SnackBar(
              content: Text('Background location tracking started'),
              backgroundColor: Colors.green,
              duration: Duration(seconds: 3),
            ),
          );
        }
      }

      // Open Google Maps with direct route from current location to destination
      final result = await googleMapsService.openGoogleMapsWithRoute(
        context: context,
        pickupLocation:
            currentPositionData, // Use actual current position from device
        transitPoints: [], // No transit points
        deliveryLocation: storeLocation,
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
}
