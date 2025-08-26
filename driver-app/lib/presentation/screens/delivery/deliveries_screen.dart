import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:ktc_logistics_driver/domain/models/order/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/presentation/components/card_orders_delivery.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/order_details_delivery_screen.dart';

class DeliveriesScreen extends StatefulWidget {
  const DeliveriesScreen({super.key});

  @override
  State<DeliveriesScreen> createState() => _DeliveriesScreenState();
}

class _DeliveriesScreenState extends State<DeliveriesScreen> with SingleTickerProviderStateMixin {
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
        title: TextCustom(
          text: 'Deliveries', 
          color: isDark
            ? SpatialDesignSystem.textDarkPrimaryColor
            : SpatialDesignSystem.textPrimaryColor,
        ),
        centerTitle: true,
        elevation: 0,
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
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          _buildNextDeliveriesCard(context),
          // const SizedBox(height: 24),
          // Text(
          //   "All Upcoming Deliveries",
          //   style: SpatialDesignSystem.subtitleLarge.copyWith(
          //     color: isDark
          //         ? SpatialDesignSystem.textDarkPrimaryColor
          //         : SpatialDesignSystem.textPrimaryColor,
          //   ),
          // ),
          // const SizedBox(height: 16),
          // BlocBuilder<DeliveryBloc, DeliveryState>(
          //   builder: (context, state) {
          //     // TODO: Replace with proper delivery orders loading from DeliveryBloc
          //     // For now, return empty list to avoid deliveryServices error
          //     return SizedBox(
          //       height: 500,
          //       child: _ListOrdersForDelivery(listOrdersDelivery: [])
          //     );
          //   }
          // ),
        ],
      ),
    );
  }

  Widget _buildNextDeliveriesCard(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
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
            ],
          ),
        ),
      ],
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
                              ? SpatialDesignSystem.primaryColor.withOpacity(0.1)
                              : SpatialDesignSystem.warningColor.withOpacity(0.1),
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
          _buildDoneOrdersSection(context),
        ],
      ),
    );
  }

  Widget _buildDoneOrdersSection(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        GlassCard(
          padding: const EdgeInsets.all(20),
          child: Column(
            children: [
              _buildOrderItem(
                context,
                "ORD-2025-08-14-034",
                "123 Le Loi Street, District 1",
                "Today, 11:45 AM",
                "Done",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildOrderItem(
                context,
                "ORD-2025-08-14-029",
                "456 Nguyen Hue Street, District 1",
                "Today, 09:30 AM",
                "Done",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildOrderItem(
                context,
                "ORD-2025-08-14-018",
                "789 Pasteur Street, District 3",
                "Today, 08:15 AM",
                "Done",
                SpatialDesignSystem.successColor,
              ),
              const Divider(),
              _buildOrderItem(
                context,
                "ORD-2025-08-13-095",
                "321 Nam Ky Khoi Nghia Street, District 3",
                "Yesterday, 03:20 PM",
                "Failed",
                SpatialDesignSystem.errorColor,
              ),
              const Divider(),
              _buildOrderItem(
                context,
                "ORD-2025-08-13-078",
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
                  status == "Done" ? Icons.check_circle_outline : Icons.error_outline,
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

class _ListOrdersForDelivery extends StatelessWidget {
  
  final List<OrdersResponse> listOrdersDelivery;

  const _ListOrdersForDelivery({ required this.listOrdersDelivery});

  @override
  Widget build(BuildContext context) {
    return ( listOrdersDelivery.isNotEmpty ) 
      ? ListView.builder(
          itemCount: listOrdersDelivery.length,
          itemBuilder: (_, i) 
            => CardOrdersDelivery(
                orderResponse: listOrdersDelivery[i],
                onPressed: () => Navigator.push(context, routeFrave(page: OrdersDetailsDeliveryScreen(order: listOrdersDelivery[i]))),
               )
        )
      : Column(
        mainAxisAlignment: MainAxisAlignment.center,
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          Center(child: SvgPicture.asset('assets/svg/no-data.svg', height: 300)),
          const SizedBox(height: 15.0),
          TextCustom(
            text: 'No Orders Available', 
            color: SpatialDesignSystem.primaryColor, 
            fontWeight: FontWeight.w500, 
            fontSize: 21
          )
        ],
      );
  }
}
