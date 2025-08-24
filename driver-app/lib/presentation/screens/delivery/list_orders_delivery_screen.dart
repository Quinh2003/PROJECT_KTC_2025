import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:ktc_logistics_driver/domain/models/order/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/presentation/components/card_orders_delivery.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/order_details_delivery_screen.dart';


class ListOrdersDeliveryScreen extends StatelessWidget {
  const ListOrdersDeliveryScreen({super.key});


  @override
  Widget build(BuildContext context){
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
          text: 'List of orders', 
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
              )
            ],
          ),
        ),
      ),
      body: BlocBuilder<DeliveryBloc, DeliveryState>(
        builder: (context, state) {
          // TODO: Replace with proper delivery orders loading from DeliveryBloc
          // For now, return empty list to avoid deliveryServices error
          return _ListOrdersForDelivery(listOrdersDelivery: []);
        }
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


