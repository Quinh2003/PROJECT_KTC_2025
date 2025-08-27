import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:ktc_logistics_driver/domain/models/response/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/presentation/components/card_orders_delivery.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/order_details_delivery_screen.dart';
import 'package:ktc_logistics_driver/presentation/themes/colors_frave.dart';


class ListOrdersDeliveryScreen extends StatelessWidget {

  @override
  Widget build(BuildContext context){

    return Scaffold(
      backgroundColor: Colors.white,
      appBar: AppBar(
        backgroundColor: Colors.white,
        title: const TextCustom(text: 'List of orders'),
        centerTitle: true,
        elevation: 0,
        leadingWidth: 80,
        leading: InkWell(
          onTap: () => Navigator.pop(context),
          child: Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: const [
              Icon(Icons.arrow_back_ios_new_rounded, size: 19, color: ColorsFrave.primaryColor ),
              TextCustom(text: 'Back', fontSize: 17, color: ColorsFrave.primaryColor )
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
    return ( listOrdersDelivery.length != 0 ) 
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
          const TextCustom(text: 'Without Orders', color: ColorsFrave.primaryColor, fontWeight: FontWeight.w500, fontSize: 21)
        ],
      );
  }
}


