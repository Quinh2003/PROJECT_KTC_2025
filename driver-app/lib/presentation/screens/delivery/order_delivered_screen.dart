import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:ktc_logistics_driver/domain/models/response/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/services/delivery_services.dart';
import 'package:ktc_logistics_driver/presentation/components/card_orders_delivery.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/delivery_home_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/order_details_delivery_screen.dart';
import 'package:ktc_logistics_driver/presentation/themes/colors_frave.dart';


class OrderDeliveredScreen extends StatelessWidget {

  @override
  Widget build(BuildContext context){

    return Scaffold(
      backgroundColor: Colors.white,
      appBar: AppBar(
        backgroundColor: Colors.white,
        title: const TextCustom(text: 'Orders Delivered'),
        centerTitle: true,
        elevation: 0,
        leadingWidth: 80,
        leading: InkWell(
          onTap: () => Navigator.push(context, routeFrave(page: DeliveryHomeScreen())),
          child: Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: const [
              Icon(Icons.arrow_back_ios_new_rounded, size: 17, color: ColorsFrave.primaryColor),
              TextCustom(text: 'Back', fontSize: 17, color: ColorsFrave.primaryColor )
            ],
          ),
        ),
      ),
      body: FutureBuilder<List<OrdersResponse>>(
        future: deliveryServices.getOrdersForDelivery('DELIVERED'),
        builder: (context, snapshot) 
          => ( !snapshot.hasData )
            ? Column(
                children: const [
                  ShimmerFrave(),
                  SizedBox(height: 10.0),
                  ShimmerFrave(),
                  SizedBox(height: 10.0),
                  ShimmerFrave(),
                ],
              )
            : _ListOrdersForDelivery(listOrdersDelivery: snapshot.data!)
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
          itemBuilder: (_, i) => 
            CardOrdersDelivery(
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
          const TextCustom(text: 'Without Orders delivered', color: ColorsFrave.primaryColor, fontWeight: FontWeight.w500, fontSize: 21)
        ],
      );
  }
}


