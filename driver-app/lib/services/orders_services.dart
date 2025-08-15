import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/domain/models/product_cart.dart';
import 'package:ktc_logistics_driver/domain/models/response/order_details_response.dart';
import 'package:ktc_logistics_driver/domain/models/response/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/domain/models/response/orders_client_response.dart';
import 'package:ktc_logistics_driver/domain/models/response/response_default.dart';


class OrdersServices {

  final _env = Environment.getInstance();

  Future<ResponseDefault> addNewOrders(int uidAddress, double total, String typePayment, List<ProductCart> products) async {

    final token = await secureStorage.readToken();

    Map<String, dynamic> data = {
      "uidAddress"  : uidAddress,
      "typePayment": typePayment,
      "total"       : total,
      "products"    : products 
    };

    final resp = await http.post(Uri.parse('${_env.endpointApi}/add-new-orders'),
      headers: {'Content-type' : 'application/json', 'xx-token' : token!},
      body: json.encode(data)
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }


  Future<List<OrdersResponse>> getOrdersByStatus( String status ) async {

    final token = await secureStorage.readToken();

    final resp = await http.get(Uri.parse('${_env.endpointApi}/get-orders-by-status/$status'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!},
    );
    return OrdersByStatusResponse.fromJson(jsonDecode(resp.body)).ordersResponse;
  }


  Future<List<DetailsOrder>> gerOrderDetailsById(String idOrder) async {

    final token = await secureStorage.readToken();

    final resp = await http.get(Uri.parse('${_env.endpointApi}/get-details-order-by-id/$idOrder'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!},
    );
    return OrderDetailsResponse.fromJson( jsonDecode(resp.body)).detailsOrder;
  }


  Future<ResponseDefault> updateStatusOrderToDispatched(String idOrder, String idDelivery) async {

    final token = await secureStorage.readToken();

    final resp = await http.put(Uri.parse('${_env.endpointApi}/update-status-order-dispatched'),
      headers: { 'Accept' : 'application/json', 'xx-token' : token! },
      body: {
        'idDelivery' : idDelivery,
        'idOrder' : idOrder
      }
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }


  Future<ResponseDefault> updateOrderStatusOnWay( String idOrder, String latitude, String longitude ) async {

    final token = await secureStorage.readToken();

    final resp = await http.put(Uri.parse('${_env.endpointApi}/update-status-order-on-way/$idOrder'),
      headers: { 'Accept' : 'application/json', 'xx-token' : token! },
      body: {
        'latitude' : latitude,
        'longitude' : longitude
      }
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }
  

  Future<ResponseDefault> updateOrderStatusDelivered(String idOrder) async {

    final token = await secureStorage.readToken();

    final resp = await http.put(Uri.parse('${_env.endpointApi}/update-status-order-delivered/$idOrder'),
      headers: { 'Accept' : 'application/json', 'xx-token' : token! },
    );
    return ResponseDefault.fromJson( jsonDecode( resp.body ));
  }
  

  Future<List<OrdersClient>> getListOrdersForClient() async {

    final token = await secureStorage.readToken();

    final resp = await http.get(Uri.parse('${_env.endpointApi}/get-list-orders-for-client'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );
    
    return OrdersClientResponse.fromJson( jsonDecode(resp.body)).ordersClient;
  }



}

final ordersServices = OrdersServices();


