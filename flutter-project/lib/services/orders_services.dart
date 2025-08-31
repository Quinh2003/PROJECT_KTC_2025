import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/domain/models/order/product_cart.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_details_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/orders_client_response.dart';
import 'package:ktc_logistics_driver/domain/models/common/response_default.dart';


class OrdersServices {

  final _env = Environment.getInstance();

  // Driver API Methods
  Future<List<OrdersResponse>> getDriverOrdersList(String driverId) async {
    try {
      final token = await secureStorage.readToken();
      
      final resp = await http.get(
        Uri.parse('${_env.endpointApi}/driver/$driverId/orders'),
        headers: {'Content-type': 'application/json', 'xx-token': token!},
      );

      if (resp.statusCode == 200) {
        final listOrdersResponse = OrdersByStatusResponse.fromJson(json.decode(resp.body));
        return listOrdersResponse.ordersResponse;
      }
      
      return [];
    } catch (e) {
      return [];
    }
  }

  Future<OrderDetailsResponse?> getDriverOrderDetail(String orderId) async {
    try {
      final token = await secureStorage.readToken();
      
      final resp = await http.get(
        Uri.parse('${_env.endpointApi}/driver/orders/$orderId'),
        headers: {'Content-type': 'application/json', 'xx-token': token!},
      );

      if (resp.statusCode == 200) {
        final orderData = json.decode(resp.body);
        return OrderDetailsResponse.fromJson(orderData);
      }
      
      return null;
    } catch (e) {
      return null;
    }
  }

  Future<ResponseDefault> updateDriverOrderStatus(String orderId, String status, String notes) async {
    try {
      final token = await secureStorage.readToken();
      
      Map<String, dynamic> data = {
        "status": status,
        "notes": notes
      };

      final resp = await http.put(
        Uri.parse('${_env.endpointApi}/driver/orders/$orderId/status'),
        headers: {'Content-type': 'application/json', 'xx-token': token!},
        body: json.encode(data)
      );

      return ResponseDefault.fromJson(json.decode(resp.body));
    } catch (e) {
      return ResponseDefault(
        resp: false, 
        msg: e.toString()
      );
    }
  }

  // Existing Methods
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

  // Lấy danh sách đơn hàng của tài xế theo ID
  Future<List<dynamic>> getDriverOrders(int driverId) async {
    final token = await secureStorage.readToken();

    try {
      final resp = await http.get(
        Uri.parse('${_env.apiBaseUrl}/driver/$driverId/orders'),
        headers: {'Accept': 'application/json', 'Authorization': 'Bearer $token'}
      );

      if (resp.statusCode == 200) {
        final List<dynamic> data = json.decode(resp.body);
        return data;
      } else {
        print('Error: ${resp.statusCode} - ${resp.body}');
        return [];
      }
    } catch (e) {
      print('Exception: $e');
      return [];
    }
  }

  // Lấy chi tiết đơn hàng theo ID
  Future<Map<String, dynamic>?> getOrderDetail(int orderId) async {
    final token = await secureStorage.readToken();

    try {
      final resp = await http.get(
        Uri.parse('${_env.apiBaseUrl}/driver/orders/$orderId'),
        headers: {'Accept': 'application/json', 'Authorization': 'Bearer $token'}
      );

      if (resp.statusCode == 200) {
        final Map<String, dynamic> data = json.decode(resp.body);
        return data;
      } else {
        print('Error: ${resp.statusCode} - ${resp.body}');
        return null;
      }
    } catch (e) {
      print('Exception: $e');
      return null;
    }
  }

  // Cập nhật trạng thái đơn hàng
  Future<bool> updateOrderStatus(int orderId, String status) async {
    final token = await secureStorage.readToken();

    try {
      final resp = await http.put(
        Uri.parse('${_env.apiBaseUrl}/driver/orders/$orderId/status'),
        headers: {
          'Content-Type': 'application/json', 
          'Authorization': 'Bearer $token'
        },
        body: json.encode({'status': status})
      );

      return resp.statusCode == 200;
    } catch (e) {
      print('Exception: $e');
      return false;
    }
  }
}

final ordersServices = OrdersServices();

