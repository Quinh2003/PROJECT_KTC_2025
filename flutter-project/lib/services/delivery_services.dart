import 'dart:convert';
import 'package:flutter/foundation.dart';
import 'package:http/http.dart' as http;
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/get_all_delivery_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_details_response.dart';


class DeliveryServices {

  final _env = Environment.getInstance();

  Future<List<Delivery>> getAlldelivery() async {

    final token = await secureStorage.readToken();

    final resp = await http.get(Uri.parse('${_env.endpointApi}/get-all-delivery'),
      headers: { 'Accept' : 'application/json', 'xx-token' : token! }
    );

    return GetAllDeliveryResponse.fromJson(jsonDecode(resp.body)).delivery;
  }


  Future<List<OrdersResponse>> getOrdersForDelivery(String statusOrder) async {

    final token = await secureStorage.readToken();

    final resp = await http.get(Uri.parse('${_env.endpointApi}/get-all-orders-by-delivery/$statusOrder'),
      headers: { 'Accept' : 'application/json', 'xx-token' : token! }
    );

    return OrdersByStatusResponse.fromJson(jsonDecode(resp.body)).ordersResponse;
  }

  // Lấy danh sách đơn hàng của tài xế - sử dụng model OrdersResponse
  Future<List<OrdersResponse>> getDriverOrders(int driverId) async {
    try {
      // Sử dụng API đã có trực tiếp
      return await this.getOrdersForDelivery("all");
    } catch (e) {
      debugPrint('Error getting driver orders: $e');
      return [];
    }
  }
  
  // Lấy chi tiết đơn hàng
  Future<OrderDetailsResponse?> getOrderDetail(int orderId) async {
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
      debugPrint('Error getting order detail: $e');
      return null;
    }
  }

  // Cập nhật trạng thái đơn hàng
  Future<bool> updateOrderStatus(int orderId, String status) async {
    try {
      final token = await secureStorage.readToken();

      final resp = await http.put(
        Uri.parse('${_env.endpointApi}/driver/orders/$orderId/status'),
        headers: { 'Accept' : 'application/json', 'xx-token' : token! },
        body: jsonEncode({'status': status}),
      ).timeout(const Duration(seconds: 15));

      return resp.statusCode == 200;
    } catch (e) {
      debugPrint('Error updating order status: $e');
      return false;
    }
  }
}

final deliveryServices = DeliveryServices();
