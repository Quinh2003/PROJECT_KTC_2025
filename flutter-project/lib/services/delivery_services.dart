import 'dart:convert';
import 'package:flutter/foundation.dart';
import 'package:http/http.dart' as http;
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/get_all_delivery_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/orders_by_status_response.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_details_response.dart';
import 'package:ktc_logistics_driver/services/orders_services.dart';


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
  // Không khuyến khích sử dụng - thay vào đó sử dụng ordersServices.getDriverOrdersList
  @Deprecated('Use ordersServices.getDriverOrdersList instead')
  Future<List<OrdersResponse>> getDriverOrders(int driverId) async {
    try {
      // Sử dụng OrdersServices để tránh trùng lặp code
      return await ordersServices.getDriverOrdersList(driverId);
    } catch (e) {
      debugPrint('Error getting driver orders: $e');
      return [];
    }
  }
  
  // Lấy chi tiết đơn hàng
  // Không khuyến khích sử dụng - thay vào đó sử dụng ordersServices.getDriverOrderDetail
  @Deprecated('Use ordersServices.getDriverOrderDetail instead')
  Future<OrderDetailsResponse?> getOrderDetail(int orderId) async {
    try {
      return await ordersServices.getDriverOrderDetail(orderId);
    } catch (e) {
      debugPrint('Error getting order detail: $e');
      return null;
    }
  }

  // Cập nhật trạng thái đơn hàng
  // Không khuyến khích sử dụng - thay vào đó sử dụng ordersServices.updateDriverOrderStatus
  @Deprecated('Use ordersServices.updateDriverOrderStatus instead')
  Future<bool> updateOrderStatus(int orderId, String status) async {
    try {
      final result = await ordersServices.updateDriverOrderStatus(orderId, status, '');
      return result.resp;
    } catch (e) {
      debugPrint('Error updating order status: $e');
      return false;
    }
  }
}

final deliveryServices = DeliveryServices();
