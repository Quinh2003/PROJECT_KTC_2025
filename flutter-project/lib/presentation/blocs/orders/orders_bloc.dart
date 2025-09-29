import 'dart:async';
import 'package:bloc/bloc.dart';
import 'package:google_maps_flutter/google_maps_flutter.dart';
import 'package:meta/meta.dart';
import 'package:ktc_logistics_driver/domain/models/order/product_cart.dart';
import 'package:ktc_logistics_driver/domain/models/order/order_status_update.dart';
import 'package:ktc_logistics_driver/services/orders_services.dart';

part 'orders_event.dart';
part 'orders_state.dart';

class OrdersBloc extends Bloc<OrdersEvent, OrdersState> {
  final OrdersServices ordersServices;

  OrdersBloc({required this.ordersServices}) : super(OrdersState()) {
    // on<OnAddNewOrdersEvent>(_onAddNewOrders);
    on<OnUpdateStatusOrderToDispatchedEvent>(_onUpdateStatusOrderToDispatched);
    on<OnUpdateStatusOrderOnWayEvent>(_onUpdateStatusOrderOnWay);
    on<OnUpdateStatusOrderDeliveredEvent>(_onUpdateStatusOrderDelivered);

    // Đăng ký các event handler mới
    on<LoadDriverOrdersEvent>(_onLoadDriverOrders);
    on<LoadOrderDetailsEvent>(_onLoadOrderDetails);
  }

  // Future<void> _onAddNewOrders(OnAddNewOrdersEvent event, Emitter<OrdersState> emit) async {
  //   try {
  //     emit(LoadingOrderState());

  //     await Future.delayed(Duration(milliseconds: 1500));

  //     final resp = await ordersServices.addNewOrders(event.uidAddress, event.total, event.typePayment, event.products);

  //     if(resp.resp) {
  //       // TODO: Implement push notification service
  //       // final listTokens = await userServices.getAdminsNotificationToken();
  //       // Map<String, dynamic> data = { 'click_action' : 'FLUTTER_NOTIFICATION_CLICK' };

  //      emit(SuccessOrdersState());

  //     } else {
  //       emit(FailureOrdersState(resp.msg));
  //     }

  //   } catch (e) {
  //     emit(FailureOrdersState(e.toString()));
  //   }
  // }

  Future<void> _onUpdateStatusOrderToDispatched(
      OnUpdateStatusOrderToDispatchedEvent event,
      Emitter<OrdersState> emit) async {
    try {
      emit(LoadingOrderState());

      // Convert to int for the new API
      int orderId = int.parse(event.idOrder);

      // Create status update model
      final statusUpdate = OrderStatusUpdate(
          statusId: 2, // Processing/Dispatched
          notes: 'Order dispatched to delivery ID: ${event.idDelivery}');

      // Use the updated API method
      final success = await ordersServices.updateOrderStatus(
          orderId: orderId, statusUpdate: statusUpdate);

      await Future.delayed(Duration(seconds: 1));

      if (success) {
        // TODO: Implement push notification service
        // Map<String, dynamic> data = { 'click_action' : 'FLUTTER_NOTIFICATION_CLICK' };
        // await pushNotification.sendNotification(
        //   event.notificationTokenDelivery,
        //   data,
        //   'Assigned order',
        //   'New order assigned'
        // );

        emit(SuccessOrdersState());
      } else {
        emit(FailureOrdersState('Failed to update order status'));
      }
    } catch (e) {
      emit(FailureOrdersState(e.toString()));
    }
  }

  Future<void> _onUpdateStatusOrderOnWay(
      OnUpdateStatusOrderOnWayEvent event, Emitter<OrdersState> emit) async {
    try {
      emit(LoadingOrderState());

      // Convert to int for the new API
      int orderId = int.parse(event.idOrder);

      // Create status update model
      final statusUpdate = OrderStatusUpdate(
          statusId: 3, // In Delivery
          notes:
              'Driver location: ${event.locationDelivery.latitude}, ${event.locationDelivery.longitude}');

      // Use the updated API method
      final success = await ordersServices.updateOrderStatus(
          orderId: orderId, statusUpdate: statusUpdate);

      await Future.delayed(Duration(seconds: 1));

      if (success) {
        emit(SuccessOrdersState());
      } else {
        emit(FailureOrdersState('Failed to update order status to on way'));
      }
    } catch (e) {
      emit(FailureOrdersState(e.toString()));
    }
  }

  Future<void> _onUpdateStatusOrderDelivered(
      OnUpdateStatusOrderDeliveredEvent event,
      Emitter<OrdersState> emit) async {
    try {
      emit(LoadingOrderState());

      // Convert to int for the new API
      int orderId = int.parse(event.idOrder);

      // Create status update model
      final statusUpdate = OrderStatusUpdate(
          statusId: 4, // Delivered
          notes: 'Order delivered successfully');

      // Use the updated API method
      final success = await ordersServices.updateOrderStatus(
          orderId: orderId, statusUpdate: statusUpdate);

      await Future.delayed(Duration(milliseconds: 450));

      if (success) {
        emit(SuccessOrdersState());
      } else {
        emit(FailureOrdersState('Failed to update order status to delivered'));
      }
    } catch (e) {
      emit(FailureOrdersState(e.toString()));
    }
  }

  // Handler mới cho việc tải danh sách đơn hàng của tài xế
  Future<void> _onLoadDriverOrders(
      LoadDriverOrdersEvent event, Emitter<OrdersState> emit) async {
    try {
      emit(LoadingOrderState());

      final orders = await ordersServices.getDriverOrdersList();
      emit(DriverOrdersLoadedState(orders));
    } catch (e) {
      emit(FailureOrdersState(
          'Không thể tải danh sách đơn hàng: ${e.toString()}'));
    }
  }

  // Handler mới cho việc tải chi tiết đơn hàng
  Future<void> _onLoadOrderDetails(
      LoadOrderDetailsEvent event, Emitter<OrdersState> emit) async {
    try {
      emit(LoadingOrderState());

      final orderDetails =
          await ordersServices.getDriverOrderDetail(event.orderId);
      if (orderDetails != null) {
        emit(OrderDetailsLoadedState(orderDetails));
      } else {
        emit(FailureOrdersState(
            'Không thể tải chi tiết đơn hàng: Không tìm thấy'));
      }
    } catch (e) {
      emit(FailureOrdersState(
          'Không thể tải chi tiết đơn hàng: ${e.toString()}'));
    }
  }
}
