part of 'orders_bloc.dart';

@immutable
class OrdersState {}

class LoadingOrderState extends OrdersState {}

class SuccessOrdersState extends OrdersState {}

class FailureOrdersState extends OrdersState {
  final String error;

  FailureOrdersState(this.error);
}

// Trạng thái mới khi tải danh sách đơn hàng thành công
class DriverOrdersLoadedState extends OrdersState {
  final List<dynamic> orders;

  DriverOrdersLoadedState(this.orders);
}

// Trạng thái mới khi tải chi tiết đơn hàng thành công
class OrderDetailsLoadedState extends OrdersState {
  final dynamic orderDetails;

  OrderDetailsLoadedState(this.orderDetails);
}

// Trạng thái mới khi tải đơn hàng cho một giao hàng thành công
class OrderForDeliveryLoadedState extends OrdersState {
  final dynamic order;

  OrderForDeliveryLoadedState(this.order);
}
