part of 'orders_bloc.dart';

@immutable
abstract class OrdersEvent {}

class OnAddNewOrdersEvent extends OrdersEvent {
  final int uidAddress;
  final double total;
  final String typePayment;
  final List<ProductCart> products;

  OnAddNewOrdersEvent(this.uidAddress, this.total, this.typePayment, this.products);
} 

class OnUpdateStatusOrderToDispatchedEvent extends OrdersEvent {
  final String idOrder;
  final String idDelivery;
  final String notificationTokenDelivery;

  OnUpdateStatusOrderToDispatchedEvent(this.idOrder, this.idDelivery, this.notificationTokenDelivery);
}

class OnUpdateStatusOrderOnWayEvent extends OrdersEvent {
  final String idOrder;
  final LatLng locationDelivery;

  OnUpdateStatusOrderOnWayEvent(this.idOrder, this.locationDelivery);
}

class OnUpdateStatusOrderDeliveredEvent extends OrdersEvent {
  final String idOrder;

  OnUpdateStatusOrderDeliveredEvent(this.idOrder);
}

// Sự kiện mới để tải danh sách đơn hàng của tài xế
class LoadDriverOrdersEvent extends OrdersEvent {}

// Sự kiện mới để tải chi tiết đơn hàng
class LoadOrderDetailsEvent extends OrdersEvent {
  final int orderId;

  LoadOrderDetailsEvent(this.orderId);
}

// Sự kiện mới để tải thông tin đơn hàng cho một giao hàng
class LoadOrderForDeliveryEvent extends OrdersEvent {
  final int deliveryId;

  LoadOrderForDeliveryEvent(this.deliveryId);
}
