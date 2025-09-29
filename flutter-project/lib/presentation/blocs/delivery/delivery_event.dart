part of 'delivery_bloc.dart';

@immutable
abstract class DeliveryEvent {}

/// Event để tải danh sách giao hàng của tài xế
class LoadDeliveriesEvent extends DeliveryEvent {
  final String? status;
  final String? sortBy;
  final String? sortDirection;

  LoadDeliveriesEvent({this.status, this.sortBy, this.sortDirection});
}

/// Event để tải danh sách giao hàng đang hoạt động
class LoadActiveDeliveriesEvent extends DeliveryEvent {}

/// Event để tải chi tiết giao hàng
class LoadDeliveryDetailsEvent extends DeliveryEvent {
  final int deliveryId;

  LoadDeliveryDetailsEvent(this.deliveryId);
}

/// Event để cập nhật trạng thái giao hàng
class UpdateDeliveryStatusEvent extends DeliveryEvent {
  final int deliveryId;
  final DeliveryStatusUpdate statusUpdate;

  UpdateDeliveryStatusEvent(this.deliveryId, this.statusUpdate);
}
