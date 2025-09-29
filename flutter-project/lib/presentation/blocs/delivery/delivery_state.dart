part of 'delivery_bloc.dart';

@immutable
abstract class DeliveryState {}

/// Trạng thái khởi tạo của DeliveryBloc
class DeliveryInitial extends DeliveryState {}

/// Trạng thái đang tải dữ liệu
class DeliveryLoading extends DeliveryState {}

/// Trạng thái lỗi
class DeliveryError extends DeliveryState {
  final String message;

  DeliveryError(this.message);
}

/// Trạng thái sau khi tải danh sách giao hàng thành công
class DeliveriesLoadedState extends DeliveryState {
  final List<Delivery> deliveries;

  DeliveriesLoadedState(this.deliveries);
}

/// Trạng thái khi không có giao hàng nào
class DeliveriesEmptyState extends DeliveryState {}

/// Trạng thái sau khi tải danh sách giao hàng đang hoạt động thành công
class ActiveDeliveriesLoadedState extends DeliveryState {
  final List<Delivery> deliveries;

  ActiveDeliveriesLoadedState(this.deliveries);
}

/// Trạng thái sau khi tải chi tiết giao hàng thành công
class DeliveryDetailsLoadedState extends DeliveryState {
  final DeliveryDetailResponse delivery;

  DeliveryDetailsLoadedState(this.delivery);
}

/// Trạng thái sau khi cập nhật trạng thái giao hàng thành công
class DeliveryStatusUpdatedState extends DeliveryState {
  final Delivery delivery;

  DeliveryStatusUpdatedState(this.delivery);
}
