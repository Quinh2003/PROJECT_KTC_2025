import 'dart:async';

import 'package:bloc/bloc.dart';
import 'package:meta/meta.dart';

import '../../../domain/models/delivery/delivery.dart';
import '../../../domain/models/delivery/delivery_detail_response.dart';
import '../../../domain/models/delivery/delivery_status_update.dart';
import '../../../services/delivery_services.dart';

part 'delivery_event.dart';
part 'delivery_state.dart';

/// Bloc để quản lý các tương tác liên quan đến giao hàng của tài xế
/// Tập trung vào các chức năng liên quan đến giao hàng mà tài xế thực hiện
class DeliveryBloc extends Bloc<DeliveryEvent, DeliveryState> {
  final DeliveryServices _deliveryServices;

  DeliveryBloc({required DeliveryServices deliveryServices})
      : _deliveryServices = deliveryServices,
        super(DeliveryInitial()) {
    on<LoadDeliveriesEvent>(_onLoadDeliveries);
    // on<LoadActiveDeliveriesEvent>(_onLoadActiveDeliveries);
    on<LoadDeliveryDetailsEvent>(_onLoadDeliveryDetails);
    on<UpdateDeliveryStatusEvent>(_onUpdateDeliveryStatus);
  }

  Future<void> _onLoadDeliveries(
      LoadDeliveriesEvent event, Emitter<DeliveryState> emit) async {
    emit(DeliveryLoading());
    try {
      final deliveries = await _deliveryServices.getDriverDeliveries(
        status: event.status,
        sortBy: event.sortBy,
        sortDirection: event.sortDirection,
      );
      
      if (deliveries.isEmpty) {
        // Send a special state for empty deliveries to distinguish from errors
        emit(DeliveriesEmptyState());
      } else {
        emit(DeliveriesLoadedState(deliveries));
      }
    } catch (e) {
      emit(DeliveryError('Không thể tải danh sách giao hàng: ${e.toString()}'));
    }
  }

  // Future<void> _onLoadActiveDeliveries(
  //     LoadActiveDeliveriesEvent event, Emitter<DeliveryState> emit) async {
  //   emit(DeliveryLoading());
  //   try {
  //     final deliveries = await _deliveryServices.getActiveDeliveries();
  //     emit(ActiveDeliveriesLoadedState(deliveries));
  //   } catch (e) {
  //     emit(DeliveryError('Không thể tải danh sách giao hàng đang hoạt động: ${e.toString()}'));
  //   }
  // }

  Future<void> _onLoadDeliveryDetails(
      LoadDeliveryDetailsEvent event, Emitter<DeliveryState> emit) async {
    emit(DeliveryLoading());
    try {
      final delivery = await _deliveryServices.getDeliveryDetail(event.deliveryId);
      if (delivery != null) {
        emit(DeliveryDetailsLoadedState(delivery));
      } else {
        emit(DeliveryError('Không thể tải chi tiết giao hàng: Không tìm thấy'));
      }
    } catch (e) {
      emit(DeliveryError('Không thể tải chi tiết giao hàng: ${e.toString()}'));
    }
  }

  Future<void> _onUpdateDeliveryStatus(
      UpdateDeliveryStatusEvent event, Emitter<DeliveryState> emit) async {
    emit(DeliveryLoading());
    try {
      final updatedDelivery = await _deliveryServices.updateDeliveryStatus(
        event.deliveryId,
        event.statusUpdate,
      );
      if (updatedDelivery != null) {
        emit(DeliveryStatusUpdatedState(updatedDelivery));
      } else {
        emit(DeliveryError('Không thể cập nhật trạng thái: Cập nhật thất bại'));
      }
    } catch (e) {
      emit(DeliveryError('Không thể cập nhật trạng thái: ${e.toString()}'));
    }
  }
}
