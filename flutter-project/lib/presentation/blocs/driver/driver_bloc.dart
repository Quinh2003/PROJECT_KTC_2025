import 'dart:async';

import 'package:bloc/bloc.dart';
import 'package:meta/meta.dart';

import '../../../domain/models/driver/driver_profile.dart';
import '../../../domain/models/analytics/driver_analytics.dart';
import '../../../services/driver_services.dart';

part 'driver_event.dart';
part 'driver_state.dart';

/// Bloc để quản lý các tương tác liên quan đến tài xế
/// Tập trung vào thông tin cá nhân, hồ sơ và trạng thái của tài xế
class DriverBloc extends Bloc<DriverEvent, DriverState> {
  final DriverServices _driverServices;

  DriverBloc({required DriverServices driverServices})
      : _driverServices = driverServices,
        super(DriverInitial()) {
    on<LoadDriverProfileEvent>(_onLoadDriverProfile);
    on<LoadDriverAnalyticsEvent>(_onLoadDriverAnalytics);
    on<UpdateDriverProfileEvent>(_onUpdateDriverProfile);
    on<UpdateDriverStatusEvent>(_onUpdateDriverStatus);
    on<UploadDriverDocumentEvent>(_onUploadDriverDocument);
  }

  Future<void> _onLoadDriverProfile(
      LoadDriverProfileEvent event, Emitter<DriverState> emit) async {
    emit(DriverLoading());
    try {
      final profile = await _driverServices.getDriverProfile();
      emit(DriverProfileLoadedState(profile));
    } catch (e) {
      emit(DriverError('Không thể tải thông tin tài xế: ${e.toString()}'));
    }
  }

  Future<void> _onLoadDriverAnalytics(
      LoadDriverAnalyticsEvent event, Emitter<DriverState> emit) async {
    emit(DriverLoading());
    try {
      final analytics = await _driverServices.getDriverAnalytics(
        startDate: event.startDate,
        endDate: event.endDate,
      );
      emit(DriverAnalyticsLoadedState(analytics));
    } catch (e) {
      emit(DriverError('Không thể tải thông tin phân tích: ${e.toString()}'));
    }
  }

  Future<void> _onUpdateDriverProfile(
      UpdateDriverProfileEvent event, Emitter<DriverState> emit) async {
    emit(DriverLoading());
    try {
      final updatedProfile =
          await _driverServices.updateDriverProfile(event.profile);
      emit(DriverProfileLoadedState(updatedProfile));
    } catch (e) {
      emit(DriverError('Không thể cập nhật thông tin tài xế: ${e.toString()}'));
    }
  }

  Future<void> _onUpdateDriverStatus(
      UpdateDriverStatusEvent event, Emitter<DriverState> emit) async {
    emit(DriverLoading());
    try {
      final result = await _driverServices.updateDriverStatus(event.statusId);
      if (result['success'] == true) {
        // Tải lại profile để cập nhật trạng thái mới
        final profile = await _driverServices.getDriverProfile();
        emit(DriverStatusUpdatedState(
            profile, result['message'] ?? 'Cập nhật trạng thái thành công'));
      } else {
        emit(DriverError(result['message'] ?? 'Không thể cập nhật trạng thái'));
      }
    } catch (e) {
      emit(
          DriverError('Không thể cập nhật trạng thái tài xế: ${e.toString()}'));
    }
  }

  Future<void> _onUploadDriverDocument(
      UploadDriverDocumentEvent event, Emitter<DriverState> emit) async {
    emit(DriverLoading());
    try {
      final documentUrl = await _driverServices.uploadDriverDocument(
        event.filePath,
        event.documentType,
      );
      emit(DocumentUploadedState(documentUrl));
    } catch (e) {
      emit(DriverError('Không thể tải lên tài liệu: ${e.toString()}'));
    }
  }
}
