part of 'driver_bloc.dart';

@immutable
abstract class DriverState {}

/// Trạng thái khởi tạo của DriverBloc
class DriverInitial extends DriverState {}

/// Trạng thái đang tải dữ liệu
class DriverLoading extends DriverState {}

/// Trạng thái lỗi
class DriverError extends DriverState {
  final String message;

  DriverError(this.message);
}

/// Trạng thái sau khi tải thông tin cá nhân tài xế thành công
class DriverProfileLoadedState extends DriverState {
  final DriverProfile profile;

  DriverProfileLoadedState(this.profile);
}

/// Trạng thái sau khi tải thông tin phân tích tài xế thành công
class DriverAnalyticsLoadedState extends DriverState {
  final DriverAnalytics analytics;

  DriverAnalyticsLoadedState(this.analytics);
}

/// Trạng thái sau khi cập nhật trạng thái tài xế thành công
class DriverStatusUpdatedState extends DriverState {
  final DriverProfile profile;
  final String message;

  DriverStatusUpdatedState(this.profile, this.message);
}

/// Trạng thái sau khi tải lên tài liệu thành công
class DocumentUploadedState extends DriverState {
  final String documentUrl;

  DocumentUploadedState(this.documentUrl);
}
