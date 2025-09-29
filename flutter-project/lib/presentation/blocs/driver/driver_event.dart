part of 'driver_bloc.dart';

@immutable
abstract class DriverEvent {}

/// Event để tải thông tin cá nhân của tài xế
class LoadDriverProfileEvent extends DriverEvent {}

/// Event để tải thông tin phân tích của tài xế
class LoadDriverAnalyticsEvent extends DriverEvent {
  final String? startDate;
  final String? endDate;

  LoadDriverAnalyticsEvent({this.startDate, this.endDate});
}

/// Event để cập nhật thông tin cá nhân của tài xế
class UpdateDriverProfileEvent extends DriverEvent {
  final DriverProfile profile;

  UpdateDriverProfileEvent(this.profile);
}

/// Event để cập nhật trạng thái của tài xế
class UpdateDriverStatusEvent extends DriverEvent {
  final int statusId;

  UpdateDriverStatusEvent(this.statusId);
}

/// Event để tải lên tài liệu của tài xế
class UploadDriverDocumentEvent extends DriverEvent {
  final String filePath;
  final String documentType;

  UploadDriverDocumentEvent(this.filePath, this.documentType);
}
