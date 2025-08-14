import 'package:equatable/equatable.dart';
import '../../../domain/models/tracking_model.dart';

// Events
abstract class TrackingEvent extends Equatable {
  @override
  List<Object?> get props => [];
}

class StartTrackingEvent extends TrackingEvent {
  final String driverId;
  final String? vehicleId;
  final String? routeId;
  
  StartTrackingEvent({
    required this.driverId, 
    this.vehicleId,
    this.routeId,
  });
  
  @override
  List<Object?> get props => [driverId, vehicleId, routeId];
}

class StopTrackingEvent extends TrackingEvent {
  final String driverId;
  
  StopTrackingEvent({required this.driverId});
  
  @override
  List<Object?> get props => [driverId];
}

class LocationUpdatedEvent extends TrackingEvent {
  final TrackingPoint trackingPoint;
  
  LocationUpdatedEvent({required this.trackingPoint});
  
  @override
  List<Object?> get props => [trackingPoint];
}

class UpdateTrackingIntervalEvent extends TrackingEvent {
  final Duration interval;
  
  UpdateTrackingIntervalEvent({required this.interval});
  
  @override
  List<Object?> get props => [interval];
}

class UpdateTrackingAccuracyEvent extends TrackingEvent {
  final String accuracy; // 'high', 'medium', 'low', etc.
  
  UpdateTrackingAccuracyEvent({required this.accuracy});
  
  @override
  List<Object?> get props => [accuracy];
}

class StartRouteTrackingEvent extends TrackingEvent {
  final String routeId;
  final String driverId;
  final String? vehicleId;
  
  StartRouteTrackingEvent({
    required this.routeId,
    required this.driverId,
    this.vehicleId,
  });
  
  @override
  List<Object?> get props => [routeId, driverId, vehicleId];
}

class EndRouteTrackingEvent extends TrackingEvent {
  final String routeId;
  
  EndRouteTrackingEvent({required this.routeId});
  
  @override
  List<Object?> get props => [routeId];
}

class StartDeliveryTrackingEvent extends TrackingEvent {
  final String deliveryId;
  final String routeId;
  
  StartDeliveryTrackingEvent({
    required this.deliveryId,
    required this.routeId,
  });
  
  @override
  List<Object?> get props => [deliveryId, routeId];
}

class EndDeliveryTrackingEvent extends TrackingEvent {
  final String deliveryId;
  final String status;
  final Map<String, dynamic>? metadata;
  
  EndDeliveryTrackingEvent({
    required this.deliveryId,
    required this.status,
    this.metadata,
  });
  
  @override
  List<Object?> get props => [deliveryId, status, metadata];
}

class LogTrackingEventEvent extends TrackingEvent {
  final String eventType;
  final String description;
  final Map<String, dynamic> metadata;
  
  LogTrackingEventEvent({
    required this.eventType,
    required this.description,
    required this.metadata,
  });
  
  @override
  List<Object?> get props => [eventType, description, metadata];
}

class FetchTrackingHistoryEvent extends TrackingEvent {
  final String driverId;
  final DateTime startDate;
  final DateTime endDate;
  
  FetchTrackingHistoryEvent({
    required this.driverId,
    required this.startDate,
    required this.endDate,
  });
  
  @override
  List<Object?> get props => [driverId, startDate, endDate];
}


