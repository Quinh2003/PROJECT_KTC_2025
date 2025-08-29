import 'package:equatable/equatable.dart';
import '../../../domain/models/delivery/tracking_model.dart';

// States
abstract class TrackingState extends Equatable {
  @override
  List<Object?> get props => [];
}

class TrackingInitialState extends TrackingState {}

class TrackingActiveState extends TrackingState {
  final String driverId;
  final String? vehicleId;
  final String? routeId;
  final Duration interval;
  final String accuracy;
  final TrackingPoint? lastLocation;
  final DateTime startedAt;
  
  TrackingActiveState({
    required this.driverId,
    this.vehicleId,
    this.routeId,
    required this.interval,
    required this.accuracy,
    this.lastLocation,
    required this.startedAt,
  });
  
  @override
  List<Object?> get props => [driverId, vehicleId, routeId, interval, accuracy, lastLocation, startedAt];
  
  TrackingActiveState copyWith({
    String? driverId,
    String? vehicleId,
    String? routeId,
    Duration? interval,
    String? accuracy,
    TrackingPoint? lastLocation,
    DateTime? startedAt,
  }) {
    return TrackingActiveState(
      driverId: driverId ?? this.driverId,
      vehicleId: vehicleId ?? this.vehicleId,
      routeId: routeId ?? this.routeId,
      interval: interval ?? this.interval,
      accuracy: accuracy ?? this.accuracy,
      lastLocation: lastLocation ?? this.lastLocation,
      startedAt: startedAt ?? this.startedAt,
    );
  }
}

class TrackingInactiveState extends TrackingState {}

class TrackingErrorState extends TrackingState {
  final String error;
  
  TrackingErrorState({required this.error});
  
  @override
  List<Object?> get props => [error];
}

class TrackingHistoryLoadingState extends TrackingState {}

class TrackingHistoryLoadedState extends TrackingState {
  final List<TrackingPoint> trackingPoints;
  final DateTime startDate;
  final DateTime endDate;
  
  TrackingHistoryLoadedState({
    required this.trackingPoints,
    required this.startDate,
    required this.endDate,
  });
  
  @override
  List<Object?> get props => [trackingPoints, startDate, endDate];
}

class RouteTrackingActiveState extends TrackingState {
  final String routeId;
  final String driverId;
  final String? vehicleId;
  final List<TrackingPoint> trackingPoints;
  final DateTime startedAt;
  
  RouteTrackingActiveState({
    required this.routeId,
    required this.driverId,
    this.vehicleId,
    required this.trackingPoints,
    required this.startedAt,
  });
  
  @override
  List<Object?> get props => [routeId, driverId, vehicleId, trackingPoints, startedAt];
  
  RouteTrackingActiveState copyWith({
    String? routeId,
    String? driverId,
    String? vehicleId,
    List<TrackingPoint>? trackingPoints,
    DateTime? startedAt,
  }) {
    return RouteTrackingActiveState(
      routeId: routeId ?? this.routeId,
      driverId: driverId ?? this.driverId,
      vehicleId: vehicleId ?? this.vehicleId,
      trackingPoints: trackingPoints ?? this.trackingPoints,
      startedAt: startedAt ?? this.startedAt,
    );
  }
}

class DeliveryTrackingActiveState extends TrackingState {
  final String deliveryId;
  final String routeId;
  final List<TrackingPoint> trackingPoints;
  final List<TrackingEvent> events;
  final DateTime startedAt;
  
  DeliveryTrackingActiveState({
    required this.deliveryId,
    required this.routeId,
    required this.trackingPoints,
    required this.events,
    required this.startedAt,
  });
  
  @override
  List<Object?> get props => [deliveryId, routeId, trackingPoints, events, startedAt];
  
  DeliveryTrackingActiveState copyWith({
    String? deliveryId,
    String? routeId,
    List<TrackingPoint>? trackingPoints,
    List<TrackingEvent>? events,
    DateTime? startedAt,
  }) {
    return DeliveryTrackingActiveState(
      deliveryId: deliveryId ?? this.deliveryId,
      routeId: routeId ?? this.routeId,
      trackingPoints: trackingPoints ?? this.trackingPoints,
      events: events ?? this.events,
      startedAt: startedAt ?? this.startedAt,
    );
  }
}


