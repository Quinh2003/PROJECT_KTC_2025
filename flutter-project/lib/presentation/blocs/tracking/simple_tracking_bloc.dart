import 'dart:async';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:geolocator/geolocator.dart';
import '../../../services/tracking_services.dart';
import '../../../domain/models/tracking/driver_location.dart';
import '../../helpers/tracking_helper.dart';

// Simple Tracking Events
abstract class SimpleTrackingEvent {}

class StartTrackingEvent extends SimpleTrackingEvent {
  final int deliveryId;
  final int? vehicleId; // Thêm vehicleId optional
  
  StartTrackingEvent(this.deliveryId, {this.vehicleId});
}

class StopTrackingEvent extends SimpleTrackingEvent {}

class SendTrackingUpdateEvent extends SimpleTrackingEvent {}

// Simple Tracking States  
abstract class SimpleTrackingState {}

class TrackingInactiveState extends SimpleTrackingState {}

class TrackingActiveState extends SimpleTrackingState {
  final int deliveryId;
  final DateTime lastUpdate;
  
  TrackingActiveState({
    required this.deliveryId,
    required this.lastUpdate,
  });
}

class TrackingErrorState extends SimpleTrackingState {
  final String error;
  TrackingErrorState(this.error);
}

/// Simple Tracking BLoC với 30 phút interval
/// Gửi location update mỗi 30 phút khi tracking active
class SimpleTrackingBloc extends Bloc<SimpleTrackingEvent, SimpleTrackingState> {
  final LocationService _locationService;
  
  Timer? _trackingTimer;
  int? _currentDeliveryId;
  
  SimpleTrackingBloc({
    LocationService? locationService,
  }) : _locationService = locationService ?? LocationService(),
       super(TrackingInactiveState()) {
    
    on<StartTrackingEvent>(_onStartTracking);
    on<StopTrackingEvent>(_onStopTracking);
    on<SendTrackingUpdateEvent>(_onSendTrackingUpdate);
  }

  /// Start tracking với 30 phút interval
  Future<void> _onStartTracking(
    StartTrackingEvent event,
    Emitter<SimpleTrackingState> emit,
  ) async {
    try {
      // Stop existing timer if any
      _trackingTimer?.cancel();
      
      _currentDeliveryId = event.deliveryId;
      
      // Đánh dấu delivery này là đang được track
      await TrackingHelper.setTrackingDelivery(event.deliveryId);
      
      // Lưu vehicleId nếu có
      if (event.vehicleId != null) {
        await _locationService.startDeliveryTracking(
          driverId: await _getDriverId() ?? 0,
          deliveryId: event.deliveryId,
          vehicleId: event.vehicleId!,
          statusId: 2, // In progress
        );
        print('💾 Saved vehicle ID ${event.vehicleId} for tracking');
      }
      
      // Send initial tracking update
      add(SendTrackingUpdateEvent());
      
      // Set up 30-minute timer
      _trackingTimer = Timer.periodic(
        const Duration(minutes: 30), 
        (_) => add(SendTrackingUpdateEvent()),
      );
      
      emit(TrackingActiveState(
        deliveryId: event.deliveryId,
        lastUpdate: DateTime.now(),
      ));
      
      print('🚀 Started tracking for delivery ${event.deliveryId} with 30-min interval');
      
    } catch (e) {
      emit(TrackingErrorState('Failed to start tracking: $e'));
    }
  }

  /// Get driver ID from secure storage
  Future<int?> _getDriverId() async {
    try {
      final driverIdStr = await _locationService.secureStorage.readDriverId();
      return driverIdStr != null ? int.parse(driverIdStr) : null;
    } catch (e) {
      print('Error getting driver ID: $e');
      return null;
    }
  }

  /// Stop tracking
  Future<void> _onStopTracking(
    StopTrackingEvent event,
    Emitter<SimpleTrackingState> emit,
  ) async {
    _trackingTimer?.cancel();
    _trackingTimer = null;
    
    // Clear tracking delivery từ SharedPreferences
    await TrackingHelper.clearTrackingDelivery();
    
    _currentDeliveryId = null;
    
    emit(TrackingInactiveState());
    print('⏹️ Stopped tracking');
  }

  /// Send tracking update
  Future<void> _onSendTrackingUpdate(
    SendTrackingUpdateEvent event,
    Emitter<SimpleTrackingState> emit,
  ) async {
    if (_currentDeliveryId == null) return;
    
    try {
      // Get current position
      final position = await Geolocator.getCurrentPosition(
        desiredAccuracy: LocationAccuracy.high,
      );
      
      // Create driver location
      final driverLocation = DriverLocation(
        latitude: position.latitude,
        longitude: position.longitude,
        timestamp: DateTime.now().toIso8601String(),
        speed: position.speed,
        heading: position.heading,
        vehicleStatus: 'active',
      );
      
      // Send to server
      final success = await _locationService.updateDeliveryTracking(
        _currentDeliveryId!,
        driverLocation,
        notes: 'Automated 30-minute location update',
        statusId: 2, // In progress
      );
      
      if (success) {
        emit(TrackingActiveState(
          deliveryId: _currentDeliveryId!,
          lastUpdate: DateTime.now(),
        ));
        print('📍 Tracking update sent successfully for delivery $_currentDeliveryId');
      } else {
        emit(TrackingErrorState('Failed to send tracking update'));
        print('❌ Failed to send tracking update for delivery $_currentDeliveryId');
      }
      
    } catch (e) {
      emit(TrackingErrorState('Location error: $e'));
      print('❌ Tracking update error: $e');
    }
  }

  @override
  Future<void> close() {
    _trackingTimer?.cancel();
    return super.close();
  }
}