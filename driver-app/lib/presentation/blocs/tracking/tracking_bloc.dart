import 'dart:async';
import 'package:bloc/bloc.dart';
import 'package:geolocator/geolocator.dart';
import '../../../domain/models/delivery/tracking_model.dart' hide TrackingEvent;
import '../../../domain/usecases/usecases.dart';
import 'tracking_event.dart';
import 'tracking_state.dart';

class TrackingBloc extends Bloc<TrackingEvent, TrackingState> {
  final StartRouteTrackingUseCase _startRouteTrackingUseCase;
  final EndRouteTrackingUseCase _endRouteTrackingUseCase;
  final UpdateLocationUseCase _updateLocationUseCase;
  final GetTrackingHistoryUseCase _getTrackingHistoryUseCase;
  
  StreamSubscription<Position>? _positionStreamSubscription;
  
  TrackingBloc({
    required StartRouteTrackingUseCase startRouteTrackingUseCase,
    required EndRouteTrackingUseCase endRouteTrackingUseCase,
    required UpdateLocationUseCase updateLocationUseCase,
    required GetTrackingHistoryUseCase getTrackingHistoryUseCase,
  }) : _startRouteTrackingUseCase = startRouteTrackingUseCase,
       _endRouteTrackingUseCase = endRouteTrackingUseCase,
       _updateLocationUseCase = updateLocationUseCase,
       _getTrackingHistoryUseCase = getTrackingHistoryUseCase,
       super(TrackingInitialState()) {
    
    on<StartTrackingEvent>(_onStartTracking);
    on<StopTrackingEvent>(_onStopTracking);
    on<LocationUpdatedEvent>(_onLocationUpdated);
    on<UpdateTrackingIntervalEvent>(_onUpdateTrackingInterval);
    on<UpdateTrackingAccuracyEvent>(_onUpdateTrackingAccuracy);
    on<StartRouteTrackingEvent>(_onStartRouteTracking);
    on<EndRouteTrackingEvent>(_onEndRouteTracking);
    on<StartDeliveryTrackingEvent>(_onStartDeliveryTracking);
    on<EndDeliveryTrackingEvent>(_onEndDeliveryTracking);
    on<LogTrackingEventEvent>(_onLogTrackingEvent);
    on<FetchTrackingHistoryEvent>(_onFetchTrackingHistory);
  }
  
  Future<void> _onStartTracking(
    StartTrackingEvent event, 
    Emitter<TrackingState> emit
  ) async {
    try {
      // Request location permissions
      bool serviceEnabled = await Geolocator.isLocationServiceEnabled();
      if (!serviceEnabled) {
        emit(TrackingErrorState(error: 'Location services are disabled.'));
        return;
      }
      
      LocationPermission permission = await Geolocator.checkPermission();
      if (permission == LocationPermission.denied) {
        permission = await Geolocator.requestPermission();
        if (permission == LocationPermission.denied) {
          emit(TrackingErrorState(error: 'Location permissions are denied'));
          return;
        }
      }
      
      if (permission == LocationPermission.deniedForever) {
        emit(TrackingErrorState(
          error: 'Location permissions are permanently denied. Please enable in settings.'
        ));
        return;
      }
      
      // Start location tracking (no need for use case here, just start GPS)
      emit(TrackingActiveState(
        driverId: event.driverId,
        vehicleId: event.vehicleId,
        routeId: event.routeId,
        interval: const Duration(seconds: 10),
        accuracy: 'high',
        startedAt: DateTime.now(),
      ));
      
      // Start location tracking
      _positionStreamSubscription?.cancel();
      
      final LocationSettings locationSettings = LocationSettings(
        accuracy: LocationAccuracy.high,
        distanceFilter: 10,
      );
      
      _positionStreamSubscription = Geolocator.getPositionStream(
        locationSettings: locationSettings
      ).listen((Position position) {
        final trackingPoint = TrackingPoint(
          latitude: position.latitude,
          longitude: position.longitude,
          driverId: event.driverId,
          vehicleId: event.vehicleId,
          timestamp: DateTime.now(),
          speed: position.speed,
          heading: position.heading,
          accuracy: position.accuracy,
          altitude: position.altitude,
          status: 'active',
        );
        
        add(LocationUpdatedEvent(trackingPoint: trackingPoint));
      });
      
      emit(TrackingActiveState(
        driverId: event.driverId,
        vehicleId: event.vehicleId,
        routeId: event.routeId,
        interval: const Duration(seconds: 10),
        accuracy: 'high',
        startedAt: DateTime.now(),
      ));
    } catch (e) {
      emit(TrackingErrorState(error: 'Failed to start tracking: ${e.toString()}'));
    }
  }
  
  Future<void> _onStopTracking(
    StopTrackingEvent event, 
    Emitter<TrackingState> emit
  ) async {
    try {
      await _positionStreamSubscription?.cancel();
      _positionStreamSubscription = null;
      
      // Stop GPS tracking (no use case needed)
      emit(TrackingInactiveState());
    } catch (e) {
      emit(TrackingErrorState(error: 'Failed to stop tracking: ${e.toString()}'));
    }
  }
  
  Future<void> _onLocationUpdated(
    LocationUpdatedEvent event, 
    Emitter<TrackingState> emit
  ) async {
    // If we're in active state, update the current location
    if (state is TrackingActiveState) {
      final currentState = state as TrackingActiveState;
      
      // Send the location update to the server through use case
      try {
        // Update location using use case
        await _updateLocationUseCase.call(event.trackingPoint);
        
        emit(currentState.copyWith(
          lastLocation: event.trackingPoint,
        ));
      } catch (e) {
        emit(TrackingErrorState(error: 'Failed to update location: ${e.toString()}'));
      }
    }
  }
  
  Future<void> _onUpdateTrackingInterval(
    UpdateTrackingIntervalEvent event, 
    Emitter<TrackingState> emit
  ) async {
    if (state is TrackingActiveState) {
      final currentState = state as TrackingActiveState;
      
      // Cancel existing subscription
      await _positionStreamSubscription?.cancel();
      
      // Create new subscription with updated interval
      final LocationSettings locationSettings = LocationSettings(
        accuracy: LocationAccuracy.high,
        distanceFilter: 10,
      );
      
      _positionStreamSubscription = Geolocator.getPositionStream(
        locationSettings: locationSettings
      ).listen((Position position) {
        final trackingPoint = TrackingPoint(
          latitude: position.latitude,
          longitude: position.longitude,
          driverId: currentState.driverId,
          vehicleId: currentState.vehicleId,
          timestamp: DateTime.now(),
          speed: position.speed,
          heading: position.heading,
          accuracy: position.accuracy,
          altitude: position.altitude,
          status: 'active',
        );
        
        add(LocationUpdatedEvent(trackingPoint: trackingPoint));
      });
      
      emit(currentState.copyWith(
        interval: event.interval,
      ));
    }
  }
  
  Future<void> _onUpdateTrackingAccuracy(
    UpdateTrackingAccuracyEvent event, 
    Emitter<TrackingState> emit
  ) async {
    if (state is TrackingActiveState) {
      final currentState = state as TrackingActiveState;
      
      emit(currentState.copyWith(
        accuracy: event.accuracy,
      ));
    }
  }
  
  Future<void> _onStartRouteTracking(
    StartRouteTrackingEvent event, 
    Emitter<TrackingState> emit
  ) async {
    try {
      // First make sure we're tracking location
      if (state is! TrackingActiveState) {
        add(StartTrackingEvent(driverId: event.driverId, vehicleId: event.vehicleId));
      }
      
      // Start route tracking using use case
      final success = await _startRouteTrackingUseCase.call(
        StartRouteTrackingParams(
          routeId: event.routeId,
          driverId: event.driverId,
          vehicleId: event.vehicleId,
        ),
      );
      
      if (success) {
        emit(RouteTrackingActiveState(
          routeId: event.routeId,
          driverId: event.driverId,
          vehicleId: event.vehicleId,
          trackingPoints: [],
          startedAt: DateTime.now(),
        ));
      } else {
        emit(TrackingErrorState(error: 'Failed to start route tracking'));
      }
    } catch (e) {
      emit(TrackingErrorState(error: 'Failed to start route tracking: ${e.toString()}'));
    }
  }
  
  Future<void> _onEndRouteTracking(
    EndRouteTrackingEvent event, 
    Emitter<TrackingState> emit
  ) async {
    try {
      // End route tracking using use case
      final success = await _endRouteTrackingUseCase.call(
        EndRouteTrackingParams(routeId: event.routeId),
      );
      
      if (success) {
        // If we're in route tracking state, go back to regular tracking
        if (state is RouteTrackingActiveState) {
          final routeState = state as RouteTrackingActiveState;
          emit(TrackingActiveState(
            driverId: routeState.driverId,
            vehicleId: routeState.vehicleId,
            routeId: null,
            interval: const Duration(seconds: 10),
            accuracy: 'high',
            startedAt: routeState.startedAt,
          ));
        }
      } else {
        emit(TrackingErrorState(error: 'Failed to end route tracking'));
      }
    } catch (e) {
      emit(TrackingErrorState(error: 'Failed to end route tracking: ${e.toString()}'));
    }
  }
  
  Future<void> _onStartDeliveryTracking(
    StartDeliveryTrackingEvent event, 
    Emitter<TrackingState> emit
  ) async {
    try {
      emit(DeliveryTrackingActiveState(
        deliveryId: event.deliveryId,
        routeId: event.routeId,
        trackingPoints: [],
        events: [],
        startedAt: DateTime.now(),
      ));
    } catch (e) {
      emit(TrackingErrorState(error: 'Failed to start delivery tracking: ${e.toString()}'));
    }
  }
  
  Future<void> _onEndDeliveryTracking(
    EndDeliveryTrackingEvent event, 
    Emitter<TrackingState> emit
  ) async {
    try {
      // If we're in delivery tracking state, go back to route tracking
      if (state is DeliveryTrackingActiveState) {
        final deliveryState = state as DeliveryTrackingActiveState;
        emit(RouteTrackingActiveState(
          routeId: deliveryState.routeId,
          driverId: 'current_driver', // This should come from auth state
          vehicleId: null,
          trackingPoints: deliveryState.trackingPoints,
          startedAt: deliveryState.startedAt,
        ));
      }
    } catch (e) {
      emit(TrackingErrorState(error: 'Failed to end delivery tracking: ${e.toString()}'));
    }
  }
  
  Future<void> _onLogTrackingEvent(
    LogTrackingEventEvent event, 
    Emitter<TrackingState> emit
  ) async {
    try {
      // Just log the event, don't change state for now
      print('Tracking Event: ${event.eventType} - ${event.description}');
    } catch (e) {
      // Just log the error, don't change state
      print('Failed to log tracking event: ${e.toString()}');
    }
  }
  
  Future<void> _onFetchTrackingHistory(
    FetchTrackingHistoryEvent event, 
    Emitter<TrackingState> emit
  ) async {
    try {
      emit(TrackingHistoryLoadingState());
      
      // Fetch tracking history using use case
      final response = await _getTrackingHistoryUseCase.call(
        TrackingHistoryParams(
          driverId: event.driverId,
          startDate: event.startDate.toIso8601String(),
          endDate: event.endDate.toIso8601String(),
        ),
      );
      
      emit(TrackingHistoryLoadedState(
        trackingPoints: response.trackingPoints,
        startDate: event.startDate,
        endDate: event.endDate,
      ));
    } catch (e) {
      emit(TrackingErrorState(error: 'Failed to fetch tracking history: ${e.toString()}'));
    }
  }
  
  @override
  Future<void> close() {
    _positionStreamSubscription?.cancel();
    return super.close();
  }
}


