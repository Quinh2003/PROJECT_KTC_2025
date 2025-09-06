part of 'mapdelivery_bloc.dart';

@immutable
class MapdeliveryState {

  final bool isReadyMapDelivery;

  final Map<String, Marker> markers;
  final Map<String, Polyline>? polyline;

  MapdeliveryState({
    this.isReadyMapDelivery = false,

    Map<String, Marker>? markers,
    Map<String, Polyline>? polyline
  }) : markers = markers ?? <String, Marker>{},
       polyline = polyline ?? <String, Polyline>{};

  MapdeliveryState copyWith({ bool? isReadyMapDelivery, Map<String, Marker>? markers, Map<String, Polyline>? polyline })
    => MapdeliveryState(
      isReadyMapDelivery: isReadyMapDelivery ?? this.isReadyMapDelivery,
      markers: markers ?? this.markers,
      polyline: polyline ?? this.polyline
    );



}



