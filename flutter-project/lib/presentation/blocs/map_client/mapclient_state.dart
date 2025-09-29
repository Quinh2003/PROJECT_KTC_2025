part of 'mapclient_bloc.dart';

@immutable
class MapclientState {

  final bool isReadyMapClient;

  final Map<String, Marker> markerClient;

  MapclientState({
    this.isReadyMapClient = false,

    Map<String, Marker>? markerClient
  }) : markerClient = markerClient ?? <String, Marker>{};


  MapclientState copyWith({ bool? isReadyMapClient, Map<String, Marker>? markerClient  })
    => MapclientState(
        isReadyMapClient: isReadyMapClient ?? this.isReadyMapClient,
        markerClient: markerClient ?? this.markerClient 
      );


}


