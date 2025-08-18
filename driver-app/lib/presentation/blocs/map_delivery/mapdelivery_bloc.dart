import 'dart:async';
import 'dart:convert';
import 'package:flutter/material.dart';
import 'package:bloc/bloc.dart';
import 'package:flutter_polyline_points/flutter_polyline_points.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:google_maps_flutter/google_maps_flutter.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/services/map_box_services.dart';
import 'package:ktc_logistics_driver/presentation/helpers/custom_markert.dart';
import 'package:ktc_logistics_driver/presentation/themes/theme_maps.dart';
import 'package:socket_io_client/socket_io_client.dart' as IO;

part 'mapdelivery_event.dart';
part 'mapdelivery_state.dart';

class MapdeliveryBloc extends Bloc<MapdeliveryEvent, MapdeliveryState> {

  MapdeliveryBloc() : super(MapdeliveryState()){

    on<OnMapReadyEvent>( _onMapReady );
    on<OnMarkertsDeliveryEvent>( _onMarkertDelivery );
    on<OnEmitLocationDeliveryEvent>( _onEmitLocationDelivery );

  }

  late GoogleMapController _mapController;
  late IO.Socket _socket;
  final mapBoxServices = MapBoxServices();

  Polyline _myRouteDestinationDelivery = Polyline(
    polylineId: PolylineId('myRouteDestinationDelivery'),
    color: Colors.black87,
    width: 5
  );


  void initMapDeliveryFrave( GoogleMapController controller ){

    if( !state.isReadyMapDelivery ){

      this._mapController = controller;
      
      _mapController.setMapStyle( jsonEncode( themeMapsFrave ));

      add( OnMapReadyEvent() );

    }
  }


  void moveCamareLocation( LatLng location ){
    final cameraUpdate = CameraUpdate.newLatLng(location);
    _mapController.animateCamera(cameraUpdate);
  }


  void initSocketDelivery() {
    final _env = Environment.getInstance();

    this._socket = IO.io('${_env.endpointBase}orders-delivery-socket' , {
      'transports': ['websocket'], 
      'autoConnect': true,
    });

    this._socket.connect();

  }
  

  void disconectSocket(){

    this._socket.disconnect();

  }



  Future<void> _onMapReady( OnMapReadyEvent event, Emitter<MapdeliveryState> emit ) async {

    emit( state.copyWith( isReadyMapDelivery: true ) );

  }

  Future<void> _onMarkertDelivery( OnMarkertsDeliveryEvent event, Emitter<MapdeliveryState> emit ) async {

    // Polylines 

    final mapBoxResponse = await mapBoxServices.getCoordsOriginAndDestinationDelivery(event.location, event.destination);

    final geometry = mapBoxResponse.routes[0].geometry;

    final points = PolylinePoints.decodePolyline(geometry.toString());
    final List<LatLng> routeCoords = points.map((point) => LatLng(point.latitude, point.longitude)).toList();

    _myRouteDestinationDelivery = this._myRouteDestinationDelivery.copyWith( pointsParam: routeCoords );

    final currentPoylines = state.polyline;
    currentPoylines!['myRouteDestinationDelivery'] = this._myRouteDestinationDelivery;

    // ------------------------ Markets

    final marketCustom = await getAssetImageMarker('assets/food-delivery-marker.png');
    final iconDestination = await getAssetImageMarker('assets/delivery-destination.png');

    final markerDelivery = Marker(
      markerId: MarkerId('markerDelivery'),
      position: event.location,
      icon: marketCustom
    );

    final markerDestination = Marker(
      markerId: MarkerId('markerDestination'),
      position: event.destination,
      icon: iconDestination
    );

    final newMarker = { ...state.markers };
    newMarker['markerDelivery'] = markerDelivery;
    newMarker['markerDestination'] = markerDestination;

    emit( state.copyWith(
      polyline: currentPoylines, 
      markers: newMarker 
    ));
  }

  Future<void> _onEmitLocationDelivery( OnEmitLocationDeliveryEvent event, Emitter<MapdeliveryState> emit ) async {

    this._socket.emit('position', { 
          'idOrder': event.idOrder, 
          'latitude': event.location.latitude, 
          'longitude' : event.location.longitude 
        });

  }

}



