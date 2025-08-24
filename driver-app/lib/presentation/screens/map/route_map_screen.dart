import 'dart:async';
import 'dart:convert';
import 'dart:math' as math;
import 'package:flutter/material.dart';
import 'package:location/location.dart';
import 'package:mapbox_maps_flutter/mapbox_maps_flutter.dart';
import '../../components/mapbox_map.dart';
import '../../components/route_instructions_card.dart';
import '../../design/spatial_ui.dart';
import '../../helpers/url_lancher_frave.dart';
import '../../../domain/models/map/mapbox_route_models.dart';
import '../../../services/map_box_services.dart';

class RouteMapScreen extends StatefulWidget {
  final String routeId;

  const RouteMapScreen({Key? key, required this.routeId}) : super(key: key);

  @override
  _RouteMapScreenState createState() => _RouteMapScreenState();
}

class _RouteMapScreenState extends State<RouteMapScreen> {
  // Services
  final LocationService _locationService = LocationService();

  // State variables
  RouteApiResponse? _routeData;
  bool _isMapLoading = true;
  bool _isRouteDataLoading = true;
  String? _errorMessage;
  int _currentRoutePointIndex = 0;
  bool _isNavigating = false;
  bool _isFollowingUser = true;
  Timer? _locationUpdateTimer;

  // Map components
  MapboxMap? _mapboxMap;
  PointAnnotationManager? _pointAnnotationManager;
  PolylineAnnotationManager? _polylineAnnotationManager;

  // Mapbox Access Token
  static const String _mapboxAccessToken = 
      "pk.eyJ1IjoiaHVuZ3BxMyIsImEiOiJjbHR3M3JzdXQwYzE5MnFteDFjYXRlcDEzIn0.GDrXTFKq1wn-FZSiTGrfew";

  @override
  void initState() {
    super.initState();
    
    // Initialize Mapbox with token
    try {
      // Set the Mapbox access token programmatically
      MapboxOptions.setAccessToken(_mapboxAccessToken);
      MapboxDirectionsService.setAccessToken(_mapboxAccessToken);
      print("Mapbox token set: $_mapboxAccessToken");
    } catch (e) {
      print("Error initializing Mapbox: $e");
      setState(() {
        _errorMessage = "Failed to initialize map. Please check your connection.";
      });
    }
    
    // Initialize location service
    _initializeLocation();
    
    // Load route data
    _loadRouteData();
    
    // Simulate driver movement during navigation
    _locationUpdateTimer = Timer.periodic(const Duration(seconds: 5), (timer) {
      if (_isNavigating) {
        _simulateDriverMovement();
      }
    });
  }

  @override
  void dispose() {
    _locationUpdateTimer?.cancel();
    super.dispose();
  }

  // Initialize location service
  Future<void> _initializeLocation() async {
    final hasPermission = await _locationService.initialize();
    if (hasPermission) {
      _locationService.startLocationUpdates((locationData) {
        setState(() {
          // Atualizar estado se necessário
        });
        _updateUserLocation(locationData);
      });
    } else {
      // Show permission dialog if needed
      if (mounted) {
        LocationService.showPermissionDialog(context);
      }
    }
  }

  // Load route data from API
  Future<void> _loadRouteData() async {
    setState(() {
      _isRouteDataLoading = true;
      _errorMessage = null;
    });
    
    try {
      _routeData = await RouteApiResponse.getRouteData(widget.routeId);
      if (mounted) {
        setState(() {
          _isRouteDataLoading = false;
        });
      }
    } catch (e) {
      print("Error loading route data: $e");
      if (mounted) {
        setState(() {
          _isRouteDataLoading = false;
          _errorMessage = "Failed to load route data. Please try again.";
        });
        
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text("Failed to load route data"),
            backgroundColor: Colors.red,
          ),
        );
      }
    }
  }

  // Map created callback
  Future<void> _onMapCreated(MapboxMap mapboxMap) async {
    try {
      _mapboxMap = mapboxMap;
      
      // Configure initial camera position
      await _mapboxMap!.setCamera(
        CameraOptions(
          center: Point(
            coordinates: Position(
              106.0, // Vietnam center longitude
              16.0,  // Vietnam center latitude
            ),
          ),
          zoom: 5.5, // Zoom out to see entire country
        ),
      );
      
      // Create annotation managers for markers and route
      _pointAnnotationManager = await _mapboxMap!.annotations.createPointAnnotationManager();
      _polylineAnnotationManager = await _mapboxMap!.annotations.createPolylineAnnotationManager();
      
      // Draw route if data is available
      if (_routeData != null) {
        await _drawRoute();
      }
      
      setState(() {
        _isMapLoading = false;
      });
    } catch (e) {
      print("Error initializing map: $e");
      setState(() {
        _isMapLoading = false;
        _errorMessage = "Failed to initialize map. Please check your connection.";
      });
    }
  }

  // Update user location on map
  void _updateUserLocation(LocationData locationData) {
    if (_mapboxMap != null && _isFollowingUser) {
      _mapboxMap!.setCamera(
        CameraOptions(
          center: Point(
            coordinates: Position(
              locationData.longitude!,
              locationData.latitude!,
            ),
          ),
          zoom: 15.0,
        ),
      );
    }
  }

  // Draw route on map
  Future<void> _drawRoute() async {
    if (_mapboxMap == null || _routeData == null) return;
    
    try {
      // Clear existing annotations
      await _pointAnnotationManager?.deleteAll();
      await _polylineAnnotationManager?.deleteAll();
      
      // Add all markers
      await _addRouteMarkers();
      
      // Add route polyline
      await _addRoutePolyline();
      
      // Center map on route
      await _centerMapOnRoute();
    } catch (e) {
      print("Error drawing route: $e");
    }
  }

  // Add markers for driver, destination and transit points
  Future<void> _addRouteMarkers() async {
    if (_pointAnnotationManager == null || _routeData == null) return;
    
    try {
      // Add driver marker
      final driverPoint = _routeData!.driverLocation;
      final driverIcon = await _loadMarkerImage('assets/driver-marker.png');
      await _pointAnnotationManager!.create(PointAnnotationOptions(
        geometry: Point(
          coordinates: Position(driverPoint.longitude, driverPoint.latitude),
        ),
        iconSize: 1.0,
        iconImage: driverIcon,
      ));
      
      // Add transit point markers
      final transitIcon = await _loadMarkerImage('assets/delivery-marker.png');
      for (final point in _routeData!.transitPoints) {
        await _pointAnnotationManager!.create(PointAnnotationOptions(
          geometry: Point(
            coordinates: Position(point.longitude, point.latitude),
          ),
          iconSize: 0.8,
          iconImage: transitIcon,
          textField: point.name,
          textSize: 12.0,
          textOffset: [0.0, 1.5],
          textColor: Colors.black.value,
        ));
      }
      
      // Add destination marker
      final destinationPoint = _routeData!.destinationLocation;
      final destinationIcon = await _loadMarkerImage('assets/destination-marker.png');
      await _pointAnnotationManager!.create(PointAnnotationOptions(
        geometry: Point(
          coordinates: Position(destinationPoint.longitude, destinationPoint.latitude),
        ),
        iconSize: 1.0,
        iconImage: destinationIcon,
        textField: destinationPoint.name,
        textSize: 12.0,
        textOffset: [0.0, 1.5],
        textColor: Colors.black.value,
      ));
    } catch (e) {
      print("Error adding markers: $e");
    }
  }

  // Load marker image
  Future<String> _loadMarkerImage(String assetPath) async {
    final byteData = await DefaultAssetBundle.of(context).load(assetPath);
    final bytes = byteData.buffer.asUint8List();
    return 'data:image/png;base64,${base64Encode(bytes)}';
  }

  // Add route polyline
  Future<void> _addRoutePolyline() async {
    if (_polylineAnnotationManager == null || _routeData == null) return;
    
    try {
      // Decode polyline and create coordinates
      final List<Position> coordinates = _decodePolyline(_routeData!.routePolyline);
      
      // Create polyline
      await _polylineAnnotationManager!.create(PolylineAnnotationOptions(
        geometry: LineString(coordinates: coordinates),
        lineColor: SpatialDesignSystem.primaryColor.value,
        lineWidth: 5.0,
      ));
    } catch (e) {
      print("Error adding route polyline: $e");
    }
  }

  // Decode polyline from Mapbox
  List<Position> _decodePolyline(String encodedPolyline) {
    List<Position> polylineCoordinates = [];
    
    try {
      int index = 0;
      int lat = 0;
      int lng = 0;
      
      while (index < encodedPolyline.length) {
        int result = 1;
        int shift = 0;
        int b;
        
        // Decode latitude
        do {
          b = encodedPolyline.codeUnitAt(index++) - 63 - 1;
          result += b << shift;
          shift += 5;
        } while (b >= 0x1f);
        
        lat += (result & 1) != 0 ? ~(result >> 1) : (result >> 1);
        
        result = 1;
        shift = 0;
        
        // Decode longitude
        do {
          b = encodedPolyline.codeUnitAt(index++) - 63 - 1;
          result += b << shift;
          shift += 5;
        } while (b >= 0x1f);
        
        lng += (result & 1) != 0 ? ~(result >> 1) : (result >> 1);
        
        final double latitude = lat * 1e-5;
        final double longitude = lng * 1e-5;
        
        polylineCoordinates.add(Position(longitude, latitude));
      }
    } catch (e) {
      print("Error decoding polyline: $e");
    }
    
    return polylineCoordinates;
  }

  // Center map on route
  Future<void> _centerMapOnRoute() async {
    if (_mapboxMap == null || _routeData == null) return;
    
    try {
      // Calculate bounds for all points
      final List<NavigationPoint> allPoints = [
        _routeData!.driverLocation,
        ..._routeData!.transitPoints,
        _routeData!.destinationLocation,
      ];
      
      double minLat = 90.0;
      double maxLat = -90.0;
      double minLng = 180.0;
      double maxLng = -180.0;
      
      for (final point in allPoints) {
        minLat = math.min(minLat, point.latitude);
        maxLat = math.max(maxLat, point.latitude);
        minLng = math.min(minLng, point.longitude);
        maxLng = math.max(maxLng, point.longitude);
      }
      
      // Add padding
      final padding = 0.5; // degrees
      minLat -= padding;
      maxLat += padding;
      minLng -= padding;
      maxLng += padding;
      
      // Set camera to bounds
      await _mapboxMap!.setCamera(
        CameraOptions(
          center: Point(
            coordinates: Position((minLng + maxLng) / 2, (minLat + maxLat) / 2),
          ),
          zoom: _calculateZoomLevel(minLat, maxLat, minLng, maxLng),
          padding: MbxEdgeInsets(top: 50.0, left: 50.0, bottom: 50.0, right: 50.0),
        ),
      );
    } catch (e) {
      print("Error centering map on route: $e");
    }
  }

  // Simulate driver movement (for demo purposes)
  void _simulateDriverMovement() {
    if (_routeData == null) return;
    
    setState(() {
      if (_currentRoutePointIndex < _routeData!.transitPoints.length) {
        _currentRoutePointIndex++;
        // Show transit point notification
        _showTransitPointNotification(_currentRoutePointIndex);
      }
    });
  }
  
  // Show notification when reaching transit points
  void _showTransitPointNotification(int pointIndex) {
    final List<String> transitPoints = [
      "Departed from Hanoi Warehouse",
      "Arrived at Thanh Hoa - Rest stop 30 minutes",
      "Arrived at Vinh - Rest 45 minutes",
      "Arrived at Da Nang - Rest 1 hour",
      "Arrived at Nha Trang - Stop 30 minutes",
      "Arrived at Dong Nai - Almost at destination",
      "Arrived at Ho Chi Minh Warehouse - Delivery completed!",
    ];
    
    if (pointIndex < transitPoints.length) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text(transitPoints[pointIndex]),
          backgroundColor: pointIndex == transitPoints.length - 1 
              ? SpatialDesignSystem.successColor 
              : SpatialDesignSystem.primaryColor,
          duration: Duration(seconds: 3),
        ),
      );
    }
  }
  
  // Show completion dialog when route is finished
  void _showCompletionDialog() {
    if (_routeData == null) return;
    
    showDialog(
      context: context,
      barrierDismissible: false,
      builder: (context) => AlertDialog(
        title: Row(
          children: [
            Icon(Icons.check_circle, color: SpatialDesignSystem.successColor),
            SizedBox(width: 8),
            Text("Congratulations!"),
          ],
        ),
        content: Text(
          "You have completed the delivery from Hanoi to Ho Chi Minh City!\n\n"
          "Distance: ${_routeData!.totalDistance.toStringAsFixed(1)} km\n"
          "Estimated time: ${(_routeData!.totalDuration / 60).toStringAsFixed(1)} hours"
        ),
        actions: [
          ElevatedButton(
            onPressed: () {
              Navigator.pop(context); // Close dialog
              Navigator.pop(context, true); // Return to previous screen with result
            },
            style: ElevatedButton.styleFrom(
              backgroundColor: SpatialDesignSystem.successColor,
            ),
            child: Text("Complete", style: TextStyle(color: Colors.white)),
          ),
        ],
      ),
    );
  }
  
  // Call customer function
  void _callCustomer() {
    if (_routeData == null) return;
    
    // Get phone number from data
    String phoneNumber = _routeData!.destinationLocation.name; // Using name as placeholder
    
    // Remove spaces from phone number
    phoneNumber = phoneNumber.replaceAll(' ', '');
    
    // Open phone dialer
    urlLauncherFrave.makePhoneCall('tel:$phoneNumber');
  }

  // Start navigation
  // Calculate zoom level based on bounds
  double _calculateZoomLevel(double minLat, double maxLat, double minLng, double maxLng) {
    const zoomMax = 19.0;
    
    double latDiff = maxLat - minLat;
    double lngDiff = maxLng - minLng;
    
    // Add padding
    latDiff *= 1.2;
    lngDiff *= 1.2;
    
    double latZoom = math.log(360 / latDiff) / math.ln2;
    double lngZoom = math.log(360 / lngDiff) / math.ln2;
    
    // Return the smaller zoom level to ensure both dimensions fit
    return math.min(latZoom, lngZoom).clamp(0.0, zoomMax);
  }

  // Toggle user location following
  void _toggleFollowUser() {
    setState(() {
      _isFollowingUser = !_isFollowingUser;
    });
  }

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Scaffold(
      backgroundColor: isDark
          ? SpatialDesignSystem.darkBackgroundColor
          : SpatialDesignSystem.backgroundColor,
      appBar: AppBar(
        title: Text(
          "Route #${widget.routeId}",
          style: SpatialDesignSystem.subtitleLarge.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        backgroundColor: Colors.transparent,
        elevation: 0,
        leading: IconButton(
          icon: Icon(
            Icons.arrow_back,
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
          onPressed: () => Navigator.pop(context),
        ),
        actions: [
          // My location button
          IconButton(
            icon: Icon(
              _isFollowingUser ? Icons.gps_fixed : Icons.gps_not_fixed,
              color: _isFollowingUser ? SpatialDesignSystem.primaryColor : null,
            ),
            onPressed: _toggleFollowUser,
          ),
        ],
      ),
      body: Column(
        children: [
          // Map
          Expanded(
            child: Padding(
              padding: const EdgeInsets.only(left: 16, right: 16, bottom: 16),
              child: Card(
                elevation: 4,
                shape: RoundedRectangleBorder(
                  borderRadius: SpatialDesignSystem.borderRadiusLarge,
                ),
                child: ClipRRect(
                  borderRadius: SpatialDesignSystem.borderRadiusLarge,
                  child: _isRouteDataLoading || _routeData == null
                    ? Center(
                        child: Column(
                          mainAxisSize: MainAxisSize.min,
                          children: [
                            CircularProgressIndicator(),
                            SizedBox(height: 16),
                            Text("Loading route data...", 
                              style: TextStyle(
                                color: isDark 
                                  ? SpatialDesignSystem.textDarkPrimaryColor
                                  : SpatialDesignSystem.textPrimaryColor
                              )
                            ),
                          ],
                        ),
                      )
                    : _errorMessage != null
                      ? Center(
                          child: Column(
                            mainAxisSize: MainAxisSize.min,
                            children: [
                              Icon(Icons.error_outline, 
                                color: SpatialDesignSystem.errorColor,
                                size: 48,
                              ),
                              SizedBox(height: 16),
                              Text(_errorMessage!, 
                                textAlign: TextAlign.center,
                                style: TextStyle(
                                  color: isDark 
                                    ? SpatialDesignSystem.textDarkPrimaryColor
                                    : SpatialDesignSystem.textPrimaryColor
                                )
                              ),
                              SizedBox(height: 16),
                              ElevatedButton(
                                onPressed: () {
                                  _loadRouteData();
                                  MapboxOptions.setAccessToken(_mapboxAccessToken);
                                },
                                child: Text("Retry"),
                              ),
                            ],
                          ),
                        )
                      : MapboxMapView(
                          onMapCreated: _onMapCreated,
                          isLoading: _isMapLoading,
                          initialCameraOptions: CameraOptions(
                            center: Point(
                              coordinates: Position(
                                _routeData!.driverLocation.longitude,
                                _routeData!.driverLocation.latitude,
                              ),
                            ),
                            zoom: 15.0,
                          ),
                        ),
                ),
              ),
            ),
          ),
          
          // Route instructions
          if (_routeData != null && _routeData!.routeSteps.isNotEmpty)
            RouteInstructionsCard(
              routeSteps: _routeData!.routeSteps,
              currentStepIndex: _currentRoutePointIndex,
              totalDistance: _routeData!.totalDistance,
              totalDuration: _routeData!.totalDuration,
              estimatedArrival: _routeData!.estimatedArrival,
            ),
            
          // Navigation controls
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceEvenly,
              children: [
                ElevatedButton.icon(
                  icon: Icon(_isNavigating ? Icons.pause : Icons.play_arrow),
                  label: Text(_isNavigating ? "Pause" : "Start Navigation"),
                  style: ElevatedButton.styleFrom(
                    backgroundColor: _isNavigating
                        ? Colors.orange
                        : SpatialDesignSystem.primaryColor,
                    foregroundColor: Colors.white,
                    padding: EdgeInsets.symmetric(horizontal: 20, vertical: 12),
                  ),
                  onPressed: () {
                    setState(() {
                      _isNavigating = !_isNavigating;
                    });
                    
                    // Show notification when starting navigation
                    if (_isNavigating) {
                      ScaffoldMessenger.of(context).showSnackBar(
                        SnackBar(
                          content: Text("Navigation started"),
                          backgroundColor: SpatialDesignSystem.primaryColor,
                          duration: Duration(seconds: 2),
                        ),
                      );
                    }
                  },
                ),
                
                // Call customer button
                if (_routeData != null)
                  ElevatedButton.icon(
                    icon: Icon(Icons.phone),
                    label: Text("Call"),
                    style: ElevatedButton.styleFrom(
                      backgroundColor: Colors.blue,
                      foregroundColor: Colors.white,
                      padding: EdgeInsets.symmetric(horizontal: 20, vertical: 12),
                    ),
                    onPressed: _callCustomer,
                  ),
                
                if (_isNavigating)
                  ElevatedButton.icon(
                    icon: Icon(Icons.check),
                    label: Text("Complete"),
                    style: ElevatedButton.styleFrom(
                      backgroundColor: SpatialDesignSystem.successColor,
                      foregroundColor: Colors.white,
                      padding: EdgeInsets.symmetric(horizontal: 20, vertical: 12),
                    ),
                    onPressed: () {
                      // Show completion dialog
                      _showCompletionDialog();
                    },
                  ),
              ],
            ),
          ),
        ],
      ),
    );
  }
}
