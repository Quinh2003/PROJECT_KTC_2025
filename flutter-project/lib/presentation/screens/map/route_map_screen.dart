import 'dart:async';
import 'dart:convert';
import 'dart:math' as math;

import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'package:mapbox_maps_flutter/mapbox_maps_flutter.dart';

import '../../design/spatial_ui.dart';
import '../../../services/mapbox_services.dart';
import '../../../data/env/secrets.dart';

// Custom Position class to avoid conflicts with Mapbox's Position class
class CustomPosition {
  final double lng;
  final double lat;

  CustomPosition(this.lng, this.lat);
  
  // Convert to Mapbox Point
  Point toMapboxPoint() {
    return Point(coordinates: Position(lng, lat));
  }
  
  // Convert List<CustomPosition> to List<Position> for LineString
  static List<Position> toLineStringCoordinates(List<CustomPosition> positions) {
    return positions.map((pos) => Position(pos.lng, pos.lat)).toList();
  }
}

// Navigation step model
class NavigationStep {
  final String instruction;
  final double distance;
  final double duration;
  final int stepNumber;
  final String location;
  final String maneuverType;

  NavigationStep({
    required this.instruction,
    required this.distance,
    required this.duration,
    required this.stepNumber,
    required this.location,
    required this.maneuverType,
  });
}

class RouteMapScreen extends StatefulWidget {
  final String routeId;

  const RouteMapScreen({Key? key, required this.routeId}) : super(key: key);

  @override
  _RouteMapScreenState createState() => _RouteMapScreenState();
}

class _RouteMapScreenState extends State<RouteMapScreen> {
  // Services
  final LocationService _locationService = LocationService();

  // Map and location
  MapboxMap? _mapboxMap;
  PointAnnotationManager? _pointAnnotationManager;
  PolylineAnnotationManager? _polylineAnnotationManager;

  // State variables
  bool _isRouteDataLoading = true;
  String? _errorMessage;
  bool _isNavigating = false;
  bool _isFollowingUser = true; 
  Timer? _locationUpdateTimer;

  // Camera move tracking
  bool _isMapMoving = false;
  Timer? _mapIdleTimer;
  bool _autoRecalculateRoute = false;

  // Route data
  List<CustomPosition> _routeCoordinates = [];
  CustomPosition? _currentLocation;
  CustomPosition? _pickupLocation;
  CustomPosition? _deliveryLocation;
  List<CustomPosition> _waypointLocations = [];

  // Route instructions
  List<NavigationStep> _routeSteps = [];
  String _currentInstruction = "Loading route...";
  double _totalDistance = 0.0;
  int _estimatedDuration = 0;
  int _currentStepIndex = 0;

  @override
  void initState() {
    super.initState();
    print("RouteMapScreen initialized for route: ${widget.routeId}");
    _initializeLocation();
    _loadRouteDataSafely();
  }

  @override
  void dispose() {
    _locationUpdateTimer?.cancel();
    _mapIdleTimer?.cancel();
    super.dispose();
  }

  // Initialize location service
  Future<void> _initializeLocation() async {
    final hasPermission = await _locationService.initialize();
    if (hasPermission) {
      _locationService.startLocationUpdates((locationData) {
        if (mounted &&
            locationData.latitude != null &&
            locationData.longitude != null) {
          setState(() {
            _currentLocation = CustomPosition(
              locationData.longitude!,
              locationData.latitude!,
            );
            print(
                "📍 Real current location updated: ${_currentLocation!.lng}, ${_currentLocation!.lat}");
          });

          // Update user location on map if following
          if (_isFollowingUser && _mapboxMap != null) {
            _updateCameraToCurrentLocation();
          }

          // Update markers if current location changed
          if (_pointAnnotationManager != null) {
            _updateCurrentLocationMarker();
          }
        }
      });
    } else {
      // Show permission dialog if needed
      if (mounted) {
        LocationService.showPermissionDialog(context);
      }
    }
  }

  // Update current location marker on map
  Future<void> _updateCurrentLocationMarker() async {
    // Update only current location marker instead of recreating all
    await _addLocationMarkers();
  }

  // Load route data from real sources
  Future<void> _loadRouteDataSafely() async {
    print("🔄 Starting to load REAL route data for ${widget.routeId}");
    setState(() {
      _isRouteDataLoading = true;
      _errorMessage = null;
    });

    try {
      // Wait for location service to get current position
      await _waitForCurrentLocation();

      // Check if location is available
      if (_currentLocation == null) {
        print("⚠️ Could not get current location, using default");
        // Use default location instead of error
        _currentLocation = CustomPosition(108.2022, 16.0544); // Da Nang center
      }

      // Set pickup and delivery based on real scenario
      double currentLat = _currentLocation!.lat;
      double currentLng = _currentLocation!.lng;

      // Check if user is in Vietnam Da Nang area (rough bounds)
      bool isInDaNang = (currentLat >= 15.9 && currentLat <= 16.3) &&
          (currentLng >= 107.9 && currentLng <= 108.4);

      if (isInDaNang) {
        // Real Da Nang to HCMC route
        print("📍 Detected location: Da Nang area");
        _pickupLocation = CustomPosition(108.2100, 16.0600); // Da Nang warehouse
        _deliveryLocation = CustomPosition(106.6297, 10.8231); // HCMC destination
        print("🎯 Route: Da Nang warehouse → HCMC school");
      } else {
        // Use nearby locations for testing
        print("📍 Using nearby test locations");
        _pickupLocation =
            CustomPosition(currentLng + 0.01, currentLat + 0.01); // ~1km away
        _deliveryLocation =
            CustomPosition(currentLng + 0.05, currentLat + 0.05); // ~5km away
        print("🎯 Route: Current → Pickup (~1km) → Delivery (~5km)");
      }

      print("✅ Real route data prepared:");
      print(
          "   📍 Current: ${_currentLocation!.lng}, ${_currentLocation!.lat}");
      print("   📦 Pickup: ${_pickupLocation!.lng}, ${_pickupLocation!.lat}");
      print(
          "   🎓 Delivery: ${_deliveryLocation!.lng}, ${_deliveryLocation!.lat}");

      // Now fetch REAL route from Mapbox Directions API
      await _fetchMapboxRoute();
    } catch (e) {
      print("❌ Error loading real route data: $e");
      _errorMessage = "Failed to load route: $e";
    }

    if (mounted) {
      setState(() {
        _isRouteDataLoading = false;
      });
    }
  }

  // Wait for current location to be available
  Future<void> _waitForCurrentLocation() async {
    int attempts = 0;
    const maxAttempts = 10; // Wait up to 10 seconds

    // Check if location service is enabled
    bool locationPermissionChecked = false;

    while (_currentLocation == null && attempts < maxAttempts) {
      print("⏳ Waiting for current location... attempt ${attempts + 1}");

      // Check location permission after 2 attempts
      if (attempts == 2 && !locationPermissionChecked) {
        locationPermissionChecked = true;
        final hasPermission = await _locationService.initialize();

        if (!hasPermission) {
          print("❌ Location permission not granted or GPS is off");
          if (mounted) {
            LocationService.showPermissionDialog(context);
          }
        } else {
          // Restart location updates
          _locationService.startLocationUpdates((locationData) {
            if (mounted &&
                locationData.latitude != null &&
                locationData.longitude != null) {
              setState(() {
                _currentLocation = CustomPosition(
                  locationData.longitude!,
                  locationData.latitude!,
                );
              });
            }
          });
        }
      }

      await Future.delayed(Duration(milliseconds: 1000));
      attempts++;
    }

    if (_currentLocation != null) {
      print(
          "✅ Current location obtained: ${_currentLocation!.lat}, ${_currentLocation!.lng}");
    } else {
      print("❌ Timeout waiting for current location");
      // Use default location if can't get real location
      setState(() {
        _currentLocation =
            CustomPosition(108.2022, 16.0544); // Default location (Da Nang)
        print("📍 Using default location (Da Nang center)");
      });
    }
  }

  // Fetch route from Mapbox API
  Future<void> _fetchMapboxRoute() async {
    try {
      final accessToken = await Secrets.getMapboxAccessToken();

      if (_currentLocation == null ||
          _pickupLocation == null ||
          _deliveryLocation == null) {
        throw Exception("Missing location data for route calculation");
      }

      // Build coordinates string: current -> pickup -> delivery
      List<CustomPosition> routeOrder = [
        _currentLocation!, // Driver's real position
        _pickupLocation!, // Pickup location
        _deliveryLocation!, // Final delivery location
      ];

      String coordinates =
          routeOrder.map((pos) => "${pos.lng},${pos.lat}").join(';');

      // Mapbox Directions API URL
      String url =
          'https://api.mapbox.com/directions/v5/mapbox/driving-traffic/$coordinates'
          '?access_token=$accessToken'
          '&geometries=geojson'
          '&overview=full'
          '&steps=true'
          '&language=en'
          '&alternatives=false';

      print("🌐 Calling Mapbox Directions API:");
      print("   URL: $url");
      print("   Coordinates: $coordinates");

      // Make HTTP request to Mapbox API
      final response = await http.get(Uri.parse(url));

      print("📡 API Response: ${response.statusCode}");

      if (response.statusCode == 200) {
        final data = json.decode(response.body);

        print("✅ Mapbox API Success! Parsing response...");

        if (data['routes'] != null && data['routes'].isNotEmpty) {
          final route = data['routes'][0];

          // Extract route geometry (coordinates from Mapbox)
          final geometry = route['geometry'];
          if (geometry != null && geometry['coordinates'] != null) {
            List<dynamic> coords = geometry['coordinates'];
            _routeCoordinates = coords
                .map((coord) =>
                    CustomPosition(coord[0].toDouble(), coord[1].toDouble()))
                .toList();
            print("🗺️ Route coordinates: ${_routeCoordinates.length} points");
          }

          // Extract route information
          _totalDistance =
              (route['distance'] ?? 0).toDouble() / 1000; // Convert to km
          _estimatedDuration = (route['duration'] ?? 0).round(); // seconds

          print("📏 Real distance: ${_totalDistance.toStringAsFixed(1)} km");
          print(
              "⏰ Real duration: ${(_estimatedDuration / 3600).toStringAsFixed(1)} hours");

          // Extract turn-by-turn instructions
          _routeSteps.clear();
          if (route['legs'] != null) {
            for (var leg in route['legs']) {
              if (leg['steps'] != null) {
                int stepNumber = 0;
                for (var step in leg['steps']) {
                  stepNumber++;
                  
                  // Extract step data
                  String instruction = step['maneuver']['instruction'] ?? 'Continue driving';
                  double distance = (step['distance'] ?? 0).toDouble();
                  double duration = (step['duration'] ?? 0).toDouble();
                  String maneuverType = step['maneuver']['type'] ?? 'straight';
                  String location = step['name'] ?? 'Road';
                  
                  _routeSteps.add(NavigationStep(
                    instruction: instruction,
                    distance: distance,
                    duration: duration,
                    stepNumber: stepNumber,
                    location: location,
                    maneuverType: maneuverType,
                  ));
                }
              }
            }
          }

          // Set first instruction
          if (_routeSteps.isNotEmpty) {
            _currentInstruction = _routeSteps[0].instruction;
            print("🧭 First instruction: $_currentInstruction");
          }

          print("✅ Mapbox route data successfully loaded:");
          print("   • ${_routeCoordinates.length} route points");
          print("   📋 ${_routeSteps.length} turn instructions");
        } else {
          throw Exception('No routes found in API response');
        }
      } else {
        throw Exception(
            'Mapbox API HTTP ${response.statusCode}: ${response.body}');
      }
    } catch (e) {
      print("❌ Error fetching Mapbox route: $e");

      // Fallback to simple straight-line route if API fails
      print("🔄 Using fallback route...");
      _routeCoordinates = [
        _currentLocation!,
        _pickupLocation!,
        _deliveryLocation!,
      ];

      // Calculate approximate distance and time for fallback
      double distance1 =
          _calculateDistance(_currentLocation!, _pickupLocation!);
      double distance2 =
          _calculateDistance(_pickupLocation!, _deliveryLocation!);
      _totalDistance = distance1 + distance2;
      _estimatedDuration =
          ((_totalDistance / 50) * 3600).round(); // Assuming 50 km/h

      // Add fallback instructions
      _routeSteps.clear();
      _routeSteps.add(NavigationStep(
        instruction: 'Drive to pickup location',
        distance: distance1 * 1000,
        duration: (distance1 / 50) * 3600,
        stepNumber: 1,
        location: 'Direct route',
        maneuverType: 'straight',
      ));
      _routeSteps.add(NavigationStep(
        instruction: 'Continue to delivery location',
        distance: distance2 * 1000,
        duration: (distance2 / 50) * 3600,
        stepNumber: 2,
        location: 'Direct route',
        maneuverType: 'straight',
      ));

      // Set first fallback instruction
      if (_routeSteps.isNotEmpty) {
        _currentInstruction = _routeSteps[0].instruction;
      }

      print("✅ Fallback route created:");
      print("   • ${_routeCoordinates.length} route points");
      print("   📋 ${_routeSteps.length} instructions");
    }
  }

  // Calculate distance between two positions
  double _calculateDistance(CustomPosition p1, CustomPosition p2) {
    // Haversine formula
    double earthRadius = 6371; // km
    double dLat = _toRadians(p2.lat - p1.lat);
    double dLng = _toRadians(p2.lng - p1.lng);
    double a = math.sin(dLat / 2) * math.sin(dLat / 2) +
        math.cos(_toRadians(p1.lat)) *
            math.cos(_toRadians(p2.lat)) *
            math.sin(dLng / 2) *
            math.sin(dLng / 2);
    double c = 2 * math.asin(math.sqrt(a));

    return earthRadius * c;
  }

  // Convert degrees to radians
  double _toRadians(double degrees) {
    return degrees * math.pi / 180;
  }

  // Update camera to current location
  Future<void> _updateCameraToCurrentLocation() async {
    if (_mapboxMap != null) {
      if (_currentLocation != null) {
        // Move camera to current location
        await _mapboxMap!.flyTo(
          CameraOptions(
            center: _currentLocation!.toMapboxPoint(),
            zoom: 16.0,
          ),
          MapAnimationOptions(duration: 1000),
        );

        // Show success message
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(
              content: Text("Camera moved to current location"),
              backgroundColor: SpatialDesignSystem.successColor,
              duration: Duration(seconds: 1),
              behavior: SnackBarBehavior.floating,
            ),
          );
        }
      } else {
        // If no current location, try restarting location service
        final hasPermission = await _locationService.initialize();
        if (hasPermission) {
          _locationService.startLocationUpdates((locationData) {
            if (mounted &&
                locationData.latitude != null &&
                locationData.longitude != null) {
              setState(() {
                _currentLocation = CustomPosition(
                  locationData.longitude!,
                  locationData.latitude!,
                );
              });

              // Try moving camera after location update
              _updateCameraToCurrentLocation();
            }
          });

          // Show location searching message
          if (mounted) {
            ScaffoldMessenger.of(context).showSnackBar(
              SnackBar(
                content: Text("Detecting current location..."),
                backgroundColor: Colors.orange,
                duration: Duration(seconds: 2),
                behavior: SnackBarBehavior.floating,
              ),
            );
          }
        } else {
          // Show error message if no permission
          if (mounted) {
            ScaffoldMessenger.of(context).showSnackBar(
              SnackBar(
                content: Text("Unable to determine location. Please enable GPS."),
                backgroundColor: Colors.red,
                duration: Duration(seconds: 2),
                behavior: SnackBarBehavior.floating,
              ),
            );
            LocationService.showPermissionDialog(context);
          }
        }
      }
    }
  }

  // Setup map annotations (markers and route)
  Future<void> _setupMapAnnotations() async {
    if (_mapboxMap == null || !mounted) return;

    try {
      // Load custom marker images from assets
      await _loadMarkerAssets();

      // Clear existing annotation managers to avoid memory leaks
      _pointAnnotationManager = null;
      _polylineAnnotationManager = null;

      // Create point annotation manager for markers with proper error handling
      try {
        _pointAnnotationManager =
            await _mapboxMap!.annotations.createPointAnnotationManager();
      } catch (e) {
        print("⚠️ Could not create point annotation manager: $e");
        // Continue execution - we'll try again later if needed
      }

      // Create polyline annotation manager for route with proper error handling
      try {
        _polylineAnnotationManager =
            await _mapboxMap!.annotations.createPolylineAnnotationManager();
      } catch (e) {
        print("⚠️ Could not create polyline annotation manager: $e");
        // Continue execution - we'll try again later if needed
      }

      // Only proceed if we have valid annotation managers
      if (_pointAnnotationManager != null && _polylineAnnotationManager != null) {
        // Add markers
        await _addLocationMarkers();

        // Add route line
        await _addRouteLine();

        print("✅ Map annotations setup completed");
      } else {
        print("⚠️ Map annotations setup incomplete - missing managers");
      }
    } catch (e) {
      print("❌ Error setting up annotations: $e");
    }
  }

  // Load marker assets into map
  Future<void> _loadMarkerAssets() async {
    // Implementation for loading marker assets
    print("⚠️ Using text-based markers (asset loading temporarily disabled)");
  }

  // Add location markers
  Future<void> _addLocationMarkers() async {
    if (_pointAnnotationManager == null) return;

    try {
      // Remove existing annotations to avoid duplication
      await _pointAnnotationManager!.deleteAll();
      
      List<PointAnnotationOptions> annotationOptions = [];

      // Add current location marker
      if (_currentLocation != null) {
        annotationOptions.add(PointAnnotationOptions(
          geometry: _currentLocation!.toMapboxPoint(),
          iconImage: 'driver-marker',
          iconSize: 0.8,
          textField: 'Your Location',
          textOffset: [0.0, -2.0],
          textColor: Colors.black.value,
        ));
      }

      // Add pickup marker
      if (_pickupLocation != null) {
        annotationOptions.add(PointAnnotationOptions(
          geometry: _pickupLocation!.toMapboxPoint(),
          iconImage: 'start-marker',
          iconSize: 0.8,
          textField: 'Pickup Location',
          textOffset: [0.0, -2.0],
          textColor: Colors.black.value,
        ));
      }

      // Add delivery marker
      if (_deliveryLocation != null) {
        annotationOptions.add(PointAnnotationOptions(
          geometry: _deliveryLocation!.toMapboxPoint(),
          iconImage: 'destination-marker',
          iconSize: 0.8,
          textField: 'Delivery Location',
          textOffset: [0.0, -2.0],
          textColor: Colors.black.value,
        ));
      }

      // Add any waypoint markers if available
      List<String> cityNames = ["Stop 1", "Stop 2", "Stop 3", "Stop 4"];
      for (int i = 0;
          i < _waypointLocations.length && i < cityNames.length;
          i++) {
        annotationOptions.add(PointAnnotationOptions(
          geometry: _waypointLocations[i].toMapboxPoint(),
          iconImage: 'delivery-marker',
          iconSize: 0.8,
          textField: cityNames[i],
          textOffset: [0.0, -2.0],
          textColor: Colors.black.value,
        ));
      }

      // Add all markers to map
      await _pointAnnotationManager!.createMulti(annotationOptions);

      print("✅ Added ${annotationOptions.length} location markers:");
      if (_currentLocation != null) {
        print(
            "   - 🚗 Driver: (${_currentLocation?.lat}, ${_currentLocation?.lng})");
      }
    } catch (e) {
      print("❌ Error adding location markers: $e");
    }
  }

  // Add route line
  Future<void> _addRouteLine() async {
    if (_polylineAnnotationManager == null || _routeCoordinates.isEmpty) return;

    try {
      // Simplify route coordinates if too many points to prevent performance issues
      List<CustomPosition> simplifiedRoute = _routeCoordinates;
      if (_routeCoordinates.length > 1000) {
        // Simplify by taking every nth point when the route is very long
        int skipFactor = (_routeCoordinates.length / 1000).ceil();
        simplifiedRoute = [];
        for (int i = 0; i < _routeCoordinates.length; i += skipFactor) {
          if (i < _routeCoordinates.length) {
            simplifiedRoute.add(_routeCoordinates[i]);
          }
        }
        // Always include the first and last points
        if (!simplifiedRoute.contains(_routeCoordinates.first)) {
          simplifiedRoute.insert(0, _routeCoordinates.first);
        }
        if (!simplifiedRoute.contains(_routeCoordinates.last)) {
          simplifiedRoute.add(_routeCoordinates.last);
        }
        print("⚡ Route simplified from ${_routeCoordinates.length} to ${simplifiedRoute.length} points");
      }

      // Create a LineString from simplified route coordinates
      LineString lineString = LineString(
          coordinates: CustomPosition.toLineStringCoordinates(simplifiedRoute));

      // Create polyline options
      PolylineAnnotationOptions options = PolylineAnnotationOptions(
        geometry: lineString,
        lineColor: Colors.blue.value,
        lineWidth: 4.0,
      );

      // Add polyline to map
      await _polylineAnnotationManager!.create(options);

      print("✅ Added route line with ${simplifiedRoute.length} points");
    } catch (e) {
      print("❌ Error adding route line: $e");
    }
  }

  // Fit camera to show entire route
  Future<void> _fitCameraToRoute() async {
    if (_mapboxMap == null || _routeCoordinates.isEmpty) return;

    try {
      // Calculate bounds for all route coordinates
      double minLng = _routeCoordinates.map((p) => p.lng).reduce((a, b) => a < b ? a : b);
      double maxLng = _routeCoordinates.map((p) => p.lng).reduce((a, b) => a > b ? a : b);
      double minLat = _routeCoordinates.map((p) => p.lat).reduce((a, b) => a < b ? a : b);
      double maxLat = _routeCoordinates.map((p) => p.lat).reduce((a, b) => a > b ? a : b);

      // Add padding to bounds
      double padding = 0.05; // About 5km padding
      minLng -= padding;
      maxLng += padding;
      minLat -= padding;
      maxLat += padding;

      // Create a camera that shows the entire route
      CameraOptions cameraOptions = CameraOptions(
        center: Point(coordinates: Position((minLng + maxLng) / 2, (minLat + maxLat) / 2)),
        zoom: 10.0,
      );

      // Animate camera to fit route
      await _mapboxMap!.flyTo(
        cameraOptions,
        MapAnimationOptions(duration: 2000),
      );

      print("✅ Camera fitted to route bounds");
    } catch (e) {
      print("❌ Error fitting camera to route: $e");
    }
  }

  // Format the duration for display
  String _formatDuration(int seconds) {
    if (seconds < 60) return "$seconds sec";
    if (seconds < 3600) {
      int minutes = (seconds / 60).floor();
      return "$minutes min";
    }
    int hours = (seconds / 3600).floor();
    int minutes = ((seconds % 3600) / 60).floor();
    return "${hours}h ${minutes}m";
  }

  // Format the distance for display
  String _formatDistance(double meters) {
    if (meters < 1000) {
      return "${meters.toInt()} m";
    } else {
      final km = meters / 1000;
      return "${km.toStringAsFixed(1)} km";
    }
  }

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Scaffold(
      appBar: AppBar(
        title: Text('Route #${widget.routeId}'),
        backgroundColor: isDark
            ? SpatialDesignSystem.darkBackgroundColor
            : SpatialDesignSystem.backgroundColor,
        actions: [
          // Toggle for automatic route recalculation
          Row(
            children: [
              Text("Auto-update", style: TextStyle(fontSize: 14)),
              Switch(
                value: _autoRecalculateRoute,
                onChanged: (value) {
                  setState(() {
                    _autoRecalculateRoute = value;
                  });
                },
              ),
            ],
          ),
        ],
      ),
      body: Stack(
        children: [
          // Main map view
          _isRouteDataLoading
              ? Center(child: CircularProgressIndicator())
              : _errorMessage != null
                  ? Center(
                      child: Column(
                        mainAxisAlignment: MainAxisAlignment.center,
                        children: [
                          Icon(Icons.error_outline, 
                               color: Colors.red, 
                               size: 48),
                          SizedBox(height: 16),
                          Text(
                            _errorMessage!,
                            style: TextStyle(fontSize: 16),
                            textAlign: TextAlign.center,
                          ),
                          SizedBox(height: 24),
                          ElevatedButton(
                            onPressed: _loadRouteDataSafely,
                            child: Text("Try Again"),
                          ),
                        ],
                      ),
                    )
                  : Theme(
                      // Apply Material theme to ensure compatibility with Mapbox's AppCompat widgets
                      data: Theme.of(context).copyWith(
                        // Use the app's primary color but ensure AppCompat compatibility
                        primaryColor: Theme.of(context).primaryColor,
                        colorScheme: ColorScheme.fromSwatch().copyWith(
                          primary: Theme.of(context).primaryColor,
                          secondary: Theme.of(context).colorScheme.secondary,
                        ),
                      ),
                      child: MapWidget(
                        key: ValueKey('route_map'),
                        onMapCreated: (MapboxMap mapboxMap) {
                          _mapboxMap = mapboxMap;
                          _setupMapAnnotations();
                          _fitCameraToRoute();
                        },
                        styleUri: MapboxStyles.MAPBOX_STREETS,
                        cameraOptions: CameraOptions(
                          center: _currentLocation != null
                              ? _currentLocation!.toMapboxPoint()
                              : Point(coordinates: Position(108.2022, 16.0544)),
                          zoom: 10.0,
                        ),
                      ),
                    ),

          // Route instructions panel
          if (!_isRouteDataLoading && _errorMessage == null)
            Positioned(
              bottom: 16,
              left: 16,
              right: 16,
              child: Card(
                elevation: 4,
                shape: RoundedRectangleBorder(
                  borderRadius: BorderRadius.circular(12),
                ),
                color: isDark
                    ? SpatialDesignSystem.darkSurfaceColor.withOpacity(0.9)
                    : SpatialDesignSystem.surfaceColor.withOpacity(0.9),
                child: Padding(
                  padding: EdgeInsets.all(16),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    mainAxisSize: MainAxisSize.min,
                    children: [
                      Row(
                        children: [
                          Icon(
                            Icons.navigation,
                            color: SpatialDesignSystem.primaryColor,
                            size: 24,
                          ),
                          SizedBox(width: 8),
                          Expanded(
                            child: Column(
                              crossAxisAlignment: CrossAxisAlignment.start,
                              children: [
                                Text(
                                  _currentInstruction,
                                  style: TextStyle(
                                    fontWeight: FontWeight.bold,
                                    fontSize: 16,
                                  ),
                                  maxLines: 2,
                                  overflow: TextOverflow.ellipsis,
                                ),
                                SizedBox(height: 4),
                                Text(
                                  "Total: ${_formatDuration(_estimatedDuration)} (${_totalDistance.toStringAsFixed(1)} km)",
                                  style: TextStyle(
                                    fontSize: 14,
                                    color: isDark
                                        ? SpatialDesignSystem.textDarkSecondaryColor
                                        : SpatialDesignSystem.textSecondaryColor,
                                  ),
                                ),
                              ],
                            ),
                          ),
                        ],
                      ),
                      SizedBox(height: 16),
                      Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: [
                          Expanded(
                            child: ElevatedButton.icon(
                              onPressed: _fitCameraToRoute,
                              icon: Icon(Icons.map),
                              label: Text("Overview"),
                              style: ElevatedButton.styleFrom(
                                backgroundColor: isDark
                                    ? SpatialDesignSystem.darkSurfaceColorSecondary
                                    : SpatialDesignSystem.lightGrey,
                                foregroundColor: isDark
                                    ? SpatialDesignSystem.textDarkPrimaryColor
                                    : SpatialDesignSystem.textPrimaryColor,
                              ),
                            ),
                          ),
                          SizedBox(width: 8),
                          Expanded(
                            child: ElevatedButton.icon(
                              onPressed: () {
                                setState(() {
                                  _isNavigating = !_isNavigating;
                                  _isFollowingUser = _isNavigating;
                                });
                              },
                              icon: Icon(
                                _isNavigating ? Icons.stop : Icons.navigation,
                              ),
                              label: Text(_isNavigating ? "Stop" : "Navigate"),
                              style: ElevatedButton.styleFrom(
                                backgroundColor: SpatialDesignSystem.primaryColor,
                                foregroundColor: Colors.white,
                              ),
                            ),
                          ),
                        ],
                      ),
                    ],
                  ),
                ),
              ),
            ),

          // Loading indicator
          if (_isRouteDataLoading)
            Container(
              color: Colors.black54,
              child: Center(
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    CircularProgressIndicator(),
                    SizedBox(height: 16),
                    Text(
                      "Loading route data...",
                      style: TextStyle(color: Colors.white, fontSize: 16),
                    ),
                  ],
                ),
              ),
            ),
        ],
      ),
      floatingActionButton: Column(
        mainAxisAlignment: MainAxisAlignment.end,
        crossAxisAlignment: CrossAxisAlignment.end,
        children: [
          FloatingActionButton(
            heroTag: 'recalculate',
            onPressed: _loadRouteDataSafely,
            backgroundColor: SpatialDesignSystem.primaryColor,
            child: Icon(Icons.refresh),
            tooltip: 'Recalculate route',
          ),
          SizedBox(height: 16),
          FloatingActionButton(
            heroTag: 'mylocation',
            onPressed: _updateCameraToCurrentLocation,
            backgroundColor: SpatialDesignSystem.primaryColor,
            child: Icon(Icons.my_location),
            tooltip: 'Go to my location',
          ),
        ],
      ),
    );
  }
}
