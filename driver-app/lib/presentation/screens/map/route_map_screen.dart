import 'dart:async';
import 'dart:convert';
import 'dart:math' as math;
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:mapbox_maps_flutter/mapbox_maps_flutter.dart';
import 'package:http/http.dart' as http;

import '../../design/spatial_ui.dart';

import '../../components/route_instructions_card.dart';
import '../../components/mapbox_view.dart';

import '../../../services/mapbox_services.dart';
import '../../../domain/models/map/mapbox_route_models.dart';
import '../../../data/env/secrets.dart';

class RouteMapScreen extends StatefulWidget {
  final String routeId;

  const RouteMapScreen({super.key, required this.routeId});

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
  bool _isFollowingUser = true; // Luôn bật theo dõi vị trí người dùng
  Timer? _locationUpdateTimer;

  // Camera move tracking
  bool _isMapMoving = false;
  Timer? _mapIdleTimer; // Timer để theo dõi khi nào bản đồ ngừng di chuyển
  bool _autoRecalculateRoute = false; // Tắt tính năng tự động tính toán lộ trình mặc định

  // Route data
  List<Position> _routeCoordinates = [];
  Position? _currentLocation;
  Position? _pickupLocation;
  Position? _deliveryLocation;
  List<Position> _waypointLocations = [];

  // Route instructions from Mapbox
  List<NavigationStep> _routeSteps = [];
  String _currentInstruction = "Đang tải hướng dẫn...";
  double _totalDistance = 0.0;
  int _estimatedDuration = 0; // in seconds
  int _currentStepIndex = 0;

  @override
  void initState() {
    super.initState();

    print("RouteMapScreen initialized for route: ${widget.routeId}");

    // Initialize location service
    _initializeLocation();

    // Load route data with error handling
    _loadRouteDataSafely();
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
        if (mounted &&
            locationData.latitude != null &&
            locationData.longitude != null) {
          setState(() {
            _currentLocation = Position(
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
    // TODO: Update only current location marker instead of recreating all
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

      // Kiểm tra vị trí đã được cập nhật chưa
      if (_currentLocation == null) {
        print("⚠️ Could not get current location, using default");
        // Sử dụng vị trí mặc định thay vì báo lỗi
        _currentLocation = Position(108.2022, 16.0544); // Da Nang center
      }

      // Set pickup and delivery based on real scenario
      // For demo: if user is in Da Nang area, set route to HCMC
      // Otherwise use nearby locations

      double currentLat = _currentLocation!.lat.toDouble();
      double currentLng = _currentLocation!.lng.toDouble();

      // Check if user is in Vietnam Da Nang area (rough bounds)
      bool isInDaNang = (currentLat >= 15.9 && currentLat <= 16.3) &&
          (currentLng >= 107.9 && currentLng <= 108.4);

      if (isInDaNang) {
        // Real Da Nang to HCMC route
        print("📍 Detected location: Da Nang area");
        _pickupLocation = Position(108.2100, 16.0600); // Da Nang warehouse
        _deliveryLocation = Position(106.6297, 10.8231); // HCMC destination
        print("🎯 Route: Da Nang warehouse → HCMC school");
      } else {
        // Use nearby locations for testing
        print("📍 Using nearby test locations");
        _pickupLocation =
            Position(currentLng + 0.01, currentLat + 0.01); // ~1km away
        _deliveryLocation =
            Position(currentLng + 0.05, currentLat + 0.05); // ~5km away
        print("🎯 Route: Current → Pickup (~1km) → Delivery (~5km)");
      }

      print("✅ Real route data prepared:");
      print(
          "   📍 Current: ${_currentLocation!.lng}, ${_currentLocation!.lat}");
      print("   � Pickup: ${_pickupLocation!.lng}, ${_pickupLocation!.lat}");
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

  // Fetch REAL route from Mapbox Directions API
  Future<void> _fetchMapboxRoute() async {
    try {
      if (_currentLocation == null ||
          _pickupLocation == null ||
          _deliveryLocation == null) {
        throw Exception("Missing location data for route calculation");
      }

      print("🌐 Fetching route data using MapboxDirectionsService");
      
      // Create navigation points for the service
      final originPoint = NavigationPoint(
        latitude: _currentLocation!.lat.toDouble(),
        longitude: _currentLocation!.lng.toDouble(),
        name: "Current Location"
      );
      
      final pickupPoint = NavigationPoint(
        latitude: _pickupLocation!.lat.toDouble(),
        longitude: _pickupLocation!.lng.toDouble(),
        name: "Delivery Location"
      );
      
      final deliveryPoint = NavigationPoint(
        latitude: _deliveryLocation!.lat.toDouble(),
        longitude: _deliveryLocation!.lng.toDouble(),
        name: "Delivery Location"
      );
      
      // Call the Mapbox Directions Service
      final directionsResponse = await MapboxDirectionsService.getDirections(
        waypoints: [originPoint, pickupPoint, deliveryPoint],
        profile: "driving-traffic",
        steps: true,
        geometries: true,
      );
      
      if (directionsResponse == null) {
        throw Exception("Failed to get directions from Mapbox");
      }
      
      print("✅ Received directions from Mapbox");
      
      // Extract route coordinates
      _routeCoordinates = directionsResponse.routePoints.map((point) => 
        Position(point.longitude, point.latitude)
      ).toList();
      
      // Extract route information
      _totalDistance = directionsResponse.distance / 1000; // Convert to km
      _estimatedDuration = directionsResponse.duration.round(); // seconds
      
      print("📏 Real distance: ${_totalDistance.toStringAsFixed(1)} km");
      print("⏰ Real duration: ${(_estimatedDuration / 3600).toStringAsFixed(1)} hours");
      
      // Extract turn-by-turn instructions
      _routeSteps.clear();
      
      if (directionsResponse.legs.isNotEmpty) {
        int stepIndex = 0;
        
        for (var leg in directionsResponse.legs) {
          for (var step in leg.steps) {
            NavigationPoint stepLocation = NavigationPoint(
              latitude: step.maneuver.location.latitude,
              longitude: step.maneuver.location.longitude,
              name: step.name
            );
            
            _routeSteps.add(NavigationStep(
              instruction: step.maneuver.instruction,
              distance: step.distance,
              duration: step.duration,
              stepNumber: ++stepIndex,
              location: stepLocation,
              maneuverType: step.maneuver.type,
            ));
          }
        }
      }
      
      // Set first instruction
      if (_routeSteps.isNotEmpty) {
        _currentInstruction = _routeSteps[0].instruction;
        print("🧭 First instruction: $_currentInstruction");
      }
      
      print("✅ Route data successfully loaded:");
      print("   • ${_routeCoordinates.length} route points");
      print("   📋 ${_routeSteps.length} turn instructions");
    } catch (e) {
      print("❌ Error fetching route data: $e");
      
      // Set error message but don't use fake route data
      _errorMessage = "Could not load route data: $e";
      
      // Clear any existing route data
      _routeCoordinates = [];
      _routeSteps = [];
      _totalDistance = 0.0;
      _estimatedDuration = 0;
      _currentInstruction = "Route not available";
    }
  }

  // Wait for current location to be available
  Future<void> _waitForCurrentLocation() async {
    int attempts = 0;
    const maxAttempts = 10; // Wait up to 10 seconds

    // Kiểm tra nếu dịch vụ vị trí đã bật
    bool locationPermissionChecked = false;

    while (_currentLocation == null && attempts < maxAttempts) {
      print("⏳ Waiting for current location... attempt ${attempts + 1}");

      // Kiểm tra quyền vị trí nếu chưa có vị trí sau 2 lần thử
      if (attempts == 2 && !locationPermissionChecked) {
        locationPermissionChecked = true;
        final hasPermission = await _locationService.initialize();

        if (!hasPermission) {
          print("❌ Location permission not granted or GPS is off");
          if (mounted) {
            LocationService.showPermissionDialog(context);
          }
        } else {
          // Khởi động lại cập nhật vị trí
          _locationService.startLocationUpdates((locationData) {
            if (mounted &&
                locationData.latitude != null &&
                locationData.longitude != null) {
              setState(() {
                _currentLocation = Position(
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
      // Sử dụng vị trí mặc định nếu không lấy được vị trí thật
      setState(() {
        _currentLocation =
            Position(108.2022, 16.0544); // Vị trí mặc định (Đà Nẵng)
        print("📍 Using default location (Da Nang center)");
      });
    }
  }

  // Fetch REAL route from Mapbox Directions API
  Future<void> _fetchMapboxRoute() async {
    try {
      if (_currentLocation == null ||
          _pickupLocation == null ||
          _deliveryLocation == null) {
        throw Exception("Missing location data for route calculation");
      }

      print("🌐 Fetching route data using MapboxDirectionsService");
      
      // Create navigation points for the service
      final originPoint = NavigationPoint(
        latitude: _currentLocation!.lat,
        longitude: _currentLocation!.lng,
        name: "Current Location"
      );
      
      final pickupPoint = NavigationPoint(
        latitude: _pickupLocation!.lat,
        longitude: _pickupLocation!.lng,
        name: "Pickup Location"
      );
      
      final deliveryPoint = NavigationPoint(
        latitude: _deliveryLocation!.lat,
        longitude: _deliveryLocation!.lng,
        name: "Delivery Location"
      );
      
      // Call the Mapbox Directions Service
      final directionsResponse = await MapboxDirectionsService.getDirections(
        waypoints: [originPoint, pickupPoint, deliveryPoint],
        profile: "driving-traffic",
        steps: true,
        geometries: true,
      );
      
      if (directionsResponse == null) {
        throw Exception("Failed to get directions from Mapbox");
      }
      
      print("✅ Received directions from Mapbox");
      
      // Extract route coordinates
      _routeCoordinates = directionsResponse.routePoints.map((point) => 
        Position(point.longitude, point.latitude)
      ).toList();
      
      // Extract route information
      _totalDistance = directionsResponse.distance / 1000; // Convert to km
      _estimatedDuration = directionsResponse.duration.round(); // seconds
      
      print("📏 Real distance: ${_totalDistance.toStringAsFixed(1)} km");
      print("⏰ Real duration: ${(_estimatedDuration / 3600).toStringAsFixed(1)} hours");
      
      // Extract turn-by-turn instructions
      _routeSteps.clear();
      
      if (directionsResponse.legs.isNotEmpty) {
        int stepIndex = 0;
        
        for (var leg in directionsResponse.legs) {
          for (var step in leg.steps) {
            _routeSteps.add(NavigationStep(
              instruction: step.maneuver.instruction,
              distance: step.distance,
              duration: step.duration,
              stepNumber: ++stepIndex,
              location: step.name,
              maneuverType: step.maneuver.type,
            ));
          }
        }
      }
      
      // Set first instruction
      if (_routeSteps.isNotEmpty) {
        _currentInstruction = _routeSteps[0].instruction;
        print("🧭 First instruction: $_currentInstruction");
      }
      
      print("✅ Route data successfully loaded:");
      print("   • ${_routeCoordinates.length} route points");
      print("   📋 ${_routeSteps.length} turn instructions");
    } catch (e) {
      print("❌ Error fetching route data: $e");
      
      // Set error message but don't use fake route data
      _errorMessage = "Could not load route data: $e";
      
      // Clear any existing route data
      _routeCoordinates = [];
      _routeSteps = [];
      _totalDistance = 0.0;
      _estimatedDuration = 0;
      _currentInstruction = "Route not available";
    }

      print("🌐 Fetching route data using MapboxDirectionsService");
      
      // Create navigation points for the service
      final originPoint = NavigationPoint(
        latitude: _currentLocation!.lat,
        longitude: _currentLocation!.lng,
        name: "Current Location"
      );
      
      final pickupPoint = NavigationPoint(
        latitude: _pickupLocation!.lat,
        longitude: _pickupLocation!.lng,
        name: "Pickup Location"
      );
      
      final deliveryPoint = NavigationPoint(
        latitude: _deliveryLocation!.lat,
        longitude: _deliveryLocation!.lng,
        name: "Delivery Location"
      );
      
      // Call the Mapbox Directions Service
      final directionsResponse = await MapboxDirectionsService.getDirections(
        waypoints: [originPoint, pickupPoint, deliveryPoint],
        profile: "driving-traffic",
        steps: true,
        geometries: true,
      );
      
      if (directionsResponse == null) {
        throw Exception("Failed to get directions from Mapbox");
      }
      
      print("✅ Received directions from Mapbox");
      
      // Extract route coordinates
      _routeCoordinates = directionsResponse.routePoints.map((point) => 
        Position(point.longitude, point.latitude)
      ).toList();
      
      // Extract route information
      _totalDistance = directionsResponse.distance / 1000; // Convert to km
      _estimatedDuration = directionsResponse.duration.round(); // seconds
      
      print("📏 Real distance: ${_totalDistance.toStringAsFixed(1)} km");
      print("⏰ Real duration: ${(_estimatedDuration / 3600).toStringAsFixed(1)} hours");
      
      // Extract turn-by-turn instructions
      _routeSteps.clear();
      
      if (directionsResponse.legs.isNotEmpty) {
        int stepIndex = 0;
        
        for (var leg in directionsResponse.legs) {
          for (var step in leg.steps) {
            _routeSteps.add(NavigationStep(
              instruction: step.maneuver.instruction,
              distance: step.distance,
              duration: step.duration,
              stepNumber: ++stepIndex,
              location: step.name,
              maneuverType: step.maneuver.type,
            ));
          }
        }
      }
      
      // Set first instruction
      if (_routeSteps.isNotEmpty) {
        _currentInstruction = _routeSteps[0].instruction;
        print("🧭 First instruction: $_currentInstruction");
      }
      
      print("✅ Route data successfully loaded:");
      print("   • ${_routeCoordinates.length} route points");
      print("   📋 ${_routeSteps.length} turn instructions");
    } catch (e) {
      print("❌ Error fetching route data: $e");
      
      // Set error message but don't use fake route data
      _errorMessage = "Could not load route data: $e";
      
      // Clear any existing route data
      _routeCoordinates = [];
      _routeSteps = [];
      _totalDistance = 0.0;
      _estimatedDuration = 0;
      _currentInstruction = "Route not available";
    }
  }
  }
  }

  // Calculate distance between two positions (Haversine formula)
  double _calculateDistance(Position pos1, Position pos2) {
    const double earthRadius = 6371; // km

    double lat1Rad = pos1.lat * (math.pi / 180);
    double lat2Rad = pos2.lat * (math.pi / 180);
    double deltaLatRad = (pos2.lat - pos1.lat) * (math.pi / 180);
    double deltaLngRad = (pos2.lng - pos1.lng) * (math.pi / 180);

    double a = math.sin(deltaLatRad / 2) * math.sin(deltaLatRad / 2) +
        math.cos(lat1Rad) *
            math.cos(lat2Rad) *
            math.sin(deltaLngRad / 2) *
            math.sin(deltaLngRad / 2);
    double c = 2 * math.asin(math.sqrt(a));

    return earthRadius * c;
  }

  // Update camera to current location
  Future<void> _updateCameraToCurrentLocation() async {
    if (_mapboxMap != null) {
      if (_currentLocation != null) {
        // Di chuyển camera đến vị trí hiện tại
        await _mapboxMap!.flyTo(
          CameraOptions(
            center: Point(coordinates: _currentLocation!),
            zoom: 16.0,
          ),
          MapAnimationOptions(duration: 1000),
        );

        // Hiển thị thông báo thành công nếu cần
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(
              content: Text("Đã di chuyển đến vị trí hiện tại"),
              backgroundColor: SpatialDesignSystem.successColor,
              duration: Duration(seconds: 1),
              behavior: SnackBarBehavior.floating,
            ),
          );
        }
      } else {
        // Nếu không có vị trí hiện tại, thử khởi động lại dịch vụ vị trí
        final hasPermission = await _locationService.initialize();
        if (hasPermission) {
          _locationService.startLocationUpdates((locationData) {
            if (mounted &&
                locationData.latitude != null &&
                locationData.longitude != null) {
              setState(() {
                _currentLocation = Position(
                  locationData.longitude!,
                  locationData.latitude!,
                );
              });

              // Thử di chuyển camera sau khi cập nhật vị trí
              _updateCameraToCurrentLocation();
            }
          });

          // Hiển thị thông báo đang tìm vị trí
          if (mounted) {
            ScaffoldMessenger.of(context).showSnackBar(
              SnackBar(
                content: Text("Đang xác định vị trí hiện tại..."),
                backgroundColor: Colors.orange,
                duration: Duration(seconds: 2),
                behavior: SnackBarBehavior.floating,
              ),
            );
          }
        } else {
          // Hiển thị thông báo lỗi nếu không có quyền
          if (mounted) {
            ScaffoldMessenger.of(context).showSnackBar(
              SnackBar(
                content: Text("Không thể xác định vị trí. Vui lòng bật GPS."),
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
    // TODO: Implement proper asset loading for Mapbox markers
    // For now, we'll use text-based markers
    print("⚠️ Using text-based markers (asset loading temporarily disabled)");
  }

  // Add location markers
  Future<void> _addLocationMarkers() async {
    if (_pointAnnotationManager == null) return;

    List<PointAnnotationOptions> annotationOptions = [];

    // Add current location marker (Driver position in Da Nang)
    if (_currentLocation != null) {
      annotationOptions.add(PointAnnotationOptions(
        geometry: Point(coordinates: _currentLocation!),
        textField: "🚗 Tài xế",
        textOffset: [0.0, 1.5],
        textColor: Colors.blue.value,
        textSize: 14.0,
        textHaloColor: Colors.white.value,
        textHaloWidth: 1.0,
        iconImage: "marker-15", // Built-in Mapbox icon
        iconColor: Colors.blue.value,
        iconSize: 1.5,
      ));
    }

    // Add pickup location marker (Warehouse in Da Nang)
    if (_pickupLocation != null) {
      annotationOptions.add(PointAnnotationOptions(
        geometry: Point(coordinates: _pickupLocation!),
        textField: "📦 Kho hàng",
        textOffset: [0.0, 1.5],
        textColor: Colors.green.value,
        textSize: 14.0,
        textHaloColor: Colors.white.value,
        textHaloWidth: 1.0,
        iconImage: "marker-15",
        iconColor: Colors.green.value,
        iconSize: 1.5,
      ));
    }

    // Add delivery location marker (School in HCMC)
    if (_deliveryLocation != null) {
      annotationOptions.add(PointAnnotationOptions(
        geometry: Point(coordinates: _deliveryLocation!),
        textField: "🎓 Trường HCMC",
        textOffset: [0.0, 1.5],
        textColor: Colors.red.value,
        textSize: 14.0,
        textHaloColor: Colors.white.value,
        textHaloWidth: 1.0,
        iconImage: "marker-15",
        iconColor: Colors.red.value,
        iconSize: 1.5,
      ));
    }

    // Add waypoint markers (Cities along the route) - only first 2 for visibility
    List<String> cityNames = ["🏛️ Hội An", "🌊 Quy Nhon"];
    for (int i = 0;
        i < _waypointLocations.length && i < cityNames.length;
        i++) {
      annotationOptions.add(PointAnnotationOptions(
        geometry: Point(coordinates: _waypointLocations[i]),
        textField: cityNames[i],
        textOffset: [0.0, 1.3],
        textColor: Colors.orange.value,
        textSize: 12.0,
        textHaloColor: Colors.white.value,
        textHaloWidth: 1.0,
        iconImage: "circle-15",
        iconColor: Colors.orange.value,
        iconSize: 1.0,
      ));
    }

    await _pointAnnotationManager!.createMulti(annotationOptions);
    print("✅ Added ${annotationOptions.length} location markers");
    print(
        "   - 🚗 Driver: Da Nang (${_currentLocation?.lat}, ${_currentLocation?.lng})");
    print("   - 📦 Pickup: Warehouse Da Nang");
    print("   - 🎓 Delivery: School HCMC");
    print("   - 🏛️ Waypoints: Hoi An, Quy Nhon");
  }

  // Add route line
  Future<void> _addRouteLine() async {
    if (_polylineAnnotationManager == null || _routeCoordinates.isEmpty) return;

    try {
      final polylineOptions = PolylineAnnotationOptions(
        geometry: LineString(coordinates: _routeCoordinates),
        lineColor: Colors.blue.value,
        lineWidth: 4.0,
      );

      await _polylineAnnotationManager!.create(polylineOptions);
      print("✅ Route line added with ${_routeCoordinates.length} points");
    } catch (e) {
      print("❌ Error adding route line: $e");
    }
  }

  // Fit camera to show all route points
  Future<void> _fitCameraToRoute() async {
    if (_mapboxMap == null || _routeCoordinates.isEmpty) return;

    try {
      // Calculate bounds for all route coordinates
      double minLng = _routeCoordinates
          .map((p) => p.lng)
          .reduce((a, b) => a < b ? a : b)
          .toDouble();
      double maxLng = _routeCoordinates
          .map((p) => p.lng)
          .reduce((a, b) => a > b ? a : b)
          .toDouble();
      double minLat = _routeCoordinates
          .map((p) => p.lat)
          .reduce((a, b) => a < b ? a : b)
          .toDouble();
      double maxLat = _routeCoordinates
          .map((p) => p.lat)
          .reduce((a, b) => a > b ? a : b)
          .toDouble();

      // Add padding to bounds
      double padding = 0.005; // About 500m padding
      minLng -= padding;
      maxLng += padding;
      minLat -= padding;
      maxLat += padding;

      // Calculate center and zoom
      Position center = Position((minLng + maxLng) / 2, (minLat + maxLat) / 2);

      await _mapboxMap!.flyTo(
        CameraOptions(
          center: Point(coordinates: center),
          zoom: 13.0, // Reasonable zoom for route overview
        ),
        MapAnimationOptions(duration: 2000),
      );

      print("✅ Camera fitted to route bounds");
    } catch (e) {
      print("❌ Error fitting camera to route: $e");
    }
  }

  // Helper methods for UI
  String _formatDuration(int seconds) {
    if (seconds == 0) return "Đang tính...";

    int hours = seconds ~/ 3600;
    int minutes = (seconds % 3600) ~/ 60;

    if (hours > 0) {
      return "${hours}h ${minutes}m";
    } else {
      return "${minutes}m";
    }
  }

  // Function to manually recalculate route from current camera position
  Future<void> _recalculateRouteFromCurrentView() async {
    if (_mapboxMap == null || !mounted) return;

    try {
      setState(() {
        _isRouteDataLoading = true;
      });

      final cameraState = await _mapboxMap!.getCameraState();
      final centerCoordinates = cameraState.center.coordinates;
      final centerPos =
          Position(centerCoordinates[0]?.toDouble() ?? 0.0, centerCoordinates[1]?.toDouble() ?? 0.0);

      await _loadRouteDataForNewArea(centerPos);
        
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text('Route recalculated for this area'),
          duration: Duration(seconds: 2),
        ),
      );
    } catch (e) {
      print("❌ Error recalculating route: $e");
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text('Failed to recalculate route: $e'),
          duration: Duration(seconds: 2),
          backgroundColor: Colors.red,
        ),
      );
    } finally {
      if (mounted) {
        setState(() {
          _isRouteDataLoading = false;
        });
      }
    }
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
        actions: [
          // Menu cho các tùy chọn bản đồ
          PopupMenuButton(
            tooltip: 'Map Options',
            icon: Icon(Icons.settings),
            onSelected: (value) {
              if (value == 'toggle_auto_recalculate') {
                setState(() {
                  _autoRecalculateRoute = !_autoRecalculateRoute;
                  
                  if (_autoRecalculateRoute) {
                    _startCameraCheckTimer();
                    ScaffoldMessenger.of(context).showSnackBar(
                      SnackBar(
                        content: Text('Auto route recalculation enabled'),
                        duration: Duration(seconds: 2),
                      ),
                    );
                  } else {
                    _mapIdleTimer?.cancel();
                    ScaffoldMessenger.of(context).showSnackBar(
                      SnackBar(
                        content: Text('Auto route recalculation disabled'),
                        duration: Duration(seconds: 2),
                      ),
                    );
                  }
                });
              }
            },
            itemBuilder: (context) => [
              PopupMenuItem(
                value: 'toggle_auto_recalculate',
                child: Row(
                  children: [
                    Icon(
                      _autoRecalculateRoute ? Icons.check_box : Icons.check_box_outline_blank,
                      color: Theme.of(context).primaryColor,
                    ),
                    SizedBox(width: 8),
                    Text('Auto Recalculate Route'),
                  ],
                ),
              ),
            ],
          ),
        ],
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
      ),
      body: Stack(
        children: [
          // Real Mapbox map display
          _errorMessage != null
              ? Center(
                  child: Column(
                    mainAxisSize: MainAxisSize.min,
                    children: [
                      Icon(
                        Icons.error_outline,
                        color: Colors.red,
                        size: 64,
                      ),
                      SizedBox(height: 16),
                      Text("Map Error",
                          style: TextStyle(
                              fontSize: 18,
                              fontWeight: FontWeight.bold,
                              color: Colors.red)),
                      SizedBox(height: 8),
                      Text(_errorMessage ?? "Unknown error",
                          textAlign: TextAlign.center,
                          style: TextStyle(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor)),
                      SizedBox(height: 16),
                      ElevatedButton(
                        onPressed: () {
                          setState(() {
                            _errorMessage = null;
                          });
                          _loadRouteDataSafely();
                        },
                        child: Text("Try Again"),
                      ),
                    ],
                  ),
                )
              : _isRouteDataLoading
                  ? Center(
                      child: Column(
                        mainAxisSize: MainAxisSize.min,
                        children: [
                          CircularProgressIndicator(
                            color: SpatialDesignSystem.primaryColor,
                          ),
                          SizedBox(height: 16),
                          Text("Loading map...",
                              style: TextStyle(
                                  color: isDark
                                      ? SpatialDesignSystem.textDarkPrimaryColor
                                      : SpatialDesignSystem.textPrimaryColor)),
                        ],
                      ),
                    )
                  : MapboxMapView(
                      routeCoordinates: _routeCoordinates,
                      currentLocation: _currentLocation,
                      routeId: widget.routeId,
                      onMapCreated: (MapboxMap mapboxMap) async {
                        print(
                            "✅ Map created successfully for route: ${widget.routeId}");
                        _mapboxMap = mapboxMap;
                        await Future.delayed(Duration(milliseconds: 500));
                        await _setupMapAnnotations();
                        if (_routeCoordinates.isNotEmpty) {
                          await _fitCameraToRoute();
                        }

                        // Thiết lập sự kiện camera để theo dõi khi người dùng di chuyển bản đồ
                        _setupMapIdleListener(mapboxMap);
                      },
                    ),

          // Simple mock route instructions
          if (!_isRouteDataLoading && _errorMessage == null)
            Positioned(
              top: 8,
              left: 8,
              right: 8,
              child: RouteInstructionsCard(
                routeSteps: _routeSteps,
                currentStepIndex: _currentStepIndex,
                totalDistance: _totalDistance,
                totalDuration: _estimatedDuration.toDouble(),
              ),
            ),

          // Nút di chuyển đến vị trí hiện tại và tính toán lại lộ trình
          if (!_isRouteDataLoading && _errorMessage == null)
            Positioned(
              right: 16,
              bottom: 100, // Đặt nút ở trên thanh điều khiển để tránh che khuất
              child: Column(
                mainAxisSize: MainAxisSize.min,
                children: [
                  // Recalculate route button
                  Container(
                    margin: EdgeInsets.only(bottom: 10),
                    decoration: BoxDecoration(
                      color: Colors.white,
                      borderRadius: BorderRadius.circular(30),
                      boxShadow: [
                        BoxShadow(
                          color: Colors.black.withOpacity(0.2),
                          blurRadius: 6,
                          offset: Offset(0, 2),
                        ),
                      ],
                    ),
                    child: IconButton(
                      icon: Icon(
                        Icons.refresh,
                        color: SpatialDesignSystem.primaryColor,
                      ),
                      onPressed: _recalculateRouteFromCurrentView,
                      tooltip: 'Recalculate route for this area',
                    ),
                  ),
                  // My location button
                  Container(
                    decoration: BoxDecoration(
                      color: Colors.white,
                      borderRadius: BorderRadius.circular(30),
                      boxShadow: [
                        BoxShadow(
                          color: Colors.black.withOpacity(0.2),
                          blurRadius: 6,
                          offset: Offset(0, 2),
                        ),
                      ],
                    ),
                    child: IconButton(
                      icon: Icon(
                        Icons.my_location,
                        color: SpatialDesignSystem.primaryColor,
                      ),
                      onPressed: _updateCameraToCurrentLocation,
                      tooltip: 'Go to my location',
                    ),
                  ),
                ],
              ),
            ),

          // Navigation controls
          Positioned(
            bottom: 20,
            left: 16,
            right: 16,
            child: Container(
              padding:
                  const EdgeInsets.symmetric(horizontal: 16.0, vertical: 12.0),
              decoration: BoxDecoration(
                color: isDark
                    ? SpatialDesignSystem.darkSurfaceColor.withOpacity(0.95)
                    : Colors.white.withOpacity(0.95),
                borderRadius: BorderRadius.circular(25),
                boxShadow: [
                  BoxShadow(
                    color: Colors.black.withOpacity(0.2),
                    blurRadius: 12,
                    offset: Offset(0, 4),
                  ),
                ],
              ),
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  Expanded(
                    child: ElevatedButton.icon(
                      icon: Icon(_isNavigating ? Icons.pause : Icons.play_arrow,
                          size: 18),
                      label: Text(_isNavigating ? "Pause" : "Start",
                          style: TextStyle(fontSize: 13)),
                      style: ElevatedButton.styleFrom(
                        backgroundColor: _isNavigating
                            ? Colors.orange
                            : SpatialDesignSystem.primaryColor,
                        foregroundColor: Colors.white,
                        padding:
                            EdgeInsets.symmetric(horizontal: 12, vertical: 10),
                        shape: RoundedRectangleBorder(
                            borderRadius: BorderRadius.circular(18)),
                      ),
                      onPressed: () {
                        setState(() {
                          _isNavigating = !_isNavigating;
                        });

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
                  ),
                  SizedBox(width: 8),
                  Expanded(
                    child: ElevatedButton.icon(
                      icon: Icon(Icons.phone, size: 18),
                      label: Text("Call", style: TextStyle(fontSize: 13)),
                      style: ElevatedButton.styleFrom(
                        backgroundColor: Colors.blue,
                        foregroundColor: Colors.white,
                        padding:
                            EdgeInsets.symmetric(horizontal: 12, vertical: 10),
                        shape: RoundedRectangleBorder(
                            borderRadius: BorderRadius.circular(18)),
                      ),
                      onPressed: () {
                        ScaffoldMessenger.of(context).showSnackBar(
                          SnackBar(
                            content: Text("Calling customer..."),
                            backgroundColor: Colors.blue,
                            duration: Duration(seconds: 2),
                          ),
                        );
                      },
                    ),
                  ),
                  if (_isNavigating) ...[
                    SizedBox(width: 8),
                    Expanded(
                      child: ElevatedButton.icon(
                        icon: Icon(Icons.check, size: 18),
                        label: Text("Done", style: TextStyle(fontSize: 13)),
                        style: ElevatedButton.styleFrom(
                          backgroundColor: SpatialDesignSystem.successColor,
                          foregroundColor: Colors.white,
                          padding: EdgeInsets.symmetric(
                              horizontal: 12, vertical: 10),
                          shape: RoundedRectangleBorder(
                              borderRadius: BorderRadius.circular(18)),
                        ),
                        onPressed: () {
                          _showSimpleCompletionDialog();
                        },
                      ),
                    ),
                  ],
                ],
              ),
            ),
          ),
        ],
      ),
    );
  }

  // Thiết lập listener khi bản đồ ngừng di chuyển
  void _setupMapIdleListener(MapboxMap mapboxMap) {
    // Thiết lập các gesture listener để phát hiện khi bản đồ ngừng di chuyển
    // Không thể sử dụng các API camera event trực tiếp, nên phải dùng gestures
    mapboxMap.gestures.getSettings().then((gestureSettings) {
      // Bật các sự kiện cử chỉ
      gestureSettings.pinchToZoomEnabled = true;
      gestureSettings.rotateEnabled = true;
      gestureSettings.scrollEnabled = true;
      mapboxMap.gestures.updateSettings(gestureSettings);

      print("✅ Map gesture settings updated");
    });

    // Kiểm tra và tải lại dữ liệu khi người dùng di chuyển camera
    // chỉ khi tính năng tự động tính toán lại route được bật
    _startCameraCheckTimer();

    print("✅ Map idle listener setup complete");
  }

  // Khởi động timer để kiểm tra vị trí camera định kỳ
  void _startCameraCheckTimer() {
    // Hủy timer cũ nếu có
    _mapIdleTimer?.cancel();

    // Tạo timer mới kiểm tra mỗi 5 giây nếu tính năng tự động tính toán lại route được bật
    if (_autoRecalculateRoute) {
      _mapIdleTimer = Timer.periodic(Duration(seconds: 5), (timer) {
        if (_mapboxMap == null || !mounted) {
          timer.cancel();
          return;
        }
  
        _checkCurrentMapPosition();
      });
  
      print("✅ Camera check timer started");
    } else {
      print("⚠️ Auto route recalculation is disabled");
    }
  }

  // Kiểm tra vị trí hiện tại của bản đồ
  Future<void> _checkCurrentMapPosition() async {
    if (_mapboxMap == null || _routeCoordinates.isEmpty || _isRouteDataLoading)
      return;

    try {
      // Lấy vị trí trung tâm camera hiện tại
      final cameraState = await _mapboxMap!.getCameraState();
      final centerCoordinates = cameraState.center.coordinates;
      // Chuyển đổi coordinates sang Position, xử lý null safety
      final centerPos =
          Position(centerCoordinates[0] ?? 0.0, centerCoordinates[1] ?? 0.0);

      // Tính khoảng cách từ vị trí trung tâm camera đến điểm gần nhất trên route
      double minDistance = double.infinity;
      for (var routePos in _routeCoordinates) {
        double distance = _calculateDistance(centerPos, routePos);
        if (distance < minDistance) {
          minDistance = distance;
        }
      }

      // Nếu khoảng cách lớn hơn ngưỡng, tải lại dữ liệu route
      double thresholdDistance = 5.0; // km

      if (minDistance > thresholdDistance && _autoRecalculateRoute) {
        print(
            "📍 Map position check: Camera is far from route ($minDistance km)");
        print("🔄 Reloading route data for new area...");

        // Tải lại dữ liệu lộ trình cho khu vực mới
        _loadRouteDataForNewArea(centerPos);
      } else {
        print(
            "✅ Map position check: Camera is close to route ($minDistance km)");
      }
    } catch (e) {
      print("❌ Error checking map position: $e");
    }
  }

  // Tải dữ liệu lộ trình cho khu vực mới
  Future<void> _loadRouteDataForNewArea(Position centerPos) async {
    if (_isRouteDataLoading) return;

    setState(() {
      _isRouteDataLoading = true;
    });

    try {
      // Tính toán điểm đến mới dựa trên vị trí trung tâm camera
      // Đặt điểm đến cách điểm trung tâm khoảng 10km về phía Đông Nam
      double destLat = centerPos.lat - 0.09; // Khoảng 10km về phía Nam
      double destLng = centerPos.lng + 0.09; // Khoảng 10km về phía Đông

      // Cập nhật vị trí đích
      setState(() {
        _pickupLocation = centerPos;
        _deliveryLocation = Position(destLng, destLat);
      });

      // Gọi API Mapbox để lấy dữ liệu lộ trình mới
      await _fetchMapboxRoute();

      // Cập nhật giao diện
      if (_mapboxMap != null) {
        try {
          // Xóa các marker và đường dẫn cũ
          if (_pointAnnotationManager != null) {
            await _pointAnnotationManager?.deleteAll();
          }
          if (_polylineAnnotationManager != null) {
            await _polylineAnnotationManager?.deleteAll();
          }

          // Thêm marker và đường dẫn mới
          await _addLocationMarkers();
          await _addRouteLine();
        } catch (e) {
          print("⚠️ Error updating map annotations: $e");
          
          // Try to recreate the annotation managers if they were disposed
          await _setupMapAnnotations();
        }
      }

      print("✅ Route data reloaded for new area");
    } catch (e) {
      print("❌ Error loading route data for new area: $e");
    } finally {
      if (mounted) {
        setState(() {
          _isRouteDataLoading = false;
        });
      }
    }
  }

  // Hiển thị hộp thoại hoàn thành chuyến đi
  void _showSimpleCompletionDialog() {
    showDialog(
      context: context,
      builder: (BuildContext context) {
        return AlertDialog(
          title: Text("Hoàn thành chuyến đi?"),
          content: Text(
              "Bạn có chắc chắn muốn đánh dấu chuyến đi này đã hoàn thành không?"),
          actions: [
            TextButton(
              child: Text("Hủy"),
              onPressed: () {
                Navigator.of(context).pop();
              },
            ),
            ElevatedButton(
              style: ElevatedButton.styleFrom(
                backgroundColor: SpatialDesignSystem.successColor,
              ),
              child: Text("Hoàn thành", style: TextStyle(color: Colors.white)),
              onPressed: () {
                Navigator.of(context).pop();

                // Hiển thị thông báo thành công
                ScaffoldMessenger.of(context).showSnackBar(
                  SnackBar(
                    content: Text("Chuyến đi đã được đánh dấu hoàn thành!"),
                    backgroundColor: SpatialDesignSystem.successColor,
                    duration: Duration(seconds: 2),
                  ),
                );

                // Quay lại màn hình trước
                Navigator.of(context).pop();
              },
            ),
          ],
        );
      },
    );
  }
}
