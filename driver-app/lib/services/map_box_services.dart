import 'dart:convert';
import 'package:flutter/material.dart';
import 'package:google_maps_flutter/google_maps_flutter.dart' show LatLng;
import 'package:http/http.dart' as http;
import 'package:location/location.dart';
import 'package:permission_handler/permission_handler.dart' as permission_handler;
import 'package:ktc_logistics_driver/domain/models/map/mapbox_route_models.dart';

/*
 * Tập tin: map_box_services.dart
 * Mô tả: Gộp tất cả các dịch vụ liên quan đến bản đồ và vị trí trong một tập tin duy nhất.
 * Bao gồm:
 * - LocationService: Quản lý vị trí thiết bị và quyền truy cập
 * - MapboxDirectionsService: Tương tác với Mapbox API để lấy chỉ đường
 * - MapBoxServices: API legacy đơn giản để tương thích với mã nguồn cũ
 */

//==============================================================================
// PHẦN 1: LOCATION SERVICE - DỊCH VỤ VỊ TRÍ
//==============================================================================

class LocationService {
  final Location _location = Location();
  bool _hasLocationPermission = false;
  LocationData? _currentLocation;

  // Getter for current location
  LocationData? get currentLocation => _currentLocation;
  
  // Getter for permission status
  bool get hasLocationPermission => _hasLocationPermission;

  // Initialize location service and check permissions
  Future<bool> initialize() async {
    try {
      // Check existing permission status first
      permission_handler.PermissionStatus locationPermission = 
          await permission_handler.Permission.location.status;
      
      // If permission is already granted, proceed
      if (locationPermission.isGranted) {
        _hasLocationPermission = true;
        await _setupLocationService();
        await _getCurrentLocation();
        return true;
      }
      
      // Otherwise, request permission
      return await requestPermission();
    } catch (e) {
      print("Error initializing location service: $e");
      return false;
    }
  }

  // Request location permission
  Future<bool> requestPermission() async {
    try {
      // Request location permission
      permission_handler.PermissionStatus locationPermission = 
          await permission_handler.Permission.location.request();
      
      if (locationPermission.isDenied) {
        return false;
      }
      
      if (locationPermission.isPermanentlyDenied) {
        return false;
      }
      
      // Request background location permission (for tracking)
      if (locationPermission.isGranted) {
        _hasLocationPermission = true;
        permission_handler.PermissionStatus backgroundPermission = 
            await permission_handler.Permission.locationAlways.status;
        if (backgroundPermission.isDenied) {
          await permission_handler.Permission.locationAlways.request();
        }
        
        // Setup location service
        await _setupLocationService();
        await _getCurrentLocation();
        return true;
      }
      
      return false;
    } catch (e) {
      print("Error requesting location permission: $e");
      return false;
    }
  }

  // Setup location service
  Future<void> _setupLocationService() async {
    if (!_hasLocationPermission) return;
    
    bool serviceEnabled;
    PermissionStatus permissionGranted;

    serviceEnabled = await _location.serviceEnabled();
    if (!serviceEnabled) {
      serviceEnabled = await _location.requestService();
      if (!serviceEnabled) {
        return;
      }
    }

    permissionGranted = await _location.hasPermission();
    if (permissionGranted == PermissionStatus.denied) {
      permissionGranted = await _location.requestPermission();
      if (permissionGranted != PermissionStatus.granted) {
        return;
      }
    }
    
    // Configure location settings for accurate tracking
    await _location.changeSettings(
      accuracy: LocationAccuracy.high,
      interval: 5000, // Update every 5 seconds
      distanceFilter: 10, // Update when moving 10m
    );
  }

  // Get current location
  Future<LocationData?> _getCurrentLocation() async {
    if (!_hasLocationPermission) return null;
    
    try {
      _currentLocation = await _location.getLocation();
      return _currentLocation;
    } catch (e) {
      print("Error getting current location: $e");
      return null;
    }
  }

  // Start location updates with a callback
  void startLocationUpdates(Function(LocationData) onLocationChanged) {
    if (!_hasLocationPermission) return;
    
    _location.onLocationChanged.listen((locationData) {
      _currentLocation = locationData;
      onLocationChanged(locationData);
    });
  }

  // Show permission dialog
  static Future<void> showPermissionDialog(BuildContext context) async {
    return showDialog(
      context: context,
      barrierDismissible: false,
      builder: (context) => AlertDialog(
        title: Text("Location Permission"),
        content: Text(
          "This app needs location permission to function properly. "
          "Please grant location permission in app settings.",
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text("Later"),
          ),
          ElevatedButton(
            onPressed: () {
              Navigator.pop(context);
              permission_handler.openAppSettings();
            },
            child: Text("Open Settings"),
          ),
        ],
      ),
    );
  }
}

//==============================================================================
// PHẦN 2: MAPBOX DIRECTIONS SERVICE - DỊCH VỤ CHỈ ĐƯỜNG MAPBOX
//==============================================================================

// Model for route points (used internally in this service)
class RoutePoint {
  final double latitude;
  final double longitude;
  final String name;

  RoutePoint({
    required this.latitude,
    required this.longitude,
    required this.name,
  });

  // Convert from NavigationPoint to RoutePoint
  factory RoutePoint.fromNavigationPoint(NavigationPoint point) {
    return RoutePoint(
      latitude: point.latitude,
      longitude: point.longitude,
      name: point.name,
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'latitude': latitude,
      'longitude': longitude,
      'name': name,
    };
  }
}

// Mapbox directions API response model
class MapboxDirectionsResponse {
  final List<MapboxRoute> routes;
  final List<Waypoint> waypoints;

  MapboxDirectionsResponse({
    required this.routes,
    required this.waypoints,
  });

  factory MapboxDirectionsResponse.fromJson(Map<String, dynamic> json) {
    return MapboxDirectionsResponse(
      routes: (json['routes'] as List)
          .map((route) => MapboxRoute.fromJson(route))
          .toList(),
      waypoints: (json['waypoints'] as List)
          .map((waypoint) => Waypoint.fromJson(waypoint))
          .toList(),
    );
  }
}

class MapboxRoute {
  final String geometry;
  final double distance;
  final double duration;
  final List<MapboxStep> steps;

  MapboxRoute({
    required this.geometry,
    required this.distance,
    required this.duration,
    required this.steps,
  });

  factory MapboxRoute.fromJson(Map<String, dynamic> json) {
    return MapboxRoute(
      geometry: json['geometry'],
      distance: json['distance'].toDouble(),
      duration: json['duration'].toDouble(),
      steps: json['legs'][0]['steps'] != null
          ? (json['legs'][0]['steps'] as List)
              .map((step) => MapboxStep.fromJson(step))
              .toList()
          : [],
    );
  }
}

class MapboxStep {
  final String maneuver;
  final String instruction;
  final double distance;
  final double duration;
  final String geometry;

  MapboxStep({
    required this.maneuver,
    required this.instruction,
    required this.distance,
    required this.duration,
    required this.geometry,
  });

  factory MapboxStep.fromJson(Map<String, dynamic> json) {
    return MapboxStep(
      maneuver: json['maneuver']['type'] ?? '',
      instruction: json['maneuver']['instruction'] ?? '',
      distance: json['distance'].toDouble(),
      duration: json['duration'].toDouble(),
      geometry: json['geometry'] ?? '',
    );
  }
}

class Waypoint {
  final String name;
  final List<double> location;

  Waypoint({
    required this.name,
    required this.location,
  });

  factory Waypoint.fromJson(Map<String, dynamic> json) {
    return Waypoint(
      name: json['name'] ?? '',
      location: List<double>.from(json['location']),
    );
  }
}

// Service to call Mapbox Directions API
class MapboxDirectionsService {
  static const String _baseUrl = "https://api.mapbox.com/directions/v5/mapbox";
  static String? _accessToken;

  // Set the Mapbox access token
  static void setAccessToken(String token) {
    _accessToken = token;
  }

  // Get access token from environment variable if not set
  static String get accessToken {
    return _accessToken ?? const String.fromEnvironment("ACCESS_TOKEN",
        defaultValue: "pk.eyJ1IjoiaHVuZ3BxMyIsImEiOiJjbHR3M3JzdXQwYzE5MnFteDFjYXRlcDEzIn0.GDrXTFKq1wn-FZSiTGrfew");
  }

  // Get directions from Mapbox API
  static Future<MapboxDirectionsResponse?> getDirections({
    required List<dynamic> waypoints,
    String profile = "driving", // driving, walking, cycling
    bool steps = true,
    bool geometries = true,
  }) async {
    if (waypoints.length < 2) {
      throw Exception("At least two waypoints are required");
    }

    // Convert waypoints to RoutePoint if they are NavigationPoint
    final List<RoutePoint> routePoints = waypoints.map((point) {
      if (point is NavigationPoint) {
        return RoutePoint.fromNavigationPoint(point);
      } else if (point is RoutePoint) {
        return point;
      } else {
        throw Exception("Unsupported waypoint type");
      }
    }).toList();

    // Create coordinates string from waypoints
    final coordinates = routePoints
        .map((point) => "${point.longitude},${point.latitude}")
        .join(";");

    // Build query parameters
    final queryParams = {
      "access_token": accessToken,
      "steps": steps.toString(),
      "geometries": geometries ? "polyline" : "geojson",
      "overview": "full",
    };

    // Build URL with query parameters
    final uri = Uri.parse(
        "$_baseUrl/$profile/$coordinates?${queryParams.entries.map((e) => "${e.key}=${e.value}").join("&")}");

    try {
      final response = await http.get(uri);

      if (response.statusCode == 200) {
        final data = json.decode(response.body);
        return MapboxDirectionsResponse.fromJson(data);
      } else {
        print("Error: ${response.statusCode} - ${response.body}");
        throw Exception(
            "Failed to get directions: ${response.statusCode}");
      }
    } catch (e) {
      print("Exception while fetching directions: $e");
      throw Exception("Failed to get directions: $e");
    }
  }
  
  // Get directions from origin to destination (compatible with map_box_services)
  static Future<RouteApiResponse> getSimpleRoute(double originLat, double originLng, 
      double destLat, double destLng, {String? originName, String? destName}) async {
      
    // Create navigation points
    final originPoint = NavigationPoint(
      latitude: originLat,
      longitude: originLng,
      name: originName ?? "Origin"
    );
    
    final destinationPoint = NavigationPoint(
      latitude: destLat,
      longitude: destLng,
      name: destName ?? "Destination"
    );
    
    try {
      // Get directions using the existing method
      final directionsResponse = await getDirections(
        waypoints: [originPoint, destinationPoint],
        profile: "driving",
        steps: true,
      );
      
      if (directionsResponse == null || directionsResponse.routes.isEmpty) {
        throw Exception("No routes found");
      }
      
      final route = directionsResponse.routes[0];
      
      // Convert Mapbox route to NavigationSteps - simplified approach
      final routeSteps = <NavigationStep>[
        NavigationStep(
          stepNumber: 1,
          instruction: "Start your journey",
          distance: route.distance,
          duration: route.duration,
          location: originPoint,
          maneuverType: "depart",
        ),
        NavigationStep(
          stepNumber: 2,
          instruction: "You have arrived at your destination",
          distance: 0,
          duration: 0,
          location: destinationPoint,
          maneuverType: "arrive",
        ),
      ];
      
      // Calculate estimated arrival time
      final now = DateTime.now();
      final estimatedArrival = now.add(Duration(seconds: route.duration.round()));
      
      return RouteApiResponse(
        routeId: DateTime.now().millisecondsSinceEpoch.toString(),
        driverLocation: originPoint,
        destinationLocation: destinationPoint,
        transitPoints: [], // No transit points in this simple route
        routeSteps: routeSteps,
        totalDistance: route.distance,
        totalDuration: route.duration,
        estimatedArrival: estimatedArrival,
        routePolyline: route.geometry,
      );
    } catch (e) {
      print("Error getting simple route: $e");
      rethrow;
    }
  }
}

//==============================================================================
// PHẦN 3: MAPBOX SERVICES LEGACY - DỊCH VỤ MAPBOX TƯƠNG THÍCH NGƯỢC
//==============================================================================

/// Lớp này được giữ lại để đảm bảo tính tương thích với mã nguồn cũ.
/// Khuyến nghị: Sử dụng MapboxDirectionsService trực tiếp trong mã mới.
class MapBoxServices {

  /// Lấy chỉ đường từ điểm xuất phát đến điểm đích
  Future<RouteApiResponse> getCoordsOriginAndDestinationDelivery(LatLng origin, LatLng destination) async {
    return MapboxDirectionsService.getSimpleRoute(
      origin.latitude, 
      origin.longitude,
      destination.latitude, 
      destination.longitude
    );
  }
}

// Singleton instances for easy access
final mapBoxServices = MapBoxServices();
final locationService = LocationService();
