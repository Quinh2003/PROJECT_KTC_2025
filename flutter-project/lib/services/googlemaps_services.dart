import 'package:flutter/material.dart';
import 'package:url_launcher/url_launcher.dart';
import 'package:geolocator/geolocator.dart';

class GoogleMapsService {
  static final GoogleMapsService _instance = GoogleMapsService._internal();
  factory GoogleMapsService() => _instance;
  GoogleMapsService._internal();
  
  /// Helper method to extract coordinates safely
  String _extractCoordinates(Map<String, dynamic> location) {
    // Ensure we get doubles
    final latitude = location['latitude'] is double 
        ? location['latitude'] 
        : double.tryParse(location['latitude'].toString()) ?? 0.0;
        
    final longitude = location['longitude'] is double 
        ? location['longitude'] 
        : double.tryParse(location['longitude'].toString()) ?? 0.0;
        
    return "$latitude,$longitude";
  }
  
  /// Opens Google Maps with a route from current location to delivery location
  Future<bool> openGoogleMapsWithRoute({
    required BuildContext context,
    Map<String, dynamic>? pickupLocation,
    required Map<String, dynamic> deliveryLocation,
    List<Map<String, dynamic>> transitPoints = const [],
  }) async {
    try {
      String origin;
      
      // If pickupLocation is provided, use its coordinates
      // Otherwise use "current" keyword for device's current location
      if (pickupLocation != null) {
        origin = _extractCoordinates(pickupLocation);
      } else {
        origin = "current";
      }
      
      // Destination - delivery location
      final String destination = _extractCoordinates(deliveryLocation);
      
      // Construct Google Maps URL base
      String url = "https://www.google.com/maps/dir/?api=1"
          "&origin=$origin"
          "&destination=$destination"
          "&travelmode=driving";
      
      // Add transit points as waypoints if any
      if (transitPoints.isNotEmpty) {
        List<String> waypointsList = [];
        
        // Add transit points
        for (var point in transitPoints) {
          waypointsList.add(_extractCoordinates(point));
        }
        
        // Join waypoints with pipe character and add to URL
        if (waypointsList.isNotEmpty) {
          final String waypoints = waypointsList.join('|');
          url += "&waypoints=$waypoints";
        }
      }
      
      debugPrint("Opening Google Maps with URL: $url");
      
      // Launch Google Maps
      final Uri uri = Uri.parse(url);
      if (await canLaunchUrl(uri)) {
        await launchUrl(uri, mode: LaunchMode.externalApplication);
        return true;
      } else {
        // Show error dialog if Google Maps can't be opened
        if (context.mounted) {
          showDialog(
            context: context,
            builder: (context) => AlertDialog(
              title: const Text("Google Maps Not Available"),
              content: const Text("Please install Google Maps to use navigation."),
              actions: [
                TextButton(
                  onPressed: () => Navigator.pop(context),
                  child: const Text("OK"),
                ),
              ],
            ),
          );
        }
        debugPrint("Could not launch Google Maps URL");
        return false;
      }
      } catch (e) {
        debugPrint("Error launching URL: $e");
        // Show error dialog if there's an exception
        if (context.mounted) {
          showDialog(
            context: context,
            builder: (context) => AlertDialog(
              title: const Text("Navigation Error"),
              content: Text("Error opening Google Maps: $e"),
              actions: [
                TextButton(
                  onPressed: () => Navigator.pop(context),
                  child: const Text("OK"),
                ),
              ],
            ),
          );
        }
        return false;
      }
    }
  
  /// Gets the current device position and returns it as a Map
  Future<Map<String, dynamic>> getCurrentPositionData() async {
    // Get current position for more realistic data
    Position currentPosition;
    try {
      currentPosition = await Geolocator.getCurrentPosition();
    } catch (e) {
      // Fallback coordinates if location can't be determined
      currentPosition = Position(
        latitude: 10.7769, 
        longitude: 106.7009, // Ho Chi Minh City coordinates
        timestamp: DateTime.now(),
        accuracy: 0,
        altitude: 0,
        heading: 0,
        speed: 0,
        speedAccuracy: 0,
        altitudeAccuracy: 0,
        headingAccuracy: 0,
      );
    }
    
    return {
      'latitude': currentPosition.latitude,
      'longitude': currentPosition.longitude,
      'address': 'Current Location'
    };
  }
  
  /// Gets dummy route data for testing
  Future<Map<String, dynamic>> getDummyRouteData() async {
    // Get current position for more realistic data
    Position currentPosition;
    try {
      currentPosition = await Geolocator.getCurrentPosition();
    } catch (e) {
      // Fallback coordinates if location can't be determined
      currentPosition = Position(
        latitude: 10.7769, 
        longitude: 106.7009, // Ho Chi Minh City coordinates
        timestamp: DateTime.now(),
        accuracy: 0,
        altitude: 0,
        heading: 0,
        speed: 0,
        speedAccuracy: 0,
        altitudeAccuracy: 0,
        headingAccuracy: 0,
      );
    }
    
    // Create dummy route with current location - all numeric values should be doubles
    final double latBase = currentPosition.latitude;
    final double lngBase = currentPosition.longitude;
    
    return {
      'pickupLocation': {
        'latitude': latBase + 0.02, // ~2km north
        'longitude': lngBase + 0.01,
        'address': '123 Pickup Street, District 1, HCMC',
      },
      'transitPoints': [
        {
          'latitude': latBase + 0.03, // ~3km north
          'longitude': lngBase + 0.03,
          'address': '456 Transit Point, District 2, HCMC',
        },
        {
          'latitude': latBase + 0.04, // ~4km north
          'longitude': lngBase + 0.02,
          'address': '789 Second Transit, District 3, HCMC',
        },
      ],
      'deliveryLocation': {
        'latitude': latBase + 0.05, // ~5km north
        'longitude': lngBase + 0.04,
        'address': '101 Delivery Avenue, District 4, HCMC',
      },
    };
  }
  
  /// Gets route data from API (to be implemented with real API)
  Future<Map<String, dynamic>> getRouteData(String orderId) async {
    // TODO: Implement actual API call to get route data
    
    // For now, return dummy data
    return getDummyRouteData();
  }
}
