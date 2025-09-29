/*
 * File: mapbox_route_models.dart
 * Purpose: Consolidated models for Mapbox navigation and route display
 * Context: Direct navigation screens, route display and map visualization
 */

import 'dart:math' as math;

// ===================== Navigation Models =====================

class NavigationPoint {
  final double latitude;
  final double longitude;
  final String name;

  NavigationPoint({
    required this.latitude,
    required this.longitude,
    required this.name,
  });

  Map<String, dynamic> toJson() {
    return {
      'latitude': latitude,
      'longitude': longitude,
      'name': name,
    };
  }

  factory NavigationPoint.fromJson(Map<String, dynamic> json) {
    return NavigationPoint(
      latitude: json['latitude']?.toDouble() ?? 0.0,
      longitude: json['longitude']?.toDouble() ?? 0.0,
      name: json['name'] ?? '',
    );
  }
}

class RouteApiResponse {
  final String routeId;
  final NavigationPoint driverLocation;
  final NavigationPoint destinationLocation;
  final List<NavigationPoint> transitPoints;
  final List<NavigationStep> routeSteps;
  final double totalDistance;
  final double totalDuration;
  final DateTime estimatedArrival;
  final String routePolyline;

  RouteApiResponse({
    required this.routeId,
    required this.driverLocation,
    required this.destinationLocation,
    required this.transitPoints,
    required this.routeSteps,
    required this.totalDistance,
    required this.totalDuration,
    required this.estimatedArrival,
    required this.routePolyline,
  });

  factory RouteApiResponse.fromJson(Map<String, dynamic> json) {
    return RouteApiResponse(
      routeId: json['routeId'] ?? '',
      driverLocation: NavigationPoint.fromJson(json['driverLocation'] ?? {}),
      destinationLocation: NavigationPoint.fromJson(json['destinationLocation'] ?? {}),
      transitPoints: (json['transitPoints'] as List?)
              ?.map((point) => NavigationPoint.fromJson(point))
              .toList() ??
          [],
      routeSteps: (json['routeSteps'] as List?)
              ?.map((step) => NavigationStep.fromJson(step))
              .toList() ??
          [],
      totalDistance: json['totalDistance']?.toDouble() ?? 0.0,
      totalDuration: json['totalDuration']?.toDouble() ?? 0.0,
      estimatedArrival: DateTime.parse(json['estimatedArrival'] ?? DateTime.now().toIso8601String()),
      routePolyline: json['routePolyline'] ?? '',
    );
  }

  // Factory method to fetch route data from API (mock implementation)
  static Future<RouteApiResponse> getRouteData(String routeId) async {
    // Mock data for demonstration - in real app this would call an API
    final List<NavigationPoint> waypoints = [
      NavigationPoint(latitude: 21.028511, longitude: 105.804817, name: "Hanoi"),
      NavigationPoint(latitude: 21.035287, longitude: 105.834160, name: "Ho Chi Minh Mausoleum"),
      NavigationPoint(latitude: 21.047763, longitude: 105.801446, name: "West Lake"),
      NavigationPoint(latitude: 21.020520, longitude: 105.841141, name: "Hoan Kiem Lake"),
    ];

    // Simulate API delay
    await Future.delayed(const Duration(seconds: 1));

    return RouteApiResponse(
      routeId: routeId,
      driverLocation: waypoints[0],
      destinationLocation: waypoints.last,
      transitPoints: waypoints.sublist(1, waypoints.length - 1),
      routeSteps: _generateMockRouteSteps(),
      totalDistance: 25.5,
      totalDuration: 1800, // 30 minutes
      estimatedArrival: DateTime.now().add(const Duration(minutes: 30)),
      routePolyline: "mock_polyline_string",
    );
  }

  static List<NavigationStep> _generateMockRouteSteps() {
    return [
      NavigationStep(
        stepNumber: 1,
        instruction: "Head north on Đường Láng",
        distance: 500,
        duration: 120,
        location: NavigationPoint(latitude: 21.028511, longitude: 105.804817, name: "Start"),
        maneuverType: "depart",
      ),
      NavigationStep(
        stepNumber: 2,
        instruction: "Turn right onto Đường Thành",
        distance: 800,
        duration: 180,
        location: NavigationPoint(latitude: 21.032000, longitude: 105.807000, name: "Turn"),
        maneuverType: "turn",
      ),
      NavigationStep(
        stepNumber: 3,
        instruction: "Continue straight for 1.2 km",
        distance: 1200,
        duration: 300,
        location: NavigationPoint(latitude: 21.040000, longitude: 105.815000, name: "Continue"),
        maneuverType: "continue",
      ),
      NavigationStep(
        stepNumber: 4,
        instruction: "You have arrived at your destination",
        distance: 0,
        duration: 0,
        location: NavigationPoint(latitude: 21.047763, longitude: 105.801446, name: "Destination"),
        maneuverType: "arrive",
      ),
    ];
  }
}

class NavigationStep {
  final int stepNumber;
  final String instruction;
  final double distance;
  final double duration;
  final NavigationPoint location;
  final String maneuverType;

  NavigationStep({
    required this.stepNumber,
    required this.instruction,
    required this.distance,
    required this.duration,
    required this.location,
    required this.maneuverType,
  });

  factory NavigationStep.fromJson(Map<String, dynamic> json) {
    return NavigationStep(
      stepNumber: json['stepNumber'] ?? 0,
      instruction: json['instruction'] ?? '',
      distance: json['distance']?.toDouble() ?? 0.0,
      duration: json['duration']?.toDouble() ?? 0.0,
      location: NavigationPoint.fromJson(json['location'] ?? {}),
      maneuverType: json['maneuverType'] ?? '',
    );
  }
}

// ===================== Mapbox API Response Models =====================

class DrivingResponse {
  final List<MapboxRoute> routes;
  final List<Waypoint> waypoints;
  final String code;
  final String uuid;

  DrivingResponse({
    required this.routes,
    required this.waypoints,
    required this.code,
    required this.uuid,
  });

  factory DrivingResponse.fromJson(Map<String, dynamic> json) => DrivingResponse(
        routes: json["routes"] != null
            ? List<MapboxRoute>.from(json["routes"].map((x) => MapboxRoute.fromJson(x)))
            : [],
        waypoints: json["waypoints"] != null
            ? List<Waypoint>.from(json["waypoints"].map((x) => Waypoint.fromJson(x)))
            : [],
        code: json["code"] ?? '',
        uuid: json["uuid"] ?? '',
      );
}

class MapboxRoute {
  final String weightName;
  final double weight;
  final double duration;
  final double distance;
  final List<Leg> legs;
  final String geometry;

  MapboxRoute({
    required this.weightName,
    required this.weight,
    required this.duration,
    required this.distance,
    required this.legs,
    required this.geometry,
  });

  factory MapboxRoute.fromJson(Map<String, dynamic> json) => MapboxRoute(
        weightName: json["weight_name"] ?? '',
        weight: json["weight"]?.toDouble() ?? 0.0,
        duration: json["duration"]?.toDouble() ?? 0.0,
        distance: json["distance"]?.toDouble() ?? 0.0,
        legs: json["legs"] != null
            ? List<Leg>.from(json["legs"].map((x) => Leg.fromJson(x)))
            : [],
        geometry: json["geometry"] ?? '',
      );
}

class Leg {
  final List<Step> steps;
  final String summary;
  final double weight;
  final double duration;
  final double distance;

  Leg({
    required this.steps,
    required this.summary,
    required this.weight,
    required this.duration,
    required this.distance,
  });

  factory Leg.fromJson(Map<String, dynamic> json) => Leg(
        steps: json["steps"] != null
            ? List<Step>.from(json["steps"].map((x) => Step.fromJson(x)))
            : [],
        summary: json["summary"] ?? '',
        weight: json["weight"]?.toDouble() ?? 0.0,
        duration: json["duration"]?.toDouble() ?? 0.0,
        distance: json["distance"]?.toDouble() ?? 0.0,
      );
}

class Step {
  final Intersections intersections;
  final String geometry;
  final String maneuver;
  final double duration;
  final double distance;
  final String name;
  final double weight;
  final String mode;
  final String drivingSide;

  Step({
    required this.intersections,
    required this.geometry,
    required this.maneuver,
    required this.duration,
    required this.distance,
    required this.name,
    required this.weight,
    required this.mode,
    required this.drivingSide,
  });

  factory Step.fromJson(Map<String, dynamic> json) => Step(
        intersections: Intersections.fromJson(json["intersections"] ?? {}),
        geometry: json["geometry"] ?? '',
        maneuver: json["maneuver"] ?? '',
        duration: json["duration"]?.toDouble() ?? 0.0,
        distance: json["distance"]?.toDouble() ?? 0.0,
        name: json["name"] ?? '',
        weight: json["weight"]?.toDouble() ?? 0.0,
        mode: json["mode"] ?? '',
        drivingSide: json["driving_side"] ?? '',
      );
}

class Intersections {
  final List<double> out;
  final List<double> entry;
  final List<double> bearings;
  final List<double> location;

  Intersections({
    required this.out,
    required this.entry,
    required this.bearings,
    required this.location,
  });

  factory Intersections.fromJson(Map<String, dynamic> json) => Intersections(
        out: json["out"] != null ? List<double>.from(json["out"]) : [],
        entry: json["entry"] != null ? List<double>.from(json["entry"]) : [],
        bearings: json["bearings"] != null ? List<double>.from(json["bearings"]) : [],
        location: json["location"] != null ? List<double>.from(json["location"]) : [],
      );
}

class Waypoint {
  final String hint;
  final double distance;
  final String name;
  final List<double> location;

  Waypoint({
    required this.hint,
    required this.distance,
    required this.name,
    required this.location,
  });

  factory Waypoint.fromJson(Map<String, dynamic> json) => Waypoint(
        hint: json["hint"] ?? '',
        distance: json["distance"]?.toDouble() ?? 0.0,
        name: json["name"] ?? '',
        location: json["location"] != null ? List<double>.from(json["location"]) : [],
      );
}

// ===================== Utility Methods =====================

class RouteUtils {
  /// Convert polyline string to list of coordinates
  static List<NavigationPoint> decodePolyline(String polyline) {
    // Implementation for polyline decoding
    // This would typically use a polyline decoding algorithm
    return [];
  }

  /// Calculate distance between two points using Haversine formula
  static double calculateDistance(NavigationPoint point1, NavigationPoint point2) {
    const double earthRadius = 6371; // Earth radius in kilometers
    
    double lat1Rad = point1.latitude * (math.pi / 180);
    double lat2Rad = point2.latitude * (math.pi / 180);
    double deltaLat = (point2.latitude - point1.latitude) * (math.pi / 180);
    double deltaLng = (point2.longitude - point1.longitude) * (math.pi / 180);

    double a = math.sin(deltaLat / 2) * math.sin(deltaLat / 2) +
        math.cos(lat1Rad) * math.cos(lat2Rad) * math.sin(deltaLng / 2) * math.sin(deltaLng / 2);
    
    double c = 2 * math.asin(math.sqrt(a));
    
    return earthRadius * c;
  }

  /// Format duration from seconds to human readable string
  static String formatDuration(double seconds) {
    int hours = (seconds / 3600).floor();
    int minutes = ((seconds % 3600) / 60).floor();
    
    if (hours > 0) {
      return '${hours}h ${minutes}m';
    } else {
      return '${minutes}m';
    }
  }

  /// Format distance from meters to human readable string
  static String formatDistance(double meters) {
    if (meters >= 1000) {
      double km = meters / 1000;
      return '${km.toStringAsFixed(1)} km';
    } else {
      return '${meters.toStringAsFixed(0)} m';
    }
  }
}

// ===================== Enums =====================

enum ManeuverType {
  turn,
  newName,
  depart,
  arrive,
  merge,
  onRamp,
  offRamp,
  fork,
  endOfRoad,
  continue_,
  roundabout,
  rotary,
  roundaboutTurn,
  notification,
  exitRoundabout,
  exitRotary,
}

enum NavigationMode {
  driving,
  walking,
  cycling,
  transit,
}

extension ManeuverTypeExtension on ManeuverType {
  String get value {
    switch (this) {
      case ManeuverType.turn:
        return 'turn';
      case ManeuverType.newName:
        return 'new name';
      case ManeuverType.depart:
        return 'depart';
      case ManeuverType.arrive:
        return 'arrive';
      case ManeuverType.merge:
        return 'merge';
      case ManeuverType.onRamp:
        return 'on ramp';
      case ManeuverType.offRamp:
        return 'off ramp';
      case ManeuverType.fork:
        return 'fork';
      case ManeuverType.endOfRoad:
        return 'end of road';
      case ManeuverType.continue_:
        return 'continue';
      case ManeuverType.roundabout:
        return 'roundabout';
      case ManeuverType.rotary:
        return 'rotary';
      case ManeuverType.roundaboutTurn:
        return 'roundabout turn';
      case ManeuverType.notification:
        return 'notification';
      case ManeuverType.exitRoundabout:
        return 'exit roundabout';
      case ManeuverType.exitRotary:
        return 'exit rotary';
    }
  }
}
