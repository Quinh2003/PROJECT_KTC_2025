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
  }
