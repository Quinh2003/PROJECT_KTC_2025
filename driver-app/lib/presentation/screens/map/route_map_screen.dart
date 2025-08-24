import 'dart:async';
import 'package:flutter/material.dart';
import 'package:mapbox_maps_flutter/mapbox_maps_flutter.dart';
import '../../design/spatial_ui.dart';
import '../../../services/map_box_services.dart';

class RouteMapScreen extends StatefulWidget {
  final String routeId;

  const RouteMapScreen({super.key, required this.routeId});

  @override
  _RouteMapScreenState createState() => _RouteMapScreenState();
}

class _RouteMapScreenState extends State<RouteMapScreen> {
  // Services
  final LocationService _locationService = LocationService();

  // State variables
  bool _isRouteDataLoading = true;
  String? _errorMessage;
  bool _isNavigating = false;
  bool _isFollowingUser = true;
  Timer? _locationUpdateTimer;

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
        setState(() {
          // Update location state if needed
        });
      });
    } else {
      // Show permission dialog if needed
      if (mounted) {
        LocationService.showPermissionDialog(context);
      }
    }
  }

  // Load route data from API - safer version
  Future<void> _loadRouteDataSafely() async {
    print("🔄 Starting to load route data for ${widget.routeId}");
    setState(() {
      _isRouteDataLoading = true;
      _errorMessage = null;
    });
    
    // Simulate loading time
    await Future.delayed(Duration(seconds: 2));
    
    print("✅ Route data loaded successfully");
    if (mounted) {
      setState(() {
        _isRouteDataLoading = false;
      });
    }
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
      body: Stack(
        children: [
          // Real Mapbox map display
          _errorMessage != null
              ? Center(
                  child: Column(
                    mainAxisSize: MainAxisSize.min,
                    children: [
                      Icon(Icons.error_outline, 
                        color: Colors.red,
                        size: 64,
                      ),
                      SizedBox(height: 16),
                      Text("Map Error", 
                        style: TextStyle(
                          fontSize: 18,
                          fontWeight: FontWeight.bold,
                          color: Colors.red
                        )
                      ),
                      SizedBox(height: 8),
                      Text(_errorMessage ?? "Unknown error", 
                        textAlign: TextAlign.center,
                        style: TextStyle(
                          color: isDark 
                            ? SpatialDesignSystem.textDarkSecondaryColor
                            : SpatialDesignSystem.textSecondaryColor
                        )
                      ),
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
                                : SpatialDesignSystem.textPrimaryColor
                            )
                          ),
                        ],
                      ),
                    )
                  : MapWidget(
                      key: ValueKey("route_map_${widget.routeId}"),
                      cameraOptions: CameraOptions(
                        center: Point(coordinates: Position(105.8342, 21.0278)), // Hanoi center
                        zoom: 14.0,
                        bearing: 0,
                        pitch: 0,
                      ),
                      styleUri: MapboxStyles.MAPBOX_STREETS,
                      textureView: false, // Try without texture view first
                      onMapCreated: (MapboxMap mapboxMap) {
                        print("✅ Map created successfully for route: ${widget.routeId}");
                      },
                    ),
          
          // Simple mock route instructions
          if (!_isRouteDataLoading && _errorMessage == null)
            Positioned(
              top: 8,
              left: 8,
              right: 8,
              child: Container(
                margin: const EdgeInsets.symmetric(horizontal: 8.0),
                child: Card(
                  elevation: 6,
                  shape: RoundedRectangleBorder(
                    borderRadius: BorderRadius.circular(12),
                  ),
                  color: isDark ? SpatialDesignSystem.darkSurfaceColor : Colors.white,
                  child: Padding(
                    padding: const EdgeInsets.all(12.0),
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      mainAxisSize: MainAxisSize.min,
                      children: [
                        // Summary row
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceBetween,
                          children: [
                            _buildCompactInfo("Duration", "30 min", isDark),
                            _buildCompactInfo("Distance", "1.3 km", isDark),
                            _buildCompactInfo("Arrival", "16:01", isDark),
                          ],
                        ),
                        
                        Divider(height: 16, thickness: 0.5),
                        
                        // Current instruction
                        Row(
                          children: [
                            Container(
                              padding: EdgeInsets.all(6),
                              decoration: BoxDecoration(
                                color: SpatialDesignSystem.primaryColor.withOpacity(0.1),
                                borderRadius: BorderRadius.circular(6),
                              ),
                              child: Icon(
                                Icons.arrow_forward,
                                color: SpatialDesignSystem.primaryColor,
                                size: 18,
                              ),
                            ),
                            SizedBox(width: 12),
                            Expanded(
                              child: Column(
                                crossAxisAlignment: CrossAxisAlignment.start,
                                children: [
                                  Text(
                                    "Head north on Đường Láng",
                                    style: TextStyle(
                                      fontSize: 14,
                                      fontWeight: FontWeight.w600,
                                      color: isDark
                                          ? SpatialDesignSystem.textDarkPrimaryColor
                                          : SpatialDesignSystem.textPrimaryColor,
                                    ),
                                    maxLines: 2,
                                    overflow: TextOverflow.ellipsis,
                                  ),
                                  SizedBox(height: 2),
                                  Text(
                                    "500 m • 2 min",
                                    style: TextStyle(
                                      fontSize: 12,
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
                        
                        // Progress indicator
                        SizedBox(height: 8),
                        Row(
                          children: [
                            Expanded(
                              child: LinearProgressIndicator(
                                value: 0.25,
                                backgroundColor: isDark
                                    ? SpatialDesignSystem.darkSurfaceColorSecondary
                                    : SpatialDesignSystem.lightGrey,
                                valueColor: AlwaysStoppedAnimation<Color>(
                                  SpatialDesignSystem.primaryColor,
                                ),
                                minHeight: 3,
                              ),
                            ),
                            SizedBox(width: 8),
                            Text(
                              "1/4",
                              style: TextStyle(
                                fontSize: 11,
                                color: isDark
                                    ? SpatialDesignSystem.textDarkSecondaryColor
                                    : SpatialDesignSystem.textSecondaryColor,
                              ),
                            ),
                          ],
                        ),
                      ],
                    ),
                  ),
                ),
              ),
            ),
            
          // Navigation controls
          Positioned(
            bottom: 20,
            left: 16,
            right: 16,
            child: Container(
              padding: const EdgeInsets.symmetric(horizontal: 16.0, vertical: 12.0),
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
                      icon: Icon(_isNavigating ? Icons.pause : Icons.play_arrow, size: 18),
                      label: Text(_isNavigating ? "Pause" : "Start", style: TextStyle(fontSize: 13)),
                      style: ElevatedButton.styleFrom(
                        backgroundColor: _isNavigating
                            ? Colors.orange
                            : SpatialDesignSystem.primaryColor,
                        foregroundColor: Colors.white,
                        padding: EdgeInsets.symmetric(horizontal: 12, vertical: 10),
                        shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(18)),
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
                        padding: EdgeInsets.symmetric(horizontal: 12, vertical: 10),
                        shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(18)),
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
                        label: Text("Complete", style: TextStyle(fontSize: 13)),
                        style: ElevatedButton.styleFrom(
                          backgroundColor: SpatialDesignSystem.successColor,
                          foregroundColor: Colors.white,
                          padding: EdgeInsets.symmetric(horizontal: 12, vertical: 10),
                          shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(18)),
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

  // Helper method for compact info display
  Widget _buildCompactInfo(String label, String value, bool isDark) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.center,
      mainAxisSize: MainAxisSize.min,
      children: [
        Text(
          label,
          style: TextStyle(
            fontSize: 11,
            color: isDark
                ? SpatialDesignSystem.textDarkSecondaryColor
                : SpatialDesignSystem.textSecondaryColor,
          ),
        ),
        Text(
          value,
          style: TextStyle(
            fontSize: 13,
            fontWeight: FontWeight.bold,
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
      ],
    );
  }

  // Simple completion dialog
  void _showSimpleCompletionDialog() {
    showDialog(
      context: context,
      barrierDismissible: false,
      builder: (context) => AlertDialog(
        title: Row(
          children: [
            Icon(Icons.check_circle, color: SpatialDesignSystem.successColor),
            SizedBox(width: 8),
            Text("Route Completed!"),
          ],
        ),
        content: Text(
          "You have successfully completed the route #${widget.routeId}!\n\n"
          "Distance: 1.3 km\n"
          "Duration: 30 minutes"
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
}
