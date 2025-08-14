import 'package:flutter/material.dart';
import 'dart:ui';
import 'dart:async';
import '../design/spatial_ui.dart';

class RouteMapScreen extends StatefulWidget {
  final String routeId;
  
  const RouteMapScreen({
    Key? key,
    required this.routeId,
  }) : super(key: key);

  @override
  State<RouteMapScreen> createState() => _RouteMapScreenState();
}

class _RouteMapScreenState extends State<RouteMapScreen> {
  bool _isNavigating = false;
  bool _showDirections = true;
  bool _showTraffic = false;
  Timer? _timer;
  double _remainingDistance = 5.7;
  String _estimatedTime = "25 min";
  String _nextDirection = "Continue straight for 2.1 km";
  int _currentStep = 0;
  
  final List<Map<String, dynamic>> _routeSteps = [
    {
      'direction': 'Continue straight for 2.1 km',
      'distance': '2.1 km',
      'icon': Icons.arrow_upward,
    },
    {
      'direction': 'Turn right onto Nguyen Hue Street',
      'distance': '1.2 km',
      'icon': Icons.arrow_forward,
    },
    {
      'direction': 'Take the third exit at the roundabout',
      'distance': '0.5 km',
      'icon': Icons.roundabout_right,
    },
    {
      'direction': 'Turn left onto Le Loi Boulevard',
      'distance': '0.8 km',
      'icon': Icons.arrow_back,
    },
    {
      'direction': 'Arrive at destination on the right',
      'distance': '0 km',
      'icon': Icons.place,
    },
  ];
  
  @override
  void initState() {
    super.initState();
  }
  
  @override
  void dispose() {
    _timer?.cancel();
    super.dispose();
  }
  
  void _startNavigation() {
    setState(() {
      _isNavigating = true;
    });
    
    _timer = Timer.periodic(const Duration(seconds: 10), (timer) {
      setState(() {
        if (_remainingDistance > 0.5) {
          _remainingDistance -= 0.5;
          
          // Update estimated time (roughly 1 min per 0.2 km)
          int minutes = (_remainingDistance / 0.2).round();
          _estimatedTime = "$minutes min";
          
          // Update current step
          if (_currentStep < _routeSteps.length - 1 && timer.tick % 3 == 0) {
            _currentStep++;
            _nextDirection = _routeSteps[_currentStep]['direction'];
          }
        } else {
          _timer?.cancel();
          _remainingDistance = 0.0;
          _estimatedTime = "Arrived";
          _nextDirection = "You have arrived at your destination";
          _currentStep = _routeSteps.length - 1;
          _showArrivalDialog();
        }
      });
    });
  }
  
  void _stopNavigation() {
    _timer?.cancel();
    setState(() {
      _isNavigating = false;
      _remainingDistance = 5.7;
      _estimatedTime = "25 min";
      _nextDirection = "Continue straight for 2.1 km";
      _currentStep = 0;
    });
  }
  
  void _showArrivalDialog() {
    showDialog(
      context: context,
      builder: (context) => Dialog(
        backgroundColor: Colors.transparent,
        child: GlassCard(
          padding: const EdgeInsets.all(24),
          child: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Icon(
                Icons.location_on,
                color: SpatialDesignSystem.successColor,
                size: 64,
              ),
              const SizedBox(height: 16),
              Text(
                "You've Arrived!",
                style: SpatialDesignSystem.headingSmall,
              ),
              const SizedBox(height: 8),
              Text(
                "You have reached your destination.",
                style: SpatialDesignSystem.bodyMedium,
                textAlign: TextAlign.center,
              ),
              const SizedBox(height: 24),
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  SpatialButton(
                    text: "Mark as Delivered",
                    onPressed: () {
                      Navigator.pop(context);
                      Navigator.pop(context);
                    },
                    isGradient: true,
                    gradient: SpatialDesignSystem.successGradient,
                    iconData: Icons.check_circle,
                  ),
                ],
              ),
            ],
          ),
        ),
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Scaffold(
      backgroundColor: isDark
          ? SpatialDesignSystem.darkBackgroundColor
          : SpatialDesignSystem.backgroundColor,
      extendBodyBehindAppBar: true,
      appBar: AppBar(
        backgroundColor: Colors.transparent,
        elevation: 0,
        leading: IconButton(
          icon: Container(
            padding: const EdgeInsets.all(8),
            decoration: BoxDecoration(
              color: Colors.black.withOpacity(0.3),
              shape: BoxShape.circle,
            ),
            child: const Icon(
              Icons.arrow_back,
              color: Colors.white,
            ),
          ),
          onPressed: () {
            if (_isNavigating) {
              _showExitConfirmationDialog();
            } else {
              Navigator.pop(context);
            }
          },
        ),
        actions: [
          IconButton(
            icon: Container(
              padding: const EdgeInsets.all(8),
              decoration: BoxDecoration(
                color: Colors.black.withOpacity(0.3),
                shape: BoxShape.circle,
              ),
              child: Icon(
                _showDirections ? Icons.menu : Icons.directions,
                color: Colors.white,
              ),
            ),
            onPressed: () {
              setState(() {
                _showDirections = !_showDirections;
              });
            },
          ),
        ],
      ),
      body: Stack(
        children: [
          // Map View
          SizedBox(
            width: double.infinity,
            height: double.infinity,
            child: Image.asset(
              "Assets/google-map.png",
              fit: BoxFit.cover,
            ),
          ),
          
          // Direction Panel
          if (_showDirections)
            Positioned(
              top: MediaQuery.of(context).padding.top + kToolbarHeight + 16,
              left: 16,
              right: 16,
              child: GlassCard(
                padding: const EdgeInsets.all(16),
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Row(
                      mainAxisAlignment: MainAxisAlignment.spaceBetween,
                      children: [
                        Column(
                          crossAxisAlignment: CrossAxisAlignment.start,
                          children: [
                            Text(
                              "Route ${widget.routeId}",
                              style: SpatialDesignSystem.subtitleMedium.copyWith(
                                color: isDark
                                    ? SpatialDesignSystem.textDarkPrimaryColor
                                    : SpatialDesignSystem.textPrimaryColor,
                              ),
                            ),
                            const SizedBox(height: 4),
                            Text(
                              "To: 123 Nguyen Hue St, District 1",
                              style: SpatialDesignSystem.bodySmall.copyWith(
                                color: isDark
                                    ? SpatialDesignSystem.textDarkSecondaryColor
                                    : SpatialDesignSystem.textSecondaryColor,
                              ),
                            ),
                          ],
                        ),
                        Row(
                          children: [
                            IconButton(
                              icon: Icon(
                                Icons.traffic,
                                color: _showTraffic
                                    ? SpatialDesignSystem.primaryColor
                                    : (isDark
                                        ? SpatialDesignSystem.textDarkSecondaryColor
                                        : SpatialDesignSystem.textSecondaryColor),
                              ),
                              onPressed: () {
                                setState(() {
                                  _showTraffic = !_showTraffic;
                                });
                              },
                            ),
                            IconButton(
                              icon: Icon(
                                Icons.layers,
                                color: isDark
                                    ? SpatialDesignSystem.textDarkSecondaryColor
                                    : SpatialDesignSystem.textSecondaryColor,
                              ),
                              onPressed: () {
                                // Show map layers
                              },
                            ),
                          ],
                        ),
                      ],
                    ),
                    const SizedBox(height: 16),
                    Row(
                      mainAxisAlignment: MainAxisAlignment.spaceAround,
                      children: [
                        _buildInfoBox(
                          "Remaining",
                          "${_remainingDistance.toStringAsFixed(1)} km",
                          Icons.straighten,
                        ),
                        const SizedBox(width: 16),
                        _buildInfoBox(
                          "ETA",
                          _estimatedTime,
                          Icons.access_time,
                        ),
                        const SizedBox(width: 16),
                        _buildInfoBox(
                          "Weather",
                          "28°C",
                          Icons.wb_sunny,
                        ),
                      ],
                    ),
                  ],
                ),
              ),
            ),
            
          // Next Direction Card
          if (_isNavigating)
            Positioned(
              bottom: 100,
              left: 16,
              right: 16,
              child: GlassCard(
                padding: const EdgeInsets.all(16),
                gradient: LinearGradient(
                  colors: [
                    SpatialDesignSystem.primaryColor.withOpacity(0.1),
                    SpatialDesignSystem.accentColor.withOpacity(0.05),
                  ],
                  begin: Alignment.topLeft,
                  end: Alignment.bottomRight,
                ),
                child: Row(
                  children: [
                    Container(
                      padding: const EdgeInsets.all(12),
                      decoration: BoxDecoration(
                        color: SpatialDesignSystem.primaryColor.withOpacity(0.1),
                        shape: BoxShape.circle,
                      ),
                      child: Icon(
                        _routeSteps[_currentStep]['icon'],
                        color: SpatialDesignSystem.primaryColor,
                        size: 28,
                      ),
                    ),
                    const SizedBox(width: 16),
                    Expanded(
                      child: Column(
                        crossAxisAlignment: CrossAxisAlignment.start,
                        children: [
                          Text(
                            _nextDirection,
                            style: SpatialDesignSystem.subtitleSmall.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkPrimaryColor
                                  : SpatialDesignSystem.textPrimaryColor,
                            ),
                          ),
                          const SizedBox(height: 4),
                          Text(
                            "Next: ${_currentStep < _routeSteps.length - 1 ? _routeSteps[_currentStep + 1]['direction'] : 'Final destination'}",
                            style: SpatialDesignSystem.captionText.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                            ),
                          ),
                        ],
                      ),
                    ),
                    const SizedBox(width: 8),
                    Text(
                      _routeSteps[_currentStep]['distance'],
                      style: SpatialDesignSystem.bodyMedium.copyWith(
                        color: SpatialDesignSystem.primaryColor,
                        fontWeight: FontWeight.bold,
                      ),
                    ),
                  ],
                ),
              ),
            ),
        ],
      ),
      bottomNavigationBar: SafeArea(
        child: Padding(
          padding: const EdgeInsets.all(16),
          child: _isNavigating
              ? Row(
                  children: [
                    Expanded(
                      child: SpatialButton(
                        text: "Stop Navigation",
                        onPressed: _stopNavigation,
                        iconData: Icons.stop_circle,
                        isOutlined: true,
                      ),
                    ),
                    const SizedBox(width: 16),
                    Expanded(
                      child: SpatialButton(
                        text: "Call Customer",
                        onPressed: () {
                          // Call customer logic
                        },
                        iconData: Icons.phone,
                      ),
                    ),
                  ],
                )
              : SpatialButton(
                  text: "Start Navigation",
                  onPressed: _startNavigation,
                  iconData: Icons.navigation,
                  isGradient: true,
                  gradient: LinearGradient(
                    colors: [
                      SpatialDesignSystem.primaryColor,
                      SpatialDesignSystem.accentColor,
                    ],
                    begin: Alignment.centerLeft,
                    end: Alignment.centerRight,
                  ),
                ),
        ),
      ),
    );
  }
  
  Widget _buildInfoBox(String label, String value, IconData icon) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Expanded(
      child: Container(
        padding: const EdgeInsets.symmetric(vertical: 12, horizontal: 8),
        decoration: BoxDecoration(
          color: isDark
              ? Colors.white.withOpacity(0.05)
              : Colors.black.withOpacity(0.03),
          borderRadius: BorderRadius.circular(8),
          border: Border.all(
            color: isDark
                ? Colors.white.withOpacity(0.1)
                : Colors.black.withOpacity(0.05),
          ),
        ),
        child: Column(
          children: [
            Icon(
              icon,
              color: SpatialDesignSystem.primaryColor,
              size: 20,
            ),
            const SizedBox(height: 4),
            Text(
              value,
              style: SpatialDesignSystem.subtitleSmall.copyWith(
                color: isDark
                    ? SpatialDesignSystem.textDarkPrimaryColor
                    : SpatialDesignSystem.textPrimaryColor,
                fontWeight: FontWeight.w600,
              ),
            ),
            const SizedBox(height: 2),
            Text(
              label,
              style: SpatialDesignSystem.captionText.copyWith(
                color: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
              ),
            ),
          ],
        ),
      ),
    );
  }
  
  void _showExitConfirmationDialog() {
    showDialog(
      context: context,
      builder: (context) => Dialog(
        backgroundColor: Colors.transparent,
        child: GlassCard(
          padding: const EdgeInsets.all(24),
          child: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Icon(
                Icons.warning_amber,
                color: SpatialDesignSystem.warningColor,
                size: 64,
              ),
              const SizedBox(height: 16),
              Text(
                "Exit Navigation?",
                style: SpatialDesignSystem.headingSmall,
              ),
              const SizedBox(height: 8),
              Text(
                "Are you sure you want to stop navigating? Your progress will be lost.",
                style: SpatialDesignSystem.bodyMedium,
                textAlign: TextAlign.center,
              ),
              const SizedBox(height: 24),
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  SpatialButton(
                    text: "Cancel",
                    onPressed: () {
                      Navigator.pop(context);
                    },
                    isOutlined: true,
                  ),
                  SpatialButton(
                    text: "Exit",
                    onPressed: () {
                      Navigator.pop(context);
                      _stopNavigation();
                      Navigator.pop(context);
                    },
                    iconData: Icons.exit_to_app,
                  ),
                ],
              ),
            ],
          ),
        ),
      ),
    );
  }
}


