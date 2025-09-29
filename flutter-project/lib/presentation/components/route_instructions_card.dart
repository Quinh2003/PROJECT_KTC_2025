import 'package:flutter/material.dart';
import '../design/spatial_ui.dart';
import '../../domain/models/map/mapbox_route_models.dart';

class RouteInstructionsCard extends StatelessWidget {
  final List<NavigationStep> routeSteps;
  final int currentStepIndex;
  final double totalDistance;
  final double totalDuration;
  final DateTime? estimatedArrival;
  final Function(int)? onStepSelected;

  const RouteInstructionsCard({
    super.key,
    required this.routeSteps,
    this.currentStepIndex = 0,
    required this.totalDistance,
    required this.totalDuration,
    this.estimatedArrival,
    this.onStepSelected,
  });

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    if (routeSteps.isEmpty || currentStepIndex >= routeSteps.length) {
      return Container(); // Nothing to display
    }

    final currentStep = routeSteps[currentStepIndex];
    
    return Container(
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
              // Compact summary row
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  // Duration
                  _buildCompactInfo("Duration", _formatDuration(totalDuration), isDark),
                  // Distance  
                  _buildCompactInfo("Distance", _formatDistance(totalDistance), isDark),
                  // Arrival time
                  if (estimatedArrival != null)
                    _buildCompactInfo("Arrival", _formatTime(estimatedArrival!), isDark),
                ],
              ),
              
              Divider(height: 16, thickness: 0.5),
              
              // Current instruction - more compact
              Row(
                children: [
                  _getCompactManeuverIcon(currentStep.maneuverType),
                  SizedBox(width: 12),
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          currentStep.instruction,
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
                          "${_formatDistance(currentStep.distance)} • ${_formatDuration(currentStep.duration)}",
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
              
              // Compact progress indicator
              SizedBox(height: 8),
              Row(
                children: [
                  Expanded(
                    child: LinearProgressIndicator(
                      value: (currentStepIndex + 1) / routeSteps.length,
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
                    "${currentStepIndex + 1}/${routeSteps.length}",
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

  Widget _getCompactManeuverIcon(String maneuver) {
    IconData iconData;
    
    switch (maneuver) {
      case "turn":
        iconData = Icons.turn_right;
        break;
      case "continue":
        iconData = Icons.arrow_forward;
        break;
      case "merge":
        iconData = Icons.merge_type;
        break;
      case "roundabout":
        iconData = Icons.roundabout_left;
        break;
      case "exit":
        iconData = Icons.exit_to_app;
        break;
      case "arrive":
        iconData = Icons.location_on;
        break;
      default:
        iconData = Icons.arrow_forward;
    }
    
    return Container(
      padding: EdgeInsets.all(6),
      decoration: BoxDecoration(
        color: SpatialDesignSystem.primaryColor.withOpacity(0.1),
        borderRadius: BorderRadius.circular(6),
      ),
      child: Icon(
        iconData,
        color: SpatialDesignSystem.primaryColor,
        size: 18,
      ),
    );
  }

  String _formatDistance(double meters) {
    if (meters < 1000) {
      return "${meters.toInt()} m";
    } else {
      final km = meters / 1000;
      return "${km.toStringAsFixed(1)} km";
    }
  }

  String _formatDuration(double seconds) {
    final minutes = (seconds / 60).ceil();
    final hours = minutes ~/ 60;
    final remainingMinutes = minutes % 60;
    
    if (hours > 0) {
      return "$hours h $remainingMinutes min";
    } else {
      return "$minutes min";
    }
  }

  String _formatTime(DateTime time) {
    final hour = time.hour.toString().padLeft(2, '0');
    final minute = time.minute.toString().padLeft(2, '0');
    return "$hour:$minute";
  }
}
