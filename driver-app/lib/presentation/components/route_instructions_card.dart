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
    Key? key,
    required this.routeSteps,
    this.currentStepIndex = 0,
    required this.totalDistance,
    required this.totalDuration,
    this.estimatedArrival,
    this.onStepSelected,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    if (routeSteps.isEmpty || currentStepIndex >= routeSteps.length) {
      return Container(); // Nothing to display
    }

    final currentStep = routeSteps[currentStepIndex];
    
    return Padding(
      padding: const EdgeInsets.all(16.0),
      child: Card(
        elevation: 4,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(12),
        ),
        color: isDark ? SpatialDesignSystem.darkSurfaceColor : Colors.white,
        child: Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            mainAxisSize: MainAxisSize.min,
            children: [
              // Summary row
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  // Duration
                  Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(
                        "Duration",
                        style: TextStyle(
                          color: isDark
                              ? SpatialDesignSystem.textDarkSecondaryColor
                              : SpatialDesignSystem.textSecondaryColor,
                        ),
                      ),
                      Text(
                        _formatDuration(totalDuration),
                        style: TextStyle(
                          fontWeight: FontWeight.bold,
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                        ),
                      ),
                    ],
                  ),
                  
                  // Distance
                  Column(
                    crossAxisAlignment: CrossAxisAlignment.center,
                    children: [
                      Text(
                        "Distance",
                        style: TextStyle(
                          color: isDark
                              ? SpatialDesignSystem.textDarkSecondaryColor
                              : SpatialDesignSystem.textSecondaryColor,
                        ),
                      ),
                      Text(
                        _formatDistance(totalDistance),
                        style: TextStyle(
                          fontWeight: FontWeight.bold,
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                        ),
                      ),
                    ],
                  ),
                  
                  // Arrival time
                  if (estimatedArrival != null)
                    Column(
                      crossAxisAlignment: CrossAxisAlignment.end,
                      children: [
                        Text(
                          "Arrival",
                          style: TextStyle(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                          ),
                        ),
                        Text(
                          _formatTime(estimatedArrival!),
                          style: TextStyle(
                            fontWeight: FontWeight.bold,
                            color: isDark
                                ? SpatialDesignSystem.textDarkPrimaryColor
                                : SpatialDesignSystem.textPrimaryColor,
                          ),
                        ),
                      ],
                    ),
                ],
              ),
              
              Divider(height: 24),
              
              // Current instruction
              Text(
                "Next Step",
                style: TextStyle(
                  fontSize: 14,
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              SizedBox(height: 8),
              Row(
                children: [
                  _getManeuverIcon(currentStep.maneuverType),
                  SizedBox(width: 12),
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          currentStep.instruction,
                          style: TextStyle(
                            fontSize: 16,
                            fontWeight: FontWeight.bold,
                            color: isDark
                                ? SpatialDesignSystem.textDarkPrimaryColor
                                : SpatialDesignSystem.textPrimaryColor,
                          ),
                        ),
                        SizedBox(height: 4),
                        Text(
                          "${_formatDistance(currentStep.distance)} • ${_formatDuration(currentStep.duration)}",
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
              
              // Progress indicator
              SizedBox(height: 16),
              Text(
                "Progress",
                style: TextStyle(
                  fontSize: 14,
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              SizedBox(height: 8),
              LinearProgressIndicator(
                value: (currentStepIndex + 1) / routeSteps.length,
                backgroundColor: isDark
                    ? SpatialDesignSystem.darkSurfaceColorSecondary
                    : SpatialDesignSystem.lightGrey,
                valueColor: AlwaysStoppedAnimation<Color>(
                  SpatialDesignSystem.primaryColor,
                ),
              ),
              SizedBox(height: 4),
              Text(
                "Step ${currentStepIndex + 1} of ${routeSteps.length}",
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
      ),
    );
  }

  Widget _getManeuverIcon(String maneuver) {
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
      padding: EdgeInsets.all(8),
      decoration: BoxDecoration(
        color: SpatialDesignSystem.primaryColor.withOpacity(0.1),
        borderRadius: BorderRadius.circular(8),
      ),
      child: Icon(
        iconData,
        color: SpatialDesignSystem.primaryColor,
        size: 24,
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
