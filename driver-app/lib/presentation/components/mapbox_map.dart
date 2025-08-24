import 'package:flutter/material.dart';
import 'package:mapbox_maps_flutter/mapbox_maps_flutter.dart';

class MapboxMapView extends StatefulWidget {
  final Function(MapboxMap) onMapCreated;
  final CameraOptions? initialCameraOptions;
  final bool isLoading;
  final String? errorMessage;
  final VoidCallback? onRetry;

  const MapboxMapView({
    super.key,
    required this.onMapCreated,
    this.initialCameraOptions,
    this.isLoading = false,
    this.errorMessage,
    this.onRetry,
  });

  @override
  State<MapboxMapView> createState() => _MapboxMapViewState();
}

class _MapboxMapViewState extends State<MapboxMapView> {
  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    // If there's an error, show error message with retry button
    if (widget.errorMessage != null) {
      return Center(
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Icon(
              Icons.error_outline,
              color: Colors.red,
              size: 48,
            ),
            SizedBox(height: 16),
            Text(
              widget.errorMessage!,
              textAlign: TextAlign.center,
              style: TextStyle(
                color: isDark ? Colors.white : Colors.black87,
              ),
            ),
            SizedBox(height: 16),
            if (widget.onRetry != null)
              ElevatedButton(
                onPressed: widget.onRetry,
                child: Text("Retry"),
              ),
          ],
        ),
      );
    }

    return Stack(
      children: [
        ClipRRect(
          borderRadius: BorderRadius.circular(12),
          child: MapWidget(
            onMapCreated: widget.onMapCreated,
            cameraOptions: widget.initialCameraOptions,
          ),
        ),
        // Loading overlay
        if (widget.isLoading)
          Container(
            color: Colors.black.withOpacity(0.5),
            child: Center(
              child: Column(
                mainAxisSize: MainAxisSize.min,
                children: [
                  CircularProgressIndicator(),
                  SizedBox(height: 16),
                  Text(
                    "Loading map...",
                    style: TextStyle(color: Colors.white),
                  ),
                ],
              ),
            ),
          ),
      ],
    );
  }
}
