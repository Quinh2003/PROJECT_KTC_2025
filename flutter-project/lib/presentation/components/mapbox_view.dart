import 'dart:async';
import 'package:flutter/material.dart';
import 'package:mapbox_maps_flutter/mapbox_maps_flutter.dart';

class MapboxMapView extends StatefulWidget {
  final Function(MapboxMap) onMapCreated;
  final CameraOptions? initialCameraOptions;
  final bool isLoading;
  final String? errorMessage;
  final VoidCallback? onRetry;
  final List<Position>? routeCoordinates;
  final Position? currentLocation;
  final String? routeId;
  final Function(CameraState)? onCameraMove; // Thêm callback cho sự kiện di chuyển camera
  final VoidCallback? onCameraIdle; // Thêm callback cho sự kiện camera ngừng di chuyển

  const MapboxMapView({
    super.key,
    required this.onMapCreated,
    this.initialCameraOptions,
    this.isLoading = false,
    this.errorMessage,
    this.onRetry,
    this.routeCoordinates,
    this.currentLocation,
    this.routeId,
    this.onCameraMove,
    this.onCameraIdle,
  });

  @override
  State<MapboxMapView> createState() => _MapboxMapViewState();
}

class _MapboxMapViewState extends State<MapboxMapView> {
  MapboxMap? _mapboxMap;
  
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
            key: ValueKey("route_map_${widget.routeId ?? 'default'}"),
            onMapCreated: _handleMapCreated,
            cameraOptions: widget.initialCameraOptions ?? CameraOptions(
              center: Point(
                coordinates: widget.currentLocation ?? 
                  Position(108.2022, 16.0544), // Da Nang center
              ),
              zoom: 9.0, // Zoom out to see more area
              bearing: 0,
              pitch: 45, // Add some pitch for better 3D view
            ),
            styleUri: MapboxStyles.SATELLITE_STREETS, // Use satellite view
            mapOptions: MapOptions(
              pixelRatio: MediaQuery.of(context).devicePixelRatio,
            ),
            textureView: true, // Use texture view for better performance
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
  
  // Xử lý sự kiện khởi tạo bản đồ
  void _handleMapCreated(MapboxMap mapboxMap) async {
    try {
      _mapboxMap = mapboxMap;
      
      // Thiết lập các sự kiện camera
      _setupCameraEvents(mapboxMap);
      
      print("✅ Mapbox map created successfully");
      
      // Gọi callback cho parent widget
      widget.onMapCreated(mapboxMap);
    } catch (e) {
      print("❌ Error initializing Mapbox map: $e");
      
      // Still attempt to call the callback to notify parent
      try {
        widget.onMapCreated(mapboxMap);
      } catch (callbackError) {
        print("❌ Error calling onMapCreated callback: $callbackError");
      }
    }
  }
  
  // Thiết lập các sự kiện camera
  void _setupCameraEvents(MapboxMap mapboxMap) {
    // Theo dõi các thay đổi camera bằng cách kiểm tra trạng thái camera định kỳ
    Timer? cameraMonitorTimer;
    cameraMonitorTimer = Timer.periodic(const Duration(milliseconds: 100), (timer) async {
      if (!mounted) {
        timer.cancel();
        return;
      }
      
      try {
        // Lấy trạng thái camera hiện tại
        final cameraState = await mapboxMap.getCameraState();
        
        // Gọi callback nếu có
        if (widget.onCameraMove != null) {
          widget.onCameraMove!(cameraState);
        }
      } catch (e) {
        // Bỏ qua lỗi khi không thể lấy trạng thái camera
      }
    });
    
    // Sử dụng mapboxMap.gestures để phát hiện khi nào người dùng tương tác với bản đồ
    mapboxMap.gestures.getSettings().then((gestureSettings) async {
      // Thiết lập callback khi kết thúc thao tác
      final updatedSettings = gestureSettings;
      
      // Lưu lại các cài đặt cử chỉ
      mapboxMap.gestures.updateSettings(updatedSettings);
      
      // Thiết lập timer để phát hiện khi nào camera ngừng di chuyển
      Timer.periodic(const Duration(milliseconds: 300), (timer) async {
        if (!mounted) {
          timer.cancel();
          cameraMonitorTimer?.cancel();
          return;
        }
        
        try {
          final isGestureInProgress = await mapboxMap.isGestureInProgress();
          final isAnimationInProgress = await mapboxMap.isUserAnimationInProgress();
          
          // Nếu không có thao tác hoặc hoạt ảnh nào đang diễn ra, gọi callback onCameraIdle
          if (!isGestureInProgress && !isAnimationInProgress) {
            if (widget.onCameraIdle != null) {
              widget.onCameraIdle!();
            }
          }
        } catch (e) {
          // Bỏ qua lỗi khi kiểm tra trạng thái cử chỉ
        }
      });
    });
    
    print("✅ Camera event listeners registered");
  }
}
