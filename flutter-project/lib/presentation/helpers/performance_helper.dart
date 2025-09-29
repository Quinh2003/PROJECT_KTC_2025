import 'package:flutter/foundation.dart';
import 'package:flutter/widgets.dart';

class PerformanceHelper {
  static const Duration _frameTime = Duration(milliseconds: 16); // 60 FPS target
  static final List<Duration> _frameTimes = [];
  static int _frameCount = 0;
  static DateTime? _lastFrameTime;

  /// Start measuring frame performance
  static void startFrameMonitoring() {
    if (!kDebugMode) return;
    
    WidgetsBinding.instance.addPostFrameCallback((_) {
      _measureFrame();
    });
  }

  static void _measureFrame() {
    final now = DateTime.now();
    if (_lastFrameTime != null) {
      final frameTime = now.difference(_lastFrameTime!);
      _frameTimes.add(frameTime);
      _frameCount++;

      // Keep only last 60 frames
      if (_frameTimes.length > 60) {
        _frameTimes.removeAt(0);
      }

      // Log dropped frames
      if (frameTime > _frameTime * 2) {
        debugPrint('⚠️ Frame drop detected: ${frameTime.inMilliseconds}ms');
      }

      // Print stats every 60 frames
      if (_frameCount % 60 == 0) {
        _printFrameStats();
      }
    }
    
    _lastFrameTime = now;
    
    // Schedule next measurement
    WidgetsBinding.instance.addPostFrameCallback((_) {
      _measureFrame();
    });
  }

  static void _printFrameStats() {
    if (_frameTimes.isEmpty) return;

    final avgTime = _frameTimes.map((d) => d.inMicroseconds).reduce((a, b) => a + b) / _frameTimes.length;
    final maxTime = _frameTimes.map((d) => d.inMicroseconds).reduce((a, b) => a > b ? a : b);
    final droppedFrames = _frameTimes.where((d) => d > _frameTime * 1.5).length;

    debugPrint('''
📊 Frame Performance Stats:
   Average: ${(avgTime / 1000).toStringAsFixed(1)}ms
   Max: ${(maxTime / 1000).toStringAsFixed(1)}ms
   Dropped frames: $droppedFrames/60 (${(droppedFrames / 60 * 100).toStringAsFixed(1)}%)
   Target: <16.7ms (60 FPS)
''');
  }

  /// Measure build time of a widget
  static T measureBuild<T>(String widgetName, T Function() buildFunction) {
    if (!kDebugMode) return buildFunction();

    final stopwatch = Stopwatch()..start();
    final result = buildFunction();
    stopwatch.stop();

    if (stopwatch.elapsedMilliseconds > 5) {
      debugPrint('🐌 Slow build detected: $widgetName took ${stopwatch.elapsedMilliseconds}ms');
    }

    return result;
  }

  /// Measure async operation time
  static Future<T> measureAsync<T>(String operationName, Future<T> Function() operation) async {
    if (!kDebugMode) return await operation();

    final stopwatch = Stopwatch()..start();
    final result = await operation();
    stopwatch.stop();

    if (stopwatch.elapsedMilliseconds > 100) {
      debugPrint('🐌 Slow async operation: $operationName took ${stopwatch.elapsedMilliseconds}ms');
    }

    return result;
  }

  /// Track memory usage
  static void trackMemoryUsage() {
    if (!kDebugMode) return;

    // This would need additional platform channel implementation for actual memory tracking
    debugPrint('💾 Memory tracking not implemented yet');
  }

  /// Log performance tip
  static void logPerformanceTip(String tip) {
    if (!kDebugMode) return;
    debugPrint('💡 Performance tip: $tip');
  }
}