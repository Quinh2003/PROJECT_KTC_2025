import 'dart:async';
import 'dart:developer' as developer;
import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';

/// Performance monitoring utility for tracking app performance
class PerformanceMonitor {
  static final PerformanceMonitor _instance = PerformanceMonitor._internal();
  factory PerformanceMonitor() => _instance;
  PerformanceMonitor._internal();

  final Map<String, Stopwatch> _timers = {};
  final List<PerformanceMetric> _metrics = [];
  Timer? _memoryTimer;
  
  bool _isEnabled = kDebugMode;

  /// Enable or disable performance monitoring
  void setEnabled(bool enabled) {
    _isEnabled = enabled;
    if (!enabled) {
      _stopMemoryMonitoring();
    }
  }

  /// Start timing an operation
  void startTimer(String operation) {
    if (!_isEnabled) return;
    
    _timers[operation] = Stopwatch()..start();
    developer.log('⏱️ Started timing: $operation', name: 'Performance');
  }

  /// Stop timing an operation and record the metric
  void stopTimer(String operation) {
    if (!_isEnabled) return;
    
    final timer = _timers[operation];
    if (timer != null) {
      timer.stop();
      final duration = timer.elapsedMilliseconds;
      
      _recordMetric(PerformanceMetric(
        operation: operation,
        duration: duration,
        timestamp: DateTime.now(),
        type: MetricType.timing,
      ));
      
      _timers.remove(operation);
      
      // Log performance warning for slow operations
      if (duration > 100) {
        developer.log('⚠️ Slow operation: $operation took ${duration}ms', 
                     name: 'Performance', level: 900);
      } else {
        developer.log('✅ Completed: $operation in ${duration}ms', 
                     name: 'Performance');
      }
    }
  }

  /// Record a custom metric
  void recordMetric(String name, double value, {MetricType type = MetricType.custom}) {
    if (!_isEnabled) return;
    
    _recordMetric(PerformanceMetric(
      operation: name,
      value: value,
      timestamp: DateTime.now(),
      type: type,
    ));
  }

  /// Record frame drop
  void recordFrameDrop(int droppedFrames) {
    if (!_isEnabled) return;
    
    _recordMetric(PerformanceMetric(
      operation: 'frame_drops',
      value: droppedFrames.toDouble(),
      timestamp: DateTime.now(),
      type: MetricType.frameDrops,
    ));
    
    if (droppedFrames > 5) {
      developer.log('🎯 Frame drops detected: $droppedFrames frames', 
                   name: 'Performance', level: 900);
    }
  }

  /// Start memory monitoring
  void startMemoryMonitoring({Duration interval = const Duration(seconds: 30)}) {
    if (!_isEnabled) return;
    
    _memoryTimer?.cancel();
    _memoryTimer = Timer.periodic(interval, (_) => _checkMemoryUsage());
    developer.log('📊 Started memory monitoring', name: 'Performance');
  }

  /// Stop memory monitoring
  void _stopMemoryMonitoring() {
    _memoryTimer?.cancel();
    _memoryTimer = null;
  }

  /// Check current memory usage
  Future<void> _checkMemoryUsage() async {
    try {
      const platform = MethodChannel('dev.flutter.pigeon.MemoryInfoApi');
      final Map<dynamic, dynamic>? result = await platform.invokeMethod('getMemoryInfo');
      
      if (result != null) {
        final memoryUsage = result['totalPss'] as int? ?? 0;
        
        _recordMetric(PerformanceMetric(
          operation: 'memory_usage',
          value: memoryUsage / 1024 / 1024, // Convert to MB
          timestamp: DateTime.now(),
          type: MetricType.memory,
        ));

        // Warn if memory usage is high (>200MB for mobile)
        if (memoryUsage > 200 * 1024 * 1024) {
          developer.log('🚨 High memory usage: ${memoryUsage ~/ 1024 ~/ 1024}MB', 
                       name: 'Performance', level: 1000);
        }
      }
    } catch (e) {
      // Platform-specific memory monitoring may not be available
      developer.log('Memory monitoring not available: $e', name: 'Performance');
    }
  }

  /// Record a performance metric
  void _recordMetric(PerformanceMetric metric) {
    _metrics.add(metric);
    
    // Keep only last 1000 metrics to prevent memory growth
    if (_metrics.length > 1000) {
      _metrics.removeRange(0, _metrics.length - 1000);
    }
  }

  /// Get performance report
  PerformanceReport getReport({Duration? period}) {
    final now = DateTime.now();
    final cutoff = period != null ? now.subtract(period) : DateTime(2000);
    
    final relevantMetrics = _metrics.where(
      (metric) => metric.timestamp.isAfter(cutoff)
    ).toList();

    return PerformanceReport(
      totalMetrics: relevantMetrics.length,
      averageTimings: _calculateAverageTimings(relevantMetrics),
      slowOperations: _getSlowOperations(relevantMetrics),
      frameDropEvents: _getFrameDropEvents(relevantMetrics),
      memoryUsage: _getMemoryMetrics(relevantMetrics),
      period: period ?? now.difference(DateTime(2000)),
    );
  }

  Map<String, double> _calculateAverageTimings(List<PerformanceMetric> metrics) {
    final Map<String, List<double>> timings = {};
    
    for (final metric in metrics) {
      if (metric.type == MetricType.timing && metric.duration != null) {
        timings.putIfAbsent(metric.operation, () => []);
        timings[metric.operation]!.add(metric.duration!.toDouble());
      }
    }
    
    return timings.map((operation, durations) => MapEntry(
      operation,
      durations.reduce((a, b) => a + b) / durations.length,
    ));
  }

  List<PerformanceMetric> _getSlowOperations(List<PerformanceMetric> metrics) {
    return metrics
        .where((metric) => 
            metric.type == MetricType.timing && 
            (metric.duration ?? 0) > 100)
        .toList()
      ..sort((a, b) => (b.duration ?? 0).compareTo(a.duration ?? 0));
  }

  List<PerformanceMetric> _getFrameDropEvents(List<PerformanceMetric> metrics) {
    return metrics
        .where((metric) => metric.type == MetricType.frameDrops)
        .toList();
  }

  List<PerformanceMetric> _getMemoryMetrics(List<PerformanceMetric> metrics) {
    return metrics
        .where((metric) => metric.type == MetricType.memory)
        .toList();
  }

  /// Clear all metrics
  void clearMetrics() {
    _metrics.clear();
    developer.log('🧹 Cleared performance metrics', name: 'Performance');
  }

  /// Print performance report to console
  void printReport({Duration? period}) {
    final report = getReport(period: period);
    
    developer.log('''
📊 Performance Report (${report.period.inMinutes} minutes)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📈 Total Metrics: ${report.totalMetrics}

⏱️ Average Timings:
${report.averageTimings.entries.map((e) => '  • ${e.key}: ${e.value.toStringAsFixed(1)}ms').join('\n')}

🐌 Slow Operations (>100ms):
${report.slowOperations.take(5).map((m) => '  • ${m.operation}: ${m.duration}ms').join('\n')}

🎯 Frame Drops:
${report.frameDropEvents.length} events recorded

💾 Memory Usage:
${report.memoryUsage.isNotEmpty ? '  • Latest: ${report.memoryUsage.last.value?.toStringAsFixed(1)}MB' : '  • No data available'}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    ''', name: 'Performance');
  }
}

/// Performance metric data class
class PerformanceMetric {
  final String operation;
  final int? duration;
  final double? value;
  final DateTime timestamp;
  final MetricType type;

  PerformanceMetric({
    required this.operation,
    this.duration,
    this.value,
    required this.timestamp,
    required this.type,
  });
}

/// Types of performance metrics
enum MetricType {
  timing,
  memory,
  frameDrops,
  custom,
}

/// Performance report data class
class PerformanceReport {
  final int totalMetrics;
  final Map<String, double> averageTimings;
  final List<PerformanceMetric> slowOperations;
  final List<PerformanceMetric> frameDropEvents;
  final List<PerformanceMetric> memoryUsage;
  final Duration period;

  PerformanceReport({
    required this.totalMetrics,
    required this.averageTimings,
    required this.slowOperations,
    required this.frameDropEvents,
    required this.memoryUsage,
    required this.period,
  });
}

/// Global performance monitor instance
final performanceMonitor = PerformanceMonitor();