import 'dart:convert';
import 'package:flutter/foundation.dart';

enum EnvironmentType {
  development,
  staging,
  production,
}

class Environment {
  final EnvironmentType type;
  final String apiBaseUrl;
  final String socketBaseUrl;
  final bool enableLogging;
  final int locationUpdateInterval;
  final String mapApiKey;
  final Map<String, dynamic> additionalConfig;

  Environment({
    required this.type,
    required this.apiBaseUrl,
    required this.socketBaseUrl,
    this.enableLogging = false,
    this.locationUpdateInterval = 10, // seconds
    required this.mapApiKey,
    this.additionalConfig = const {},
  });

  factory Environment.development() {
    return Environment(
      type: EnvironmentType.development,
      apiBaseUrl: 'http://192.168.1.100:8080/api/v1',
      socketBaseUrl: 'ws://192.168.1.100:8080/ws',
      enableLogging: true,
      locationUpdateInterval: 15,
      mapApiKey: 'YOUR_GOOGLE_MAPS_API_KEY_DEV',
      additionalConfig: {
        'enableMockData': true,
        'mockDataDelay': 1000,
      },
    );
  }

  factory Environment.staging() {
    return Environment(
      type: EnvironmentType.staging,
      apiBaseUrl: 'https://staging-api.ktclogistics.com/api/v1',
      socketBaseUrl: 'wss://staging-api.ktclogistics.com/ws',
      enableLogging: true,
      locationUpdateInterval: 10,
      mapApiKey: 'YOUR_GOOGLE_MAPS_API_KEY_STAGING',
    );
  }

  factory Environment.production() {
    return Environment(
      type: EnvironmentType.production,
      apiBaseUrl: 'https://api.ktclogistics.com/api/v1',
      socketBaseUrl: 'wss://api.ktclogistics.com/ws',
      enableLogging: false,
      locationUpdateInterval: 5,
      mapApiKey: 'YOUR_GOOGLE_MAPS_API_KEY_PROD',
    );
  }

  static Environment fromJson(Map<String, dynamic> json) {
    return Environment(
      type: _parseEnvironmentType(json['type']),
      apiBaseUrl: json['apiBaseUrl'],
      socketBaseUrl: json['socketBaseUrl'],
      enableLogging: json['enableLogging'] ?? false,
      locationUpdateInterval: json['locationUpdateInterval'] ?? 10,
      mapApiKey: json['mapApiKey'],
      additionalConfig: json['additionalConfig'] ?? {},
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'type': type.toString(),
      'apiBaseUrl': apiBaseUrl,
      'socketBaseUrl': socketBaseUrl,
      'enableLogging': enableLogging,
      'locationUpdateInterval': locationUpdateInterval,
      'mapApiKey': mapApiKey,
      'additionalConfig': additionalConfig,
    };
  }

  static EnvironmentType _parseEnvironmentType(String? typeStr) {
    switch (typeStr) {
      case 'EnvironmentType.development':
        return EnvironmentType.development;
      case 'EnvironmentType.staging':
        return EnvironmentType.staging;
      case 'EnvironmentType.production':
        return EnvironmentType.production;
      default:
        return EnvironmentType.development;
    }
  }

  bool get isDevelopment => type == EnvironmentType.development;
  bool get isStaging => type == EnvironmentType.staging;
  bool get isProduction => type == EnvironmentType.production;

  Environment copyWith({
    EnvironmentType? type,
    String? apiBaseUrl,
    String? socketBaseUrl,
    bool? enableLogging,
    int? locationUpdateInterval,
    String? mapApiKey,
    Map<String, dynamic>? additionalConfig,
  }) {
    return Environment(
      type: type ?? this.type,
      apiBaseUrl: apiBaseUrl ?? this.apiBaseUrl,
      socketBaseUrl: socketBaseUrl ?? this.socketBaseUrl,
      enableLogging: enableLogging ?? this.enableLogging,
      locationUpdateInterval: locationUpdateInterval ?? this.locationUpdateInterval,
      mapApiKey: mapApiKey ?? this.mapApiKey,
      additionalConfig: additionalConfig ?? this.additionalConfig,
    );
  }

  @override
  String toString() {
    return jsonEncode(toJson());
  }

  dynamic getAdditionalConfig(String key, [dynamic defaultValue]) {
    return additionalConfig[key] ?? defaultValue;
  }

  void log(String message) {
    if (enableLogging) {
      debugPrint('[KTC Logistics] $message');
    }
  }
}

class EnvironmentManager {
  static Environment? _current;

  static Environment get current {
    if (_current == null) {
      // Default to development if not set
      _current = Environment.development();
    }
    return _current!;
  }

  static void setEnvironment(Environment environment) {
    _current = environment;
  }

  static void setDevelopment() {
    _current = Environment.development();
  }

  static void setStaging() {
    _current = Environment.staging();
  }

  static void setProduction() {
    _current = Environment.production();
  }
}


