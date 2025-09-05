import 'dart:io';

/// Class giúp đọc token từ file .env
class SecretLoader {
  static final SecretLoader _singleton = SecretLoader._internal();
  Map<String, String> _secrets = {};
  bool _loaded = false;

  factory SecretLoader() {
    return _singleton;
  }

  SecretLoader._internal();

  Future<void> load() async {
    if (_loaded) return;
    
    try {
      final envFile = File('.env');
      
      if (await envFile.exists()) {
        final lines = await envFile.readAsLines();
        
        for (final line in lines) {
          if (line.trim().isEmpty || line.trim().startsWith('#')) continue;
          
          final parts = line.split('=');
          if (parts.length >= 2) {
            final key = parts[0].trim();
            final value = parts.sublist(1).join('=').trim();
            _secrets[key] = value;
          }
        }
        
        print('✅ Loaded environment variables from .env file');
      } else {
        print('⚠️ .env file not found, using default tokens');
      }
    } catch (e) {
      print('❌ Error loading .env file: $e');
    }
    
    _loaded = true;
  }

  String get(String key, {String defaultValue = ''}) {
    return _secrets[key] ?? defaultValue;
  }
}

/// Class chứa các token bảo mật
class Secrets {
  static Future<String> getMapboxAccessToken() async {
    await SecretLoader().load();
    
    // Trả về token từ file .env, nếu không có thì dùng token mặc định
    return SecretLoader().get(
      'MAPBOX_ACCESS_TOKEN',
      defaultValue: 'sk.eyJ1IjoiZmFuZ2xlZSIsImEiOiJjbWVwZGhyd2wwMWMwMmpzYm9yMmY2b2Y2In0.Z3uN0URqPxOD71hyFKP9_w'
    );
  }
}
