import 'dart:convert';
import 'dart:io';
import 'dart:async';
import 'package:http/http.dart' as http;
import '../local_secure/secure_storage.dart';
import 'api_interceptor.dart';

class ApiException implements Exception {
  final String message;
  final int? statusCode;
  final String? body;

  ApiException(this.message, {this.statusCode, this.body});

  @override
  String toString() {
    if (statusCode != null) {
      return 'ApiException: $message (Status code: $statusCode)';
    }
    return 'ApiException: $message';
  }
}

/// Cache entry for HTTP responses
class CacheEntry {
  final dynamic data;
  final DateTime expiresAt;

  CacheEntry({required this.data, required this.expiresAt});

  bool get isExpired => DateTime.now().isAfter(expiresAt);
}

/// HTTP client with authentication, caching, and error handling
class HttpClient {
  final String baseUrl;
  final SecureStorageFrave secureStorage;
  final http.Client _client;
  final ApiInterceptor _apiInterceptor;
  
  // Cache for GET requests (simple in-memory cache)
  final Map<String, CacheEntry> _cache = {};
  static const Duration _defaultCacheDuration = Duration(minutes: 5);
  static const Duration _defaultTimeout = Duration(seconds: 30);

  HttpClient({
    required this.baseUrl,
    required this.secureStorage,
  }) : _client = http.Client(),
       _apiInterceptor = ApiInterceptor(
         baseUrl: baseUrl,
         secureStorage: secureStorage,
       );

  /// Dispose resources
  void dispose() {
    _client.close();
    _cache.clear();
  }

  /// Get authorization headers with token (cached)
  String? _cachedToken;
  DateTime? _tokenCacheTime;
  static const Duration _tokenCacheDuration = Duration(minutes: 10);

  Future<Map<String, String>> _getAuthHeaders() async {
    // Use cached token if available and not expired
    if (_cachedToken != null && 
        _tokenCacheTime != null && 
        DateTime.now().difference(_tokenCacheTime!) < _tokenCacheDuration) {
      return {
        'Content-Type': 'application/json',
        'Authorization': 'Bearer $_cachedToken',
      };
    }

    final token = await secureStorage.readToken();
    if (token == null) {
      throw ApiException('No authentication token available');
    }
    
    // Cache the token
    _cachedToken = token;
    _tokenCacheTime = DateTime.now();
    
    return {
      'Content-Type': 'application/json',
      'Authorization': 'Bearer $token',
    };
  }

  /// Check cache for GET requests
  T? _getFromCache<T>(String cacheKey) {
    final entry = _cache[cacheKey];
    if (entry != null && !entry.isExpired) {
      return entry.data as T?;
    }
    return null;
  }

  /// Store in cache
  void _setCache<T>(String cacheKey, T data, {Duration? duration}) {
    _cache[cacheKey] = CacheEntry(
      data: data,
      expiresAt: DateTime.now().add(duration ?? _defaultCacheDuration),
    );
  }

  /// Generic GET request with caching
  Future<T> get<T>(
    String endpoint, {
    Map<String, String>? queryParams,
    T Function(Map<String, dynamic> json)? fromJson,
    bool requiresAuth = true,
    bool useCache = true,
    Duration? cacheDuration,
    Duration? timeout,
  }) async {
    var url = '$baseUrl$endpoint';
    
    if (queryParams != null && queryParams.isNotEmpty) {
      url += '?${queryParams.entries.map((e) => '${e.key}=${e.value}').join('&')}';
    }

    // Check cache first for GET requests
    if (useCache) {
      final cached = _getFromCache<T>(url);
      if (cached != null) {
        return cached;
      }
    }

    final headers = requiresAuth ? await _getAuthHeaders() : {'Content-Type': 'application/json'};
    
    try {
      final response = await _client.get(
        Uri.parse(url),
        headers: headers,
      ).timeout(timeout ?? _defaultTimeout);
      
      try {
        final result = _handleResponse<T>(response, fromJson: fromJson);
        
        // Cache successful GET responses
        if (useCache && response.statusCode >= 200 && response.statusCode < 300) {
          _setCache(url, result, duration: cacheDuration);
        }
        
        return result;
      } catch (e) {
        // Nếu lỗi 401 Unauthorized, thử làm mới token và gửi lại yêu cầu
        if (e is ApiException && e.statusCode == 401) {
          // Sử dụng ApiInterceptor để làm mới token
          final newToken = await _apiInterceptor.refreshToken();
          
          if (newToken != null) {
            // Xóa token đã lưu trong cache
            _cachedToken = null;
            _tokenCacheTime = null;
            
            // Thử lại yêu cầu ban đầu với token mới
            final newHeaders = await _getAuthHeaders();
            final newResponse = await _client.get(
              Uri.parse(url),
              headers: newHeaders,
            ).timeout(timeout ?? _defaultTimeout);
            
            final result = _handleResponse<T>(newResponse, fromJson: fromJson);
            
            // Cache successful GET responses after token refresh
            if (useCache && newResponse.statusCode >= 200 && newResponse.statusCode < 300) {
              _setCache(url, result, duration: cacheDuration);
            }
            
            return result;
          }
          
          // Nếu không thể làm mới token, ném lại lỗi ban đầu
          rethrow;
        } else {
          // Không phải lỗi 401, ném lại ngoại lệ
          rethrow;
        }
      }
    } on TimeoutException {
      throw ApiException('Request timeout');
    } on SocketException {
      throw ApiException('No internet connection');
    } catch (e) {
      throw ApiException('Error performing GET request: $e');
    }
  }

  /// Generic POST request
  Future<T> post<T>(
    String endpoint, {
    dynamic body,
    T Function(Map<String, dynamic> json)? fromJson,
    bool requiresAuth = true,
    Duration? timeout,
  }) async {
    final url = '$baseUrl$endpoint';
    final headers = requiresAuth ? await _getAuthHeaders() : {'Content-Type': 'application/json'};
    
    try {
      final response = await _client.post(
        Uri.parse(url),
        headers: headers,
        body: body != null ? jsonEncode(body) : null,
      ).timeout(timeout ?? _defaultTimeout);
      
      try {
        return _handleResponse<T>(response, fromJson: fromJson);
      } catch (e) {
        // Nếu lỗi 401 Unauthorized, thử làm mới token và gửi lại yêu cầu
        if (e is ApiException && e.statusCode == 401) {
          // Sử dụng ApiInterceptor để làm mới token
          final newToken = await _apiInterceptor.refreshToken();
          
          if (newToken != null) {
            // Xóa token đã lưu trong cache
            _cachedToken = null;
            _tokenCacheTime = null;
            
            // Thử lại yêu cầu ban đầu với token mới
            final newHeaders = await _getAuthHeaders();
            final newResponse = await _client.post(
              Uri.parse(url),
              headers: newHeaders,
              body: body != null ? jsonEncode(body) : null,
            ).timeout(timeout ?? _defaultTimeout);
            
            return _handleResponse<T>(newResponse, fromJson: fromJson);
          }
          
          // Nếu không thể làm mới token, ném lại lỗi ban đầu
          rethrow;
        } else {
          // Không phải lỗi 401, ném lại ngoại lệ
          rethrow;
        }
      }
    } on TimeoutException {
      throw ApiException('Request timeout');
    } on SocketException {
      throw ApiException('No internet connection');
    } catch (e) {
      throw ApiException('Error performing POST request: $e');
    }
  }

  /// Generic PUT request
  Future<T> put<T>(
    String endpoint, {
    dynamic body,
    T Function(Map<String, dynamic> json)? fromJson,
    bool requiresAuth = true,
  }) async {
    final url = '$baseUrl$endpoint';
    final headers = requiresAuth ? await _getAuthHeaders() : {'Content-Type': 'application/json'};
    
    try {
      final response = await _client.put(
        Uri.parse(url),
        headers: headers,
        body: body != null ? jsonEncode(body) : null,
      );
      
      try {
        return _handleResponse<T>(response, fromJson: fromJson);
      } catch (e) {
        // Nếu lỗi 401 Unauthorized, thử làm mới token và gửi lại yêu cầu
        if (e is ApiException && e.statusCode == 401) {
          // Sử dụng ApiInterceptor để làm mới token
          final newToken = await _apiInterceptor.refreshToken();
          
          if (newToken != null) {
            // Xóa token đã lưu trong cache
            _cachedToken = null;
            _tokenCacheTime = null;
            
            // Thử lại yêu cầu ban đầu với token mới
            final newHeaders = await _getAuthHeaders();
            final newResponse = await _client.put(
              Uri.parse(url),
              headers: newHeaders,
              body: body != null ? jsonEncode(body) : null,
            );
            
            return _handleResponse<T>(newResponse, fromJson: fromJson);
          }
          
          // Nếu không thể làm mới token, ném lại lỗi ban đầu
          rethrow;
        } else {
          // Không phải lỗi 401, ném lại ngoại lệ
          rethrow;
        }
      }
    } on SocketException {
      throw ApiException('No internet connection');
    } catch (e) {
      throw ApiException('Error performing PUT request: $e');
    }
  }

  /// Generic PATCH request
  Future<T> patch<T>(
    String endpoint, {
    dynamic body,
    T Function(Map<String, dynamic> json)? fromJson,
    bool requiresAuth = true,
  }) async {
    final url = '$baseUrl$endpoint';
    final headers = requiresAuth ? await _getAuthHeaders() : {'Content-Type': 'application/json'};
    
    try {
      final response = await _client.patch(
        Uri.parse(url),
        headers: headers,
        body: body != null ? jsonEncode(body) : null,
      );
      
      // Log để debug
      print('🔄 HTTP Response: ${response.statusCode}');
      print('🔄 Response headers: ${response.headers}');
      if (response.body.isEmpty) {
        print('🔄 Response body is empty');
      } else {
        print('🔄 Response body: ${response.body}');
      }
      
      try {
        return _handleResponse<T>(response, fromJson: fromJson);
      } catch (e) {
        // Nếu lỗi 401 Unauthorized, thử làm mới token và gửi lại yêu cầu
        if (e is ApiException && e.statusCode == 401) {
          // Sử dụng ApiInterceptor để làm mới token
          final newToken = await _apiInterceptor.refreshToken();
          
          if (newToken != null) {
            // Xóa token đã lưu trong cache
            _cachedToken = null;
            _tokenCacheTime = null;
            
            // Thử lại yêu cầu ban đầu với token mới
            final newHeaders = await _getAuthHeaders();
            final newResponse = await _client.patch(
              Uri.parse(url),
              headers: newHeaders,
              body: body != null ? jsonEncode(body) : null,
            );
            
            return _handleResponse<T>(newResponse, fromJson: fromJson);
          }
          
          // Nếu không thể làm mới token, ném lại lỗi ban đầu
          rethrow;
        } else {
          // Không phải lỗi 401, ném lại ngoại lệ
          rethrow;
        }
      }
    } on SocketException {
      throw ApiException('No internet connection');
    } catch (e) {
      throw ApiException('Error performing PATCH request: $e');
    }
  }

  /// Generic DELETE request
  Future<T> delete<T>(
    String endpoint, {
    T Function(Map<String, dynamic> json)? fromJson,
    bool requiresAuth = true,
  }) async {
    final url = '$baseUrl$endpoint';
    final headers = requiresAuth ? await _getAuthHeaders() : {'Content-Type': 'application/json'};
    
    try {
      final response = await _client.delete(
        Uri.parse(url),
        headers: headers,
      );
      
      try {
        return _handleResponse<T>(response, fromJson: fromJson);
      } catch (e) {
        // Nếu lỗi 401 Unauthorized, thử làm mới token và gửi lại yêu cầu
        if (e is ApiException && e.statusCode == 401) {
          // Sử dụng ApiInterceptor để làm mới token
          final newToken = await _apiInterceptor.refreshToken();
          
          if (newToken != null) {
            // Xóa token đã lưu trong cache
            _cachedToken = null;
            _tokenCacheTime = null;
            
            // Thử lại yêu cầu ban đầu với token mới
            final newHeaders = await _getAuthHeaders();
            final newResponse = await _client.delete(
              Uri.parse(url),
              headers: newHeaders,
            );
            
            return _handleResponse<T>(newResponse, fromJson: fromJson);
          }
          
          // Nếu không thể làm mới token, ném lại lỗi ban đầu
          rethrow;
        } else {
          // Không phải lỗi 401, ném lại ngoại lệ
          rethrow;
        }
      }
    } on SocketException {
      throw ApiException('No internet connection');
    } catch (e) {
      throw ApiException('Error performing DELETE request: $e');
    }
  }

  /// Handle HTTP response
  T _handleResponse<T>(http.Response response, {T Function(Map<String, dynamic> json)? fromJson}) {
    if (response.statusCode >= 200 && response.statusCode < 300) {
      // Thành công với response rỗng
      if (response.body.isEmpty || response.body.trim().isEmpty) {
        // Nếu T là bool hoặc void hoặc dynamic, trả về true
        if (T == bool) {
          return true as T;
        }
        
        // Nếu chúng ta có fromJson, gọi nó với một map rỗng để tránh lỗi null
        if (fromJson != null) {
          return fromJson({});
        }
        
        // Trường hợp khác, trả về null hoặc map rỗng tùy vào kiểu T
        try {
          return {} as T; // Thử trả về map rỗng
        } catch (_) {
          return null as T; // Nếu không thể ép kiểu, trả về null
        }
      }
      
      // Xử lý body không rỗng
      if (T == bool) {
        return true as T;
      }
      
      final dynamic jsonData = json.decode(response.body);
      
      if (fromJson != null) {
        if (jsonData is Map<String, dynamic>) {
          return fromJson(jsonData);
        } else {
          throw ApiException('Invalid response format', statusCode: response.statusCode, body: response.body);
        }
      }
      
      return jsonData as T;
    } else if (response.statusCode == 401) {
      // Xóa token đã lưu trong cache để buộc lần request sau phải lấy lại token mới
      _cachedToken = null;
      _tokenCacheTime = null;
      throw ApiException('Unauthorized - Token may have expired', statusCode: response.statusCode, body: response.body);
    } else if (response.statusCode == 403) {
      throw ApiException('Forbidden', statusCode: response.statusCode, body: response.body);
    } else if (response.statusCode == 404) {
      throw ApiException('Resource not found', statusCode: response.statusCode, body: response.body);
    } else {
      throw ApiException('Request failed', statusCode: response.statusCode, body: response.body);
    }
  }
  
  /// Upload file to endpoint
  Future<String> uploadFile(
    String endpoint,
    String filePath, {
    Map<String, String>? fields,
    String fileField = 'file',
    bool requiresAuth = true,
  }) async {
    final url = '$baseUrl$endpoint';
    final request = http.MultipartRequest('POST', Uri.parse(url));
    
    if (requiresAuth) {
      final token = await secureStorage.readToken();
      if (token == null) {
        throw ApiException('No authentication token available');
      }
      request.headers.addAll({'Authorization': 'Bearer $token'});
    }
    
    if (fields != null) {
      request.fields.addAll(fields);
    }
    
    request.files.add(await http.MultipartFile.fromPath(fileField, filePath));
    
    try {
      final streamedResponse = await request.send();
      final response = await http.Response.fromStream(streamedResponse);
      
      if (response.statusCode >= 200 && response.statusCode < 300) {
        final Map<String, dynamic> data = json.decode(response.body);
        return data['fileUrl'] as String;
      } else {
        throw ApiException(
          'Failed to upload file',
          statusCode: response.statusCode,
          body: response.body,
        );
      }
    } on SocketException {
      throw ApiException('No internet connection');
    } catch (e) {
      throw ApiException('Error uploading file: $e');
    }
  }
  
  /// Close the HTTP client
  void close() {
    _client.close();
  }
}
