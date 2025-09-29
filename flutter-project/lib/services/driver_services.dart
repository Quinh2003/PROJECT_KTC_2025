import '../data/env/environment.dart';
import '../data/local_secure/secure_storage.dart';
import '../data/network/http_client.dart';
import '../domain/models/driver/driver_profile.dart';
import '../domain/models/analytics/driver_analytics.dart';

/// DriverServices - Handles driver-specific API operations such as profile management,
/// analytics, and status updates. This service does not handle location tracking.
class DriverServices {
  // Dependencies
  final Environment _env = Environment.getInstance();
  final SecureStorageFrave secureStorage = SecureStorageFrave();
  late final HttpClient _httpClient;
  
  /// Base URL for API requests
  String get baseUrl => _env.apiBaseUrl;
  
  /// Private constructor for singleton pattern
  DriverServices() {
    _httpClient = HttpClient(baseUrl: baseUrl, secureStorage: secureStorage);
  }

  /// Retrieves the driver ID from secure storage
  /// 
  /// Returns null if driver ID is not found
  Future<int?> _getDriverId() async {
    final driverId = await secureStorage.readDriverId();
    if (driverId == null) {
      return null;
    }
    return int.parse(driverId);
  }

  /// Retrieves driver analytics data for a specified date range
  /// 
  /// [startDate] Optional start date in format YYYY-MM-DD
  /// [endDate] Optional end date in format YYYY-MM-DD
  /// 
  /// Returns [DriverAnalytics] object with analytics data
  /// Throws exception if driver ID is not found or if API request fails
  Future<DriverAnalytics> getDriverAnalytics({
    String? startDate,
    String? endDate,
  }) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }
    
    // Add query parameters if provided
    final queryParams = <String, String>{};
    if (startDate != null) queryParams['startDate'] = startDate;
    if (endDate != null) queryParams['endDate'] = endDate;
    
    try {
      final data = await _httpClient.get<Map<String, dynamic>>(
        '/drivers/$driverId/analytics',
        queryParams: queryParams,
      );
      return DriverAnalytics.fromJson(data);
    } catch (e) {
      // Fallback to legacy API
      return _getDriverAnalyticsLegacy();
    }
  }
  
  /// Legacy method for retrieving driver analytics (compatibility with old API)
  /// 
  /// This is a fallback method used when the new API endpoint fails
  Future<DriverAnalytics> _getDriverAnalyticsLegacy() async {
    final token = await secureStorage.readToken();
    
    try {
      print('🚛 DriverService: Getting analytics via HttpClient...');

      final response = await _httpClient.get<Map<String, dynamic>>(
        '/dashboard/driver-analytics',
        useCache: false,
        timeout: const Duration(seconds: 45),
      );

      print('🚛 DriverService: Analytics response received');
      
      if (response is Map<String, dynamic>) {
        return DriverAnalytics.fromJson(response);
      } else if (response is Map && response.containsKey('data')) {
        return DriverAnalytics.fromJson(response['data']);
      }
      
      throw Exception('Invalid analytics response format');
    } catch (e) {
      print('❌ DriverService: Error getting analytics: $e');
      throw Exception('Failed to load driver analytics: $e');
    }
  }

  /// Retrieves the driver's profile information
  /// 
  /// Returns [DriverProfile] object with driver data
  /// Throws exception if driver ID is not found or if API request fails
  Future<DriverProfile> getDriverProfile() async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }
    
    try {
      final data = await _httpClient.get<Map<String, dynamic>>(
        '/drivers/$driverId',
      );
      return DriverProfile.fromJson(data);
    } catch (e) {
      throw Exception('Error getting driver profile: $e');
    }
  }

  /// Updates the driver's profile information
  /// 
  /// [profile] DriverProfile object containing updated profile data
  /// 
  /// Returns updated [DriverProfile] object
  /// Throws exception if driver ID is not found or if API request fails
  Future<DriverProfile> updateDriverProfile(DriverProfile profile) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }
    
    try {
      final data = await _httpClient.put<Map<String, dynamic>>(
        '/drivers/$driverId',
        body: profile.toJson(),
      );
      return DriverProfile.fromJson(data);
    } catch (e) {
      throw Exception('Error updating driver profile: $e');
    }
  }

  /// Uploads a driver document or image
  /// 
  /// [filePath] Path to the file to upload
  /// [documentType] Type of document (e.g., 'license', 'vehicle_photo')
  /// 
  /// Returns URL of the uploaded document
  /// Throws exception if driver ID is not found or if upload fails
  Future<String> uploadDriverDocument(String filePath, String documentType) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }
    
    try {
      return await _httpClient.uploadFile(
        '/drivers/$driverId/documents',
        filePath,
        fields: {'documentType': documentType},
      );
    } catch (e) {
      throw Exception('Error uploading document: $e');
    }
  }
  
  /// Updates the driver's status
  /// 
  /// [statusId] ID of the new status (e.g., 1: Available, 2: Busy, 3: Offline)
  /// 
  /// Returns response from the API with success or failure message
  /// Response format: {'success': bool, 'message': String}
  Future<Map<String, dynamic>> updateDriverStatus(int statusId) async {
    final token = await secureStorage.readToken();
    final driverId = await secureStorage.readDriverId();
    
    if (token == null || driverId == null) {
      return {'success': false, 'message': 'Token or driver ID not found'};
    }
    
    try {
      print('🚛 DriverService: Updating status via HttpClient...');

      final response = await _httpClient.patch<Map<String, dynamic>>(
        '/drivers/status',
        body: {'statusId': statusId},
        fromJson: (json) => json,
      );

      print('🚛 DriverService: Status update response received');
      return response;
    } catch (e) {
      return {'success': false, 'message': 'Error updating status: $e'};
    }
  }
}

/// Singleton instance of DriverServices for app-wide use
final driverServices = DriverServices();
