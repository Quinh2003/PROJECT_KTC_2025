import '../data/env/environment.dart';
import '../data/local_secure/secure_storage.dart';
import '../data/network/http_client.dart';
import '../domain/models/maintenance/maintenance_request.dart';

/// MaintenanceServices - Handles maintenance-related API operations for drivers
/// such as creating maintenance requests, viewing maintenance history, and updating status.
class MaintenanceServices {
  // Dependencies
  final Environment _env = Environment.getInstance();
  final SecureStorageFrave secureStorage = SecureStorageFrave();
  late final HttpClient _httpClient;

  /// Base URL for API requests
  String get baseUrl => _env.apiBaseUrl;

  /// Constructor
  MaintenanceServices() {
    _httpClient = HttpClient(baseUrl: baseUrl, secureStorage: secureStorage);
  }

  /// Retrieves the driver ID from secure storage
  ///
  /// Returns null if driver ID is not found
  Future<int?> _getDriverId() async {
    final driverId = await secureStorage.readDriverId();

    if (driverId == null || driverId.isEmpty || driverId == "0") {
      // Fallback: Try to use userId if available
      final userId = await secureStorage.readUserId();
      if (userId != null && userId.isNotEmpty && userId != "0") {
        try {
          final parsedId = int.parse(userId);
          // Save it as driverId for future use
          await secureStorage.persistentDriverId(userId);
          return parsedId;
        } catch (e) {
          return null;
        }
      }
      return null;
    }

    try {
      return int.parse(driverId);
    } catch (e) {
      return null;
    }
  }

  /// Step 1: Driver creates a maintenance request
  ///
  /// [vehicleId] ID of the vehicle requiring maintenance
  /// [description] Detailed description of the maintenance issue
  /// [maintenanceType] Type of maintenance (ROUTINE, EMERGENCY, etc.)
  /// [notes] Optional additional notes
  ///
  /// Returns the created [MaintenanceRequest]
  /// Throws exception if driver ID is not found or if API request fails
  Future<MaintenanceRequest> createMaintenanceRequest({
    required int vehicleId,
    required String description,
    required String maintenanceType,
    String? notes,
  }) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }

    try {
      final createDto = {
        'vehicleId': vehicleId,
        'description': description,
        'maintenanceType': maintenanceType,
        'statusId': 51, // MAINTENANCE_PENDING
        if (notes != null) 'notes': notes,
      };

      final response = await _httpClient.post(
        '/drivers/$driverId/maintenance-requests/emergency',
        body: createDto,
        timeout: const Duration(seconds: 30),
      );

      final responseData = response is Map<String, dynamic> ? response : {};
      final data =
          responseData['data'] as Map<String, dynamic>? ?? responseData;
      return MaintenanceRequest.fromJson(data as Map<String, dynamic>);
    } catch (e) {
      throw Exception('Failed to create maintenance request: $e');
    }
  }

  /// Step 4: Driver views their maintenance requests
  ///
  /// [page] Page number for pagination (starts at 0)
  /// [size] Number of items per page
  /// [status] Optional filter by status
  /// [maintenanceType] Optional filter by maintenance type
  ///
  /// Returns a list of [MaintenanceRequest] objects
  /// Throws exception if driver ID is not found or if API request fails
  Future<List<MaintenanceRequest>> getDriverMaintenanceRequests({
    int page = 0,
    int size = 10,
    String? status,
    String? maintenanceType,
    bool useCache = true,
  }) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }

    try {
      final Map<String, String> queryParams = {
        'page': page.toString(),
        'size': size.toString(),
      };

      if (status != null) {
        queryParams['status'] = status;
      }
      if (maintenanceType != null) {
        queryParams['maintenanceType'] = maintenanceType;
      }

      final response = await _httpClient.get(
        '/drivers/$driverId/maintenance-requests',
        queryParams: queryParams,
        useCache: useCache,
        timeout: const Duration(seconds: 30),
      );

      final responseData = response is Map<String, dynamic> ? response : {};
      final data =
          responseData['data'] as Map<String, dynamic>? ?? responseData;
      final content = data['content'] as List<dynamic>? ?? [];

      return content
          .map((json) =>
              MaintenanceRequest.fromJson(json as Map<String, dynamic>))
          .toList();
    } catch (e) {
      throw Exception('Failed to fetch driver maintenance requests: $e');
    }
  }

  /// Driver views details of a specific maintenance request
  ///
  /// [maintenanceId] ID of the maintenance request
  ///
  /// Returns a [MaintenanceRequest] object with complete details
  /// Throws exception if driver ID is not found or if API request fails
  Future<MaintenanceRequest> getMaintenanceRequestDetail({
    required int maintenanceId,
  }) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }

    try {
      final response = await _httpClient.get(
        '/drivers/$driverId/maintenance-requests/$maintenanceId',
        timeout: const Duration(seconds: 30),
      );

      final responseData = response is Map<String, dynamic> ? response : {};
      final data =
          responseData['data'] as Map<String, dynamic>? ?? responseData;
      return MaintenanceRequest.fromJson(data as Map<String, dynamic>);
    } catch (e) {
      throw Exception('Failed to fetch maintenance request detail: $e');
    }
  }

  /// Step 6: Driver picks up vehicle after maintenance completion or cancels maintenance request
  ///
  /// [maintenanceId] ID of the maintenance request
  /// [notes] Optional additional notes
  ///
  /// Returns the updated [MaintenanceRequest]
  /// Throws exception if driver ID is not found or if API request fails
  ///
  /// This method updates the status to IN_USE (18), which can be used both for
  /// picking up vehicle after maintenance or canceling a maintenance request
  Future<MaintenanceRequest> pickUpVehicle({
    required int maintenanceId,
    String? notes,
  }) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }

    try {
      String url = '/maintenance-requests/$maintenanceId';

      final body = {
        'statusId': 18,
        if (notes != null) 'notes': notes,
      };

      final response = await _httpClient.put(url, body: body);

      final responseData = response is Map<String, dynamic> ? response : {};
      final data =
          responseData['data'] as Map<String, dynamic>? ?? responseData;
      return MaintenanceRequest.fromJson(data as Map<String, dynamic>);
    } catch (e) {
      throw Exception('Failed to update maintenance status: $e');
    }
  }

  /// Get maintenance request counts by status for driver dashboard
  ///
  /// Returns a Map with status counts (pending, scheduled, in-progress, completed)
  /// Throws exception if driver ID is not found or if API request fails
  Future<Map<String, int>> getMaintenanceStatusCounts() async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      throw Exception('Driver ID not found');
    }

    try {
      final response = await _httpClient.get(
        '/drivers/$driverId/maintenance-requests/counts',
        timeout: const Duration(seconds: 30),
      );

      final responseData = response is Map<String, dynamic> ? response : {};
      final data =
          responseData['data'] as Map<String, dynamic>? ?? responseData;

      return {
        'pending': data['pending'] as int? ?? 0,
        'scheduled': data['scheduled'] as int? ?? 0,
        'inProgress': data['inProgress'] as int? ?? 0,
        'completed': data['completed'] as int? ?? 0,
      };
    } catch (e) {
      throw Exception('Failed to fetch maintenance status counts: $e');
    }
  }
}
