class DriverTrackingRequest {
  final double latitude;
  final double longitude;
  final String location;
  final int statusId;
  final String notes;
  final int vehicleId;

  DriverTrackingRequest({
    required this.latitude,
    required this.longitude,
    required this.location,
    required this.statusId,
    required this.notes,
    required this.vehicleId,
  });

  Map<String, dynamic> toJson() {
    return {
      'latitude': latitude,
      'longitude': longitude,
      'location': location,
      'statusId': statusId,
      'notes': notes,
      'vehicleId': vehicleId,
    };
  }
}
