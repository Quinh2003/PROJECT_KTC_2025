import 'dart:convert';

class SocketMessage {
  final String type;
  final Map<String, dynamic> data;
  final DateTime timestamp;
  
  SocketMessage({
    required this.type,
    required this.data,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
  
  factory SocketMessage.fromJson(String jsonString) {
    final Map<String, dynamic> json = jsonDecode(jsonString);
    return SocketMessage(
      type: json['type'],
      data: json['data'],
      timestamp: json['timestamp'] != null 
          ? DateTime.parse(json['timestamp']) 
          : DateTime.now(),
    );
  }
  
  String toJson() {
    return jsonEncode({
      'type': type,
      'data': data,
      'timestamp': timestamp.toIso8601String(),
    });
  }
}


