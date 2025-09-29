import 'dart:io';

class DeliveryProof {
  final int? id;
  final int orderId;
  final String proofType;
  final String? imageUrl;
  final File? imageFile;
  final String? recipientName;
  final String? recipientSignature;
  final String? notes;
  final DateTime timestamp;

  DeliveryProof({
    this.id,
    required this.orderId,
    required this.proofType,
    this.imageUrl,
    this.imageFile,
    this.recipientName,
    this.recipientSignature,
    this.notes,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();

  // Factory constructor to create from JSON
  factory DeliveryProof.fromJson(Map<String, dynamic> json) {
    // Lấy orderId từ trường order.id
    int? orderId;
    if (json['order'] != null && json['order'] is Map<String, dynamic>) {
      orderId = json['order']['id'] as int?;
    }

    // Lấy imageUrl từ filePath nếu có
    String? imageUrl;
    if (json['filePath'] != null) {
      imageUrl = json['filePath'];
    } else {
      imageUrl = json['imageUrl'];
    }
    
    // Ensure imageUrl is usable (not a local server file path)
    if (imageUrl != null && imageUrl.startsWith('file:///')) {
      // Convert to a relative path that can be combined with base URL later
      imageUrl = imageUrl.substring(8); // Remove 'file:///'
    }

    // Trả về đối tượng DeliveryProof
    return DeliveryProof(
      id: json['id'],
      orderId: orderId ?? 0, // Sử dụng giá trị mặc định nếu null
      proofType: json['proofType'] ?? 'PHOTO',
      imageUrl: imageUrl,
      recipientName: json['recipientName'],
      recipientSignature: json['recipientSignature'],
      notes: json['notes'],
      // capturedAt là trường timestamp trong response
      timestamp: json['capturedAt'] != null 
          ? DateTime.parse(json['capturedAt']) 
          : (json['timestamp'] != null 
              ? DateTime.parse(json['timestamp']) 
              : DateTime.now()),
    );
  }

  // Convert to JSON
  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'orderId': orderId,
      'proofType': proofType,
      'imageUrl': imageUrl,
      'recipientName': recipientName,
      'recipientSignature': recipientSignature,
      'notes': notes,
      'timestamp': timestamp.toIso8601String(),
    };
  }

  // Create a copy with updated fields
  DeliveryProof copyWith({
    int? id,
    int? orderId,
    String? proofType,
    String? imageUrl,
    File? imageFile,
    String? recipientName,
    String? recipientSignature,
    String? notes,
    DateTime? timestamp,
  }) {
    return DeliveryProof(
      id: id ?? this.id,
      orderId: orderId ?? this.orderId,
      proofType: proofType ?? this.proofType,
      imageUrl: imageUrl ?? this.imageUrl,
      imageFile: imageFile ?? this.imageFile,
      recipientName: recipientName ?? this.recipientName,
      recipientSignature: recipientSignature ?? this.recipientSignature,
      notes: notes ?? this.notes,
      timestamp: timestamp ?? this.timestamp,
    );
  }
}