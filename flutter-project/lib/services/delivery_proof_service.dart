import 'dart:convert';
import 'dart:io';
import 'package:http/http.dart' as http;
import 'package:http_parser/http_parser.dart';
import 'package:ktc_logistics_driver/domain/models/delivery_proof/delivery_proof.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/data/network/http_client.dart';

class DeliveryProofService {
  final String _baseUrl = Environment.getInstance().apiBaseUrl;
  
  // Upload delivery proof (photo or signature)
  Future<DeliveryProof?> uploadDeliveryProof({
    required int orderId,
    required String proofType,
    required File imageFile,
    String? recipientName,
    String? recipientSignature,
    String? notes,
  }) async {
    try {
      final secureStorage = SecureStorageFrave();
      final token = await secureStorage.readToken();
      
      if (token == null) {
        throw Exception('Authentication token not found');
      }
      
      // Create multipart request
      var uri = Uri.parse('$_baseUrl/delivery-proofs/upload');
      
      var request = http.MultipartRequest('POST', uri);
      
      // Add headers
      request.headers.addAll({
        'Authorization': 'Bearer $token',
        // Không set Content-Type ở đây, multipart request sẽ tự set
      });
      
      // Add text fields
      print('DeliveryProofService - orderId being sent: $orderId');
      // Chuyển kiểu số nguyên sang string
      request.fields['orderId'] = orderId.toString();
      request.fields['proofType'] = proofType;
      
      if (recipientName != null) {
        request.fields['recipientName'] = recipientName;
      }
      
      if (recipientSignature != null) {
        request.fields['recipientSignature'] = recipientSignature;
      }
      
      if (notes != null) {
        request.fields['notes'] = notes;
      }
      
      // Add file
      String fileName = imageFile.path.split('/').last;
      String fileExtension = fileName.split('.').last.toLowerCase();
      
      // Determine content type based on file extension
      String contentType = 'image/jpeg';
      if (fileExtension == 'png') {
        contentType = 'image/png';
      }
      
      request.files.add(
        http.MultipartFile(
          'file',
          imageFile.readAsBytes().asStream(),
          imageFile.lengthSync(),
          filename: fileName,
          contentType: MediaType.parse(contentType),
        ),
      );
      
      // Send request
      final streamedResponse = await request.send();
      final response = await http.Response.fromStream(streamedResponse);
      
      if (response.statusCode == 200 || response.statusCode == 201) {
        final decodedData = json.decode(response.body);
        return DeliveryProof.fromJson(decodedData);
      } else {
        throw Exception('Failed to upload delivery proof: ${response.statusCode} - ${response.body}');
      }
    } catch (e) {
      print('Error uploading delivery proof: $e');
      return null;
    }
  }
  
  // Get delivery proof by orderId
  Future<List<DeliveryProof>> getDeliveryProofs(int orderId) async {
    try {
      final secureStorage = SecureStorageFrave();
      final token = await secureStorage.readToken();
      
      if (token == null) {
        throw Exception('Authentication token not found');
      }
      
      final response = await http.get(
        Uri.parse('$_baseUrl/delivery-proofs/$orderId'),
        headers: {
          'Authorization': 'Bearer $token',
          'Content-Type': 'application/json',
        },
      );
      
      if (response.statusCode == 200) {
        final List<dynamic> decodedData = json.decode(response.body);
        return decodedData.map((data) => DeliveryProof.fromJson(data)).toList();
      } else if (response.statusCode == 404) {
        // If no proofs found, return empty list instead of throwing an error
        print('No delivery proofs found for order $orderId');
        return [];
      } else {
        throw Exception('Failed to get delivery proofs: ${response.statusCode}');
      }
    } catch (e) {
      print('Error getting delivery proofs: $e');
      return [];
    }
  }
}