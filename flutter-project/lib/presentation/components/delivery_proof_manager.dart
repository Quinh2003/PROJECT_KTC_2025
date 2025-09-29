import 'dart:io';
import 'package:flutter/material.dart';
import 'package:image_picker/image_picker.dart';
import 'package:intl/intl.dart';
import 'package:ktc_logistics_driver/domain/models/delivery_proof/delivery_proof.dart';
import 'package:ktc_logistics_driver/services/delivery_proof_service.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_button.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';

class DeliveryProofManager extends StatefulWidget {
  final int orderId;
  final String orderStatus;
  final bool canUpload;
  final List<DeliveryProof>? initialProofs;
  final Function()? onProofUpdated;

  const DeliveryProofManager({
    super.key,
    required this.orderId,
    required this.orderStatus,
    this.canUpload = false,
    this.initialProofs,
    this.onProofUpdated,
  });

  @override
  State<DeliveryProofManager> createState() => _DeliveryProofManagerState();
}

class _DeliveryProofManagerState extends State<DeliveryProofManager> {
  bool _isLoading = false;
  bool _isUploading = false;
  List<DeliveryProof> _proofs = [];
  File? _selectedImage;
  final TextEditingController _recipientNameController = TextEditingController();
  final TextEditingController _notesController = TextEditingController();
  final DeliveryProofService _deliveryProofService = DeliveryProofService();
  bool _showUploadForm = false;

  @override
  void initState() {
    super.initState();
    _loadProofs();
  }

  @override
  void dispose() {
    _recipientNameController.dispose();
    _notesController.dispose();
    super.dispose();
  }

  Future<void> _loadProofs() async {
    if (widget.initialProofs != null) {
      setState(() {
        _proofs = widget.initialProofs!;
        _isLoading = false;
      });
      return;
    }

    setState(() {
      _isLoading = true;
    });

    try {
      final proofs = await _deliveryProofService.getDeliveryProofs(widget.orderId);
      
      if (!mounted) return;
      
      setState(() {
        _proofs = proofs;
        _isLoading = false;
      });
    } catch (e) {
      if (!mounted) return;
      
      setState(() {
        _isLoading = false;
      });
      
      _showErrorSnackBar('Error loading delivery proofs: $e');
    }
  }

  void _toggleUploadForm() {
    setState(() {
      _showUploadForm = !_showUploadForm;
      // Reset form when opening
      if (_showUploadForm) {
        _selectedImage = null;
        _recipientNameController.clear();
        _notesController.clear();
      }
    });
  }

  Future<void> _pickImage(ImageSource source) async {
    try {
      final ImagePicker picker = ImagePicker();
      final XFile? image = await picker.pickImage(
        source: source,
        imageQuality: 70,
        maxWidth: 1280,
      );
      
      if (image != null && mounted) {
        setState(() {
          _selectedImage = File(image.path);
        });
      }
    } catch (e) {
      _showErrorSnackBar('Error picking image: $e');
    }
  }
  
  Future<void> _uploadProof() async {
    if (_selectedImage == null) {
      _showErrorSnackBar('Please select an image first');
      return;
    }
    
    setState(() {
      _isUploading = true;
    });
    
    // Log orderId để kiểm tra giá trị
    print('Uploading proof for orderId: ${widget.orderId}');
    
    try {
      final deliveryProof = await _deliveryProofService.uploadDeliveryProof(
        orderId: widget.orderId,
        proofType: 'PHOTO',
        imageFile: _selectedImage!,
        recipientName: _recipientNameController.text.isNotEmpty ? 
            _recipientNameController.text : null,
        notes: _notesController.text.isNotEmpty ? 
            _notesController.text : null,
      );
      
      if (!mounted) return;
      setState(() {
        _isUploading = false;
      });
      
      if (deliveryProof != null) {
        // Add the new proof to the list
        setState(() {
          _proofs.add(deliveryProof);
          _showUploadForm = false;  // Close the form
        });
        
        // Show success message
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(content: Text('Delivery proof uploaded successfully')),
        );
        
        // Notify parent if needed
        if (widget.onProofUpdated != null) {
          widget.onProofUpdated!();
        }
      } else {
        _showErrorSnackBar('Failed to upload delivery proof');
      }
    } catch (e) {
      if (!mounted) return;
      setState(() {
        _isUploading = false;
      });
      _showErrorSnackBar('Error uploading proof: $e');
    }
  }

  void _showErrorSnackBar(String message) {
    if (!mounted) return;
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(content: Text(message)),
    );
  }
  
  // Get authentication token from secure storage
  Future<String> _getAuthToken() async {
    final secureStorage = SecureStorageFrave();
    final token = await secureStorage.readToken();
    if (token == null) {
      throw Exception('Authentication token not found');
    }
    return token;
  }
  
  // Create an image provider that includes authentication headers
  ImageProvider _getAuthenticatedImageProvider(String imageUrl, String token) {
    // Process the image URL to use the new API endpoint
    String processedUrl;
    String fileName;
    final baseUrl = Environment.getInstance().apiBaseUrl;
    
    // Extract filename from any type of URL
    if (imageUrl.contains('/')) {
      fileName = imageUrl.split('/').last;
    } else {
      fileName = imageUrl;
    }
    
    // Create URL pointing to our authenticated endpoint
    processedUrl = '$baseUrl/delivery-proofs/files/$fileName';
    
    print('🖼️ Loading image through authenticated API: $processedUrl');
    
    // Return a network image with authentication headers
    return NetworkImage(
      processedUrl,
      headers: {
        'Authorization': 'Bearer $token',
      },
    );
  }

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Column(
      children: [
        const SizedBox(height: 16),
        GlassCard(
          padding: const EdgeInsets.all(16),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Text(
                    "Delivery Proof",
                    style: SpatialDesignSystem.subtitleMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                  ),
                  if (widget.canUpload && !_showUploadForm)
                    SpatialButton(
                      text: 'Add Proof',
                      iconData: Icons.add_photo_alternate,
                      onPressed: _toggleUploadForm,
                      isGlass: true,
                      backgroundColor: SpatialDesignSystem.successColor.withOpacity(0.1),
                      textColor: SpatialDesignSystem.successColor,
                      padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
                    ),
                ],
              ),
              
              // Upload Form (when shown)
              if (_showUploadForm) ...[
                const SizedBox(height: 16),
                const Divider(),
                const SizedBox(height: 16),
                
                // Image Preview
                if (_selectedImage != null)
                  Container(
                    height: 200,
                    width: double.infinity,
                    margin: const EdgeInsets.only(bottom: 16),
                    decoration: BoxDecoration(
                      borderRadius: BorderRadius.circular(12),
                      color: Colors.grey.shade200,
                      image: DecorationImage(
                        image: FileImage(_selectedImage!),
                        fit: BoxFit.cover,
                      ),
                    ),
                  )
                else
                  Container(
                    height: 120,
                    width: double.infinity,
                    margin: const EdgeInsets.only(bottom: 16),
                    decoration: BoxDecoration(
                      borderRadius: BorderRadius.circular(12),
                      color: isDark ? Colors.grey.shade800 : Colors.grey.shade200,
                    ),
                    child: const Center(
                      child: Text(
                        'No image selected',
                        style: TextStyle(color: Colors.grey),
                      ),
                    ),
                  ),
                
                // Image Selection Buttons
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                  children: [
                    SpatialButton(
                      text: 'Camera',
                      iconData: Icons.camera_alt,
                      onPressed: () => _pickImage(ImageSource.camera),
                      isGlass: true,
                      backgroundColor: Theme.of(context).brightness == Brightness.dark
                          ? Colors.white.withOpacity(0.1)
                          : Colors.black.withOpacity(0.05),
                      textColor: SpatialDesignSystem.primaryColor,
                      padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                    ),
                    SpatialButton(
                      text: 'Gallery',
                      iconData: Icons.photo_library,
                      onPressed: () => _pickImage(ImageSource.gallery),
                      isGlass: true,
                      backgroundColor: Theme.of(context).brightness == Brightness.dark
                          ? Colors.white.withOpacity(0.1)
                          : Colors.black.withOpacity(0.05),
                      textColor: SpatialDesignSystem.primaryColor,
                      padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                    ),
                  ],
                ),
                
                const SizedBox(height: 16),
                
                // Recipient Name Field
                TextField(
                  controller: _recipientNameController,
                  decoration: const InputDecoration(
                    labelText: 'Recipient Name (Optional)',
                    border: OutlineInputBorder(),
                  ),
                ),
                
                const SizedBox(height: 16),
                
                // Notes Field
                TextField(
                  controller: _notesController,
                  decoration: const InputDecoration(
                    labelText: 'Notes (Optional)',
                    border: OutlineInputBorder(),
                  ),
                  maxLines: 2,
                ),
                
                const SizedBox(height: 16),
                
                // Action Buttons
                Row(
                  children: [
                    Expanded(
                      child: SpatialButton(
                        text: 'Cancel',
                        onPressed: _toggleUploadForm,
                        isOutlined: true,
                      ),
                    ),
                    const SizedBox(width: 16),
                    Expanded(
                      child: SpatialButton(
                        text: _isUploading ? '...' : 'Submit',
                        iconData: _isUploading ? Icons.hourglass_top : Icons.check_circle_outline,
                        onPressed: _isUploading ? () {} : () => _uploadProof(),
                        isGradient: true,
                        padding: _isUploading 
                            ? const EdgeInsets.symmetric(horizontal: 10, vertical: 8)
                            : null,
                        gradient: LinearGradient(
                          colors: [
                            SpatialDesignSystem.successColor,
                            SpatialDesignSystem.successColor.withGreen(150),
                          ],
                          begin: Alignment.topLeft,
                          end: Alignment.bottomRight,
                        ),
                        textColor: Colors.white,
                      ),
                    ),
                  ],
                ),
              ],
              
              if (!_showUploadForm) ...[
                const SizedBox(height: 16),
                
                // Show delivery proofs or loading indicator
                if (_isLoading)
                  const Center(
                    child: Padding(
                      padding: EdgeInsets.all(16.0),
                      child: CircularProgressIndicator(),
                    ),
                  )
                else if (_proofs.isEmpty)
                  Center(
                    child: Padding(
                      padding: const EdgeInsets.all(16.0),
                      child: Column(
                        children: [
                          Text(
                            'No delivery proofs uploaded yet',
                            style: SpatialDesignSystem.bodyMedium.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                            ),
                          ),
                          // Removed duplicate "Upload Delivery Proof" button
                          // We already have "Add Proof" button at the top of the component
                        ],
                      ),
                    ),
                  )
                else
                  Column(
                    children: _proofs.map((proof) => 
                      _buildProofItem(proof, context)
                    ).toList(),
                  ),
              ],
            ],
          ),
        ),
      ],
    );
  }
  
  Widget _buildProofItem(DeliveryProof proof, BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Padding(
      padding: const EdgeInsets.only(bottom: 12.0),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          // Header with date
          Row(
            children: [
              const Icon(
                Icons.check_circle,
                color: SpatialDesignSystem.successColor,
                size: 16,
              ),
              const SizedBox(width: 8),
              Text(
                'Proof #${proof.id ?? ''}',
                style: TextStyle(
                  fontWeight: FontWeight.bold,
                  fontSize: 14,
                  color: isDark ? Colors.white : Colors.black87,
                ),
              ),
              const Spacer(),
              Text(
                DateFormat('MMM d, yyyy h:mm a').format(proof.timestamp),
                style: TextStyle(
                  fontSize: 12,
                  color: isDark ? Colors.white70 : Colors.black54,
                ),
              ),
            ],
          ),
          
          const SizedBox(height: 12),
          
          // Proof Image
                if (proof.imageUrl != null)
            FutureBuilder<String>(
              future: _getAuthToken(),
              builder: (context, snapshot) {
                if (snapshot.connectionState == ConnectionState.waiting) {
                  return Container(
                    height: 120,
                    width: double.infinity,
                    decoration: BoxDecoration(
                      borderRadius: BorderRadius.circular(8),
                      color: Colors.grey.shade200,
                    ),
                    child: const Center(child: CircularProgressIndicator()),
                  );
                } else if (snapshot.hasError || !snapshot.hasData) {
                  return Container(
                    height: 120,
                    width: double.infinity,
                    decoration: BoxDecoration(
                      borderRadius: BorderRadius.circular(8),
                      color: Colors.grey.shade200,
                    ),
                    child: const Center(
                      child: Icon(Icons.error_outline, color: Colors.red, size: 40),
                    ),
                  );
                }
                
                return GestureDetector(
                  onTap: () {
                    // Show image in full-screen (can be implemented later)
                  },
                  child: Container(
                    height: 120,
                    width: double.infinity,
                    decoration: BoxDecoration(
                      borderRadius: BorderRadius.circular(8),
                      image: DecorationImage(
                        image: _getAuthenticatedImageProvider(
                          proof.imageUrl!, 
                          snapshot.data!
                        ),
                        fit: BoxFit.cover,
                      ),
                    ),
                  ),
                );
              },
            ),          const SizedBox(height: 12),
          
          // Details (if available)
          if (proof.recipientName != null && proof.recipientName!.isNotEmpty)
            _buildInfoRow(
              context,
              Icons.person,
              'Recipient',
              proof.recipientName!,
            ),
            
          if (proof.notes != null && proof.notes!.isNotEmpty)
            _buildInfoRow(
              context,
              Icons.notes,
              'Notes',
              proof.notes!,
            ),
            
          // Add a divider between multiple proofs
          if (proof != _proofs.last)
            const Padding(
              padding: EdgeInsets.symmetric(vertical: 8),
              child: Divider(),
            ),
        ],
      ),
    );
  }
  
  Widget _buildInfoRow(BuildContext context, IconData icon, String label, String value) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Padding(
      padding: const EdgeInsets.only(bottom: 8),
      child: Row(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Icon(
            icon,
            size: 16,
            color: isDark ? Colors.white70 : Colors.black54,
          ),
          const SizedBox(width: 8),
          Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                label,
                style: TextStyle(
                  fontSize: 12,
                  fontWeight: FontWeight.w500,
                  color: isDark ? Colors.white70 : Colors.black54,
                ),
              ),
              const SizedBox(height: 2),
              Text(
                value,
                style: TextStyle(
                  fontSize: 14,
                  color: isDark ? Colors.white : Colors.black87,
                ),
              ),
            ],
          ),
        ],
      ),
    );
  }
}