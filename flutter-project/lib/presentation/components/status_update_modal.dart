import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_button.dart';

class StatusUpdateModal extends StatefulWidget {
  final String title;
  final String itemId;
  final String currentStatus;
  final List<Map<String, dynamic>> statusOptions;
  final Function(String status, String notes) onUpdateStatus;
  final Color Function(String status) getStatusColor;

  const StatusUpdateModal({
    super.key,
    required this.title,
    required this.itemId,
    required this.currentStatus,
    required this.statusOptions,
    required this.onUpdateStatus,
    required this.getStatusColor,
  });

  @override
  State<StatusUpdateModal> createState() => _StatusUpdateModalState();
}

class _StatusUpdateModalState extends State<StatusUpdateModal> {
  late String _selectedStatus;
  final TextEditingController _notesController = TextEditingController();

  @override
  void initState() {
    super.initState();
    _selectedStatus = widget.currentStatus;
  }

  @override
  void dispose() {
    _notesController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;

    return Dialog(
      backgroundColor: Colors.transparent,
      elevation: 0,
      child: ConstrainedBox(
        constraints: BoxConstraints(
          maxHeight: MediaQuery.of(context).size.height * 0.8,
          maxWidth: MediaQuery.of(context).size.width * 0.9,
        ),
        child: GlassCard(
          padding: const EdgeInsets.all(20),
          gradient: LinearGradient(
            colors: [
              SpatialDesignSystem.primaryColor.withOpacity(0.1),
              SpatialDesignSystem.accentColor.withOpacity(0.05),
            ],
            begin: Alignment.topLeft,
            end: Alignment.bottomRight,
          ),
          child: SingleChildScrollView(
            child: Column(
              mainAxisSize: MainAxisSize.min,
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                // Header
                Row(
                  children: [
                    Container(
                      padding: const EdgeInsets.all(8),
                      decoration: BoxDecoration(
                        color:
                            SpatialDesignSystem.primaryColor.withOpacity(0.1),
                        borderRadius: BorderRadius.circular(8),
                      ),
                      child: Icon(
                        Icons.update,
                        size: 22,
                        color: SpatialDesignSystem.primaryColor,
                      ),
                    ),
                    const SizedBox(width: 12),
                    Expanded(
                      child: Text(
                        widget.title,
                        style: SpatialDesignSystem.subtitleMedium.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                          fontWeight: FontWeight.w600,
                        ),
                      ),
                    ),
                  ],
                ),

                const SizedBox(height: 16),
                const Divider(),
                const SizedBox(height: 12),

                // Item ID
                Text(
                  widget.itemId,
                  style: SpatialDesignSystem.bodyMedium.copyWith(
                    fontWeight: FontWeight.bold,
                    color: SpatialDesignSystem.primaryColor,
                  ),
                ),

                const SizedBox(height: 16),

                // Status dropdown
                Text(
                  "Select new status:",
                  style: SpatialDesignSystem.bodyMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                  ),
                ),
                const SizedBox(height: 8),
                Container(
                  decoration: BoxDecoration(
                    borderRadius: BorderRadius.circular(10),
                    boxShadow: [
                      BoxShadow(
                        color: Colors.black.withOpacity(0.05),
                        blurRadius: 5,
                        offset: const Offset(0, 2),
                      ),
                    ],
                  ),
                  child: DropdownButtonFormField<String>(
                    value: _selectedStatus,
                    decoration: InputDecoration(
                      border: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(10),
                        borderSide: BorderSide(
                          color: isDark
                              ? Colors.white.withOpacity(0.1)
                              : Colors.black.withOpacity(0.1),
                        ),
                      ),
                      enabledBorder: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(10),
                        borderSide: BorderSide(
                          color: isDark
                              ? Colors.white.withOpacity(0.1)
                              : Colors.black.withOpacity(0.1),
                        ),
                      ),
                      focusedBorder: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(10),
                        borderSide: BorderSide(
                          color: SpatialDesignSystem.primaryColor,
                        ),
                      ),
                      filled: true,
                      fillColor: isDark
                          ? Colors.black.withOpacity(0.2)
                          : Colors.white.withOpacity(0.8),
                      contentPadding: const EdgeInsets.symmetric(
                          horizontal: 16, vertical: 12),
                    ),
                    style: SpatialDesignSystem.bodyMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                    isExpanded: true, // Ensure dropdown uses full width
                    itemHeight:
                        48, // Giảm từ 56 xuống 48 để phù hợp với padding nhỏ hơn
                    items:
                        widget.statusOptions.map((Map<String, dynamic> status) {
                      return DropdownMenuItem<String>(
                        value: status["name"],
                        child: Container(
                          padding: const EdgeInsets.symmetric(
                              vertical: 4), // Giảm từ 8 xuống 4
                          child: Row(
                            children: [
                              Container(
                                width: 12,
                                height: 12,
                                decoration: BoxDecoration(
                                  color: widget.getStatusColor(status["name"]),
                                  shape: BoxShape.circle,
                                ),
                              ),
                              const SizedBox(width: 12),
                              Expanded(
                                child: Text(
                                  status["display"],
                                  style:
                                      SpatialDesignSystem.bodyMedium.copyWith(
                                    color: isDark
                                        ? SpatialDesignSystem
                                            .textDarkPrimaryColor
                                        : SpatialDesignSystem.textPrimaryColor,
                                    fontWeight: FontWeight.w500,
                                  ),
                                  overflow: TextOverflow.ellipsis,
                                  maxLines: 1,
                                ),
                              ),
                            ],
                          ),
                        ),
                      );
                    }).toList(),
                    onChanged: (String? newValue) {
                      if (newValue != null) {
                        setState(() {
                          _selectedStatus = newValue;
                        });
                      }
                    },
                    icon: Icon(
                      Icons.arrow_drop_down,
                      color: SpatialDesignSystem.primaryColor,
                    ),
                    dropdownColor: isDark
                        ? SpatialDesignSystem.darkBackgroundColor
                        : SpatialDesignSystem.backgroundColor,
                  ),
                ),

                const SizedBox(height: 16),

                // Notes field
                Text(
                  "Add notes (optional):",
                  style: SpatialDesignSystem.bodyMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                  ),
                ),
                const SizedBox(height: 8),
                Container(
                  decoration: BoxDecoration(
                    borderRadius: BorderRadius.circular(10),
                    boxShadow: [
                      BoxShadow(
                        color: Colors.black.withOpacity(0.05),
                        blurRadius: 5,
                        offset: const Offset(0, 2),
                      ),
                    ],
                  ),
                  child: TextField(
                    controller: _notesController,
                    decoration: InputDecoration(
                      border: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(10),
                        borderSide: BorderSide(
                          color: isDark
                              ? Colors.white.withOpacity(0.1)
                              : Colors.black.withOpacity(0.1),
                        ),
                      ),
                      enabledBorder: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(10),
                        borderSide: BorderSide(
                          color: isDark
                              ? Colors.white.withOpacity(0.1)
                              : Colors.black.withOpacity(0.1),
                        ),
                      ),
                      focusedBorder: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(10),
                        borderSide: BorderSide(
                          color: SpatialDesignSystem.primaryColor,
                        ),
                      ),
                      filled: true,
                      fillColor: isDark
                          ? Colors.black.withOpacity(0.2)
                          : Colors.white.withOpacity(0.8),
                      hintText: "Enter notes about status change",
                      hintStyle: TextStyle(
                        color: isDark
                            ? Colors.white.withOpacity(0.5)
                            : Colors.black.withOpacity(0.5),
                      ),
                      contentPadding: const EdgeInsets.symmetric(
                          horizontal: 16, vertical: 12),
                    ),
                    maxLines: 3,
                    style: SpatialDesignSystem.bodyMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                  ),
                ),

                const SizedBox(height: 20),

                // Action buttons
                Row(
                  mainAxisAlignment: MainAxisAlignment.end,
                  children: [
                    SpatialButton(
                      text: "Cancel",
                      onPressed: () => Navigator.pop(context),
                      isOutlined: true,
                      textColor: isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor,
                      padding: const EdgeInsets.symmetric(
                          horizontal: 16, vertical: 10),
                    ),
                    const SizedBox(width: 12),
                    SpatialButton(
                      text: "Update",
                      onPressed: () {
                        // Close dialog
                        Navigator.pop(context);

                        // Call the update function
                        widget.onUpdateStatus(
                          _selectedStatus,
                          _notesController.text.trim(),
                        );
                      },
                      isGradient: true,
                      gradient: LinearGradient(
                        colors: [
                          SpatialDesignSystem.primaryColor,
                          SpatialDesignSystem.accentColor,
                        ],
                        begin: Alignment.topLeft,
                        end: Alignment.bottomRight,
                      ),
                      padding: const EdgeInsets.symmetric(
                          horizontal: 16, vertical: 10),
                    ),
                  ],
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}

/// Helper function to show the status update modal
void showStatusUpdateModal({
  required BuildContext context,
  required String title,
  required String itemId,
  required String currentStatus,
  required List<Map<String, dynamic>> statusOptions,
  required Function(String status, String notes) onUpdateStatus,
  required Color Function(String status) getStatusColor,
}) {
  showDialog(
    context: context,
    builder: (context) => StatusUpdateModal(
      title: title,
      itemId: itemId,
      currentStatus: currentStatus,
      statusOptions: statusOptions,
      onUpdateStatus: onUpdateStatus,
      getStatusColor: getStatusColor,
    ),
  );
}
