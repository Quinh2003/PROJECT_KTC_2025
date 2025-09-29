import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';
import 'package:ktc_logistics_driver/presentation/components/spatial_glass_card.dart';
import 'package:ktc_logistics_driver/domain/models/maintenance/maintenance_request.dart';
import 'package:ktc_logistics_driver/presentation/blocs/maintenance/maintenance_bloc.dart';

class MaintenanceScreen extends StatelessWidget {
  const MaintenanceScreen({super.key});

  @override
  Widget build(BuildContext context) {
    // Do not create BlocProvider here, use the BlocProvider already provided in app.dart
    // and trigger load event when the screen is first displayed
    return BlocListener<MaintenanceBloc, MaintenanceState>(
      listener: (context, state) {
        // Trigger load event when the screen is first initialized
        if (state is MaintenanceInitial) {
          context.read<MaintenanceBloc>().add(const LoadMaintenanceRequests());
        }
      },
      child: const MaintenanceScreenContent(),
    );
  }
}

class MaintenanceScreenContent extends StatefulWidget {
  const MaintenanceScreenContent({super.key});

  @override
  State<MaintenanceScreenContent> createState() =>
      _MaintenanceScreenContentState();
}

class _MaintenanceScreenContentState extends State<MaintenanceScreenContent>
    with SingleTickerProviderStateMixin {
  // Add variable to track dark mode
  bool _isDark = false;
  late TabController _tabController;

  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: 3, vsync: this);
  }

  @override
  void dispose() {
    _tabController.dispose();
    super.dispose();
  }

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();
    // Update dark mode state
    final newIsDark = Theme.of(context).brightness == Brightness.dark;
    if (_isDark != newIsDark) {
      setState(() {
        _isDark = newIsDark;
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    // Update state in build to ensure always correct
    _isDark = Theme.of(context).brightness == Brightness.dark;

    return Scaffold(
      backgroundColor: _isDark
          ? SpatialDesignSystem.darkBackgroundColor
          : SpatialDesignSystem.backgroundColor,
      appBar: AppBar(
        backgroundColor: _isDark
            ? SpatialDesignSystem.darkBackgroundColor
            : SpatialDesignSystem.backgroundColor,
        title: Text(
          'Maintenance',
          style: SpatialDesignSystem.headingMedium.copyWith(
            color: _isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        centerTitle: true,
        elevation: 0,
        automaticallyImplyLeading: false, // Remove back button
        actions: [
          IconButton(
            icon: Icon(
              Icons.refresh,
              color: _isDark
                  ? SpatialDesignSystem.textDarkPrimaryColor
                  : SpatialDesignSystem.primaryColor,
            ),
            onPressed: () {
              print('🔄 UI: Maintenance refresh button pressed!');
              // Reload maintenance from API using the specific refresh event
              context.read<MaintenanceBloc>().add(const RefreshMaintenanceRequests());
              print('🔄 UI: RefreshMaintenanceRequests dispatched');
              
              // Show loading indicator
              ScaffoldMessenger.of(context).showSnackBar(
                const SnackBar(
                  content: Text('Refreshing maintenance requests...'),
                  duration: Duration(seconds: 1),
                ),
              );
            },
            tooltip: 'Reload',
          ),
        ],
        bottom: TabBar(
          controller: _tabController,
          indicatorColor: SpatialDesignSystem.primaryColor,
          labelColor: SpatialDesignSystem.primaryColor,
          unselectedLabelColor: _isDark
              ? SpatialDesignSystem.textDarkSecondaryColor
              : SpatialDesignSystem.textSecondaryColor,
          isScrollable: true,
          tabAlignment: TabAlignment.center,
          labelPadding: const EdgeInsets.symmetric(horizontal: 12),
          tabs: const [
            Tab(text: "Pending"),
            Tab(text: "Under Maintenance"),
            Tab(text: "Completed"),
          ],
        ),
      ),
      body: Container(
        decoration: BoxDecoration(
          gradient: LinearGradient(
            begin: Alignment.topCenter,
            end: Alignment.bottomCenter,
            colors: [
              _isDark
                  ? SpatialDesignSystem.darkBackgroundColor
                  : SpatialDesignSystem.backgroundColor,
              _isDark
                  ? SpatialDesignSystem.darkBackgroundColor.withOpacity(0.8)
                  : SpatialDesignSystem.backgroundColor.withOpacity(0.8),
            ],
          ),
        ),
        child: BlocConsumer<MaintenanceBloc, MaintenanceState>(
          listener: (context, state) {
            if (state is MaintenanceError) {
              ScaffoldMessenger.of(context).showSnackBar(
                SnackBar(
                  content: Text(state.message),
                  backgroundColor: SpatialDesignSystem.errorColor,
                ),
              );
            } else if (state is MaintenanceCreated) {
              ScaffoldMessenger.of(context).showSnackBar(
                SnackBar(
                  content:
                      const Text('Maintenance request created successfully'),
                  backgroundColor: SpatialDesignSystem.successColor,
                ),
              );
            }
          },
          builder: (context, state) {
            return TabBarView(
              controller: _tabController,
              children: [
                PendingMaintenanceTab(),
                UnderMaintenanceTab(),
                CompletedMaintenanceTab(),
              ],
            );
          },
        ),
      ),
      floatingActionButton: _buildFloatingActionButton(context),
    );
  }

  Widget _buildFloatingActionButton(BuildContext context) {
    return BlocBuilder<MaintenanceBloc, MaintenanceState>(
      builder: (context, state) {
        final isCreating = state is MaintenanceCreating;

        return FloatingActionButton(
          onPressed:
              isCreating ? null : () => _showCreateMaintenanceDialog(context),
          backgroundColor:
              isCreating ? Colors.grey : SpatialDesignSystem.primaryColor,
          child: isCreating
              ? const SizedBox(
                  width: 20,
                  height: 20,
                  child: CircularProgressIndicator(
                    strokeWidth: 2,
                    valueColor: AlwaysStoppedAnimation<Color>(Colors.white),
                  ),
                )
              : const Icon(Icons.add, color: Colors.white),
        );
      },
    );
  }

  void _showCreateMaintenanceDialog(BuildContext context) {
    final descriptionController = TextEditingController();
    final notesController = TextEditingController();
    String selectedType = MaintenanceType.routine.name;
    int selectedVehicleId =
        1; // Default vehicle ID, should be retrieved from a list

    showDialog(
      context: context,
      builder: (dialogContext) => AlertDialog(
        backgroundColor: _isDark
            ? SpatialDesignSystem.darkSurfaceColor
            : SpatialDesignSystem.surfaceColor,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(16),
        ),
        title: Container(
          padding: const EdgeInsets.only(bottom: 8),
          decoration: BoxDecoration(
            border: Border(
              bottom: BorderSide(
                color: Colors.grey.withOpacity(0.2),
                width: 1,
              ),
            ),
          ),
          child: Text(
            'Create maintenance request',
            style: SpatialDesignSystem.headingSmall.copyWith(
              color: _isDark
                  ? SpatialDesignSystem.textDarkPrimaryColor
                  : SpatialDesignSystem.textPrimaryColor,
            ),
          ),
        ),
        content: SingleChildScrollView(
          child: Column(
            mainAxisSize: MainAxisSize.min,
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              // Vehicle selection (would be a dropdown in a real app)
              Text(
                'Vehicle',
                style: SpatialDesignSystem.captionText.copyWith(
                  color: _isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              Container(
                padding:
                    const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                decoration: BoxDecoration(
                  color: _isDark
                      ? SpatialDesignSystem.darkSurfaceColorSecondary
                      : SpatialDesignSystem.backgroundColor,
                  borderRadius: BorderRadius.circular(8),
                  border: Border.all(color: Colors.grey.withOpacity(0.3)),
                ),
                child: Row(
                  children: [
                    Icon(Icons.directions_car,
                        size: 18,
                        color: _isDark
                            ? SpatialDesignSystem.textDarkPrimaryColor
                            : SpatialDesignSystem.textPrimaryColor),
                    const SizedBox(width: 8),
                    Text(
                      'KTC-51A 12345', // Demo value, should be dynamic
                      style: SpatialDesignSystem.bodyMedium.copyWith(
                        color: _isDark
                            ? SpatialDesignSystem.textDarkPrimaryColor
                            : SpatialDesignSystem.textPrimaryColor,
                      ),
                    ),
                  ],
                ),
              ),
              const SizedBox(height: 16),

              // Maintenance type selection
              Text(
                'Maintenance type',
                style: SpatialDesignSystem.captionText.copyWith(
                  color: _isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              StatefulBuilder(
                builder: (context, setState) {
                  return DropdownButtonFormField<String>(
                    value: selectedType,
                    decoration: InputDecoration(
                      filled: true,
                      fillColor: _isDark
                          ? SpatialDesignSystem.darkSurfaceColorSecondary
                          : SpatialDesignSystem.backgroundColor,
                      border: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(8),
                        borderSide:
                            BorderSide(color: Colors.grey.withOpacity(0.3)),
                      ),
                      contentPadding: const EdgeInsets.symmetric(
                          horizontal: 12, vertical: 8),
                    ),
                    dropdownColor: _isDark
                        ? SpatialDesignSystem.darkSurfaceColorSecondary
                        : SpatialDesignSystem.backgroundColor,
                    style: TextStyle(
                      color: _isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                    items: [
                      DropdownMenuItem(
                        value: MaintenanceType.routine.name,
                        child: Text('Routine maintenance'),
                      ),
                      DropdownMenuItem(
                        value: MaintenanceType.repair.name,
                        child: Text('Repair'),
                      ),
                      DropdownMenuItem(
                        value: MaintenanceType.inspection.name,
                        child: Text('Inspection'),
                      ),
                      DropdownMenuItem(
                        value: MaintenanceType.emergency.name,
                        child: Text('Emergency'),
                      ),
                    ],
                    onChanged: (value) {
                      if (value != null) {
                        setState(() => selectedType = value);
                      }
                    },
                  );
                },
              ),
              const SizedBox(height: 16),

              // Description input
              Text(
                'Problem description',
                style: SpatialDesignSystem.captionText.copyWith(
                  color: _isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              TextField(
                controller: descriptionController,
                style: TextStyle(
                  color: _isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
                decoration: InputDecoration(
                  filled: true,
                  fillColor: _isDark
                      ? SpatialDesignSystem.darkSurfaceColorSecondary
                      : SpatialDesignSystem.backgroundColor,
                  border: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(8),
                    borderSide: BorderSide(color: Colors.grey.withOpacity(0.3)),
                  ),
                  hintText: 'Describe the vehicle issue in detail',
                  contentPadding:
                      const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                ),
                maxLines: 3,
              ),
              const SizedBox(height: 16),

              // Notes input
              Text(
                'Additional notes (optional)',
                style: SpatialDesignSystem.captionText.copyWith(
                  color: _isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              TextField(
                controller: notesController,
                style: TextStyle(
                  color: _isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                ),
                decoration: InputDecoration(
                  filled: true,
                  fillColor: _isDark
                      ? SpatialDesignSystem.darkSurfaceColorSecondary
                      : SpatialDesignSystem.backgroundColor,
                  border: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(8),
                    borderSide: BorderSide(color: Colors.grey.withOpacity(0.3)),
                  ),
                  hintText: 'Additional information',
                  contentPadding:
                      const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                ),
                maxLines: 2,
              ),
            ],
          ),
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.of(dialogContext).pop(),
            child: Text(
              'Cancel',
              style: SpatialDesignSystem.bodyMedium.copyWith(
                color: _isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
              ),
            ),
          ),
          ElevatedButton(
            onPressed: () {
              if (descriptionController.text.trim().isEmpty) {
                ScaffoldMessenger.of(context).showSnackBar(
                  const SnackBar(
                      content: Text('Please enter a problem description')),
                );
                return;
              }

              // Create maintenance request
              context.read<MaintenanceBloc>().add(
                    CreateMaintenanceRequest(
                      driverId: 1, // This will be ignored by the service
                      createDto: CreateMaintenanceRequestDto(
                        vehicleId: selectedVehicleId,
                        description: descriptionController.text.trim(),
                        maintenanceType: selectedType,
                        statusId: 51, // MAINTENANCE_PENDING
                        notes: notesController.text.trim().isNotEmpty
                            ? notesController.text.trim()
                            : null,
                      ),
                    ),
                  );

              Navigator.of(dialogContext).pop();
            },
            style: ElevatedButton.styleFrom(
              backgroundColor: SpatialDesignSystem.primaryColor,
              shape: RoundedRectangleBorder(
                borderRadius: BorderRadius.circular(8),
              ),
            ),
            child: Text(
              'Create request',
              style: SpatialDesignSystem.bodyMedium.copyWith(
                color: Colors.white,
              ),
            ),
          ),
        ],
      ),
    );
  }
}

// Dialog for driver to take vehicle to garage (Step 5)
void _showTakeToGarageDialog(BuildContext context, MaintenanceRequest request) {
  final notesController = TextEditingController();

  showDialog(
    context: context,
    builder: (dialogContext) => AlertDialog(
      backgroundColor: SpatialDesignSystem.surfaceColor,
      shape: RoundedRectangleBorder(
        borderRadius: BorderRadius.circular(16),
      ),
      title: Container(
        padding: const EdgeInsets.only(bottom: 8),
        decoration: BoxDecoration(
          border: Border(
            bottom: BorderSide(
              color: Colors.grey.withOpacity(0.2),
              width: 1,
            ),
          ),
        ),
        child: Text(
          'Take vehicle to garage',
          style: SpatialDesignSystem.headingSmall.copyWith(
            color: SpatialDesignSystem.textPrimaryColor,
          ),
        ),
      ),
      content: Column(
        mainAxisSize: MainAxisSize.min,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'Confirm taking vehicle ${request.vehicle?.licensePlate ?? "N/A"} to garage for maintenance.',
            style: SpatialDesignSystem.bodyMedium.copyWith(
              color: SpatialDesignSystem.textPrimaryColor,
            ),
          ),
          const SizedBox(height: 16),
          Text(
            'Notes (optional)',
            style: SpatialDesignSystem.captionText.copyWith(
              color: SpatialDesignSystem.textSecondaryColor,
            ),
          ),
          TextField(
            controller: notesController,
            decoration: InputDecoration(
              filled: true,
              fillColor: SpatialDesignSystem.backgroundColor,
              border: OutlineInputBorder(
                borderRadius: BorderRadius.circular(8),
                borderSide: BorderSide(color: Colors.grey.withOpacity(0.3)),
              ),
              hintText: 'Add notes when handing over the vehicle...',
              contentPadding:
                  const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
            ),
            maxLines: 3,
          ),
        ],
      ),
      actions: [
        TextButton(
          onPressed: () => Navigator.of(dialogContext).pop(),
          child: Text(
            'Hủy',
            style: SpatialDesignSystem.bodyMedium.copyWith(
              color: SpatialDesignSystem.textSecondaryColor,
            ),
          ),
        ),
        ElevatedButton(
          onPressed: () {
            // Update status to MAINTENANCE (19)
            context.read<MaintenanceBloc>().add(
                  UpdateMaintenanceRequest(
                    driverId: 0, // Not used in our updated service
                    maintenanceId: request.id,
                    updateDto: CreateMaintenanceRequestDto(
                      vehicleId: request.vehicle?.id ?? 0,
                      description: request.description,
                      maintenanceType: request.maintenanceType,
                      statusId: 19, // MAINTENANCE status
                      notes: notesController.text.trim().isNotEmpty
                          ? notesController.text.trim()
                          : null,
                    ),
                  ),
                );

            Navigator.of(dialogContext).pop();
            ScaffoldMessenger.of(context).showSnackBar(
              const SnackBar(
                content: Text('Confirmed vehicle taken to garage'),
                backgroundColor: Colors.green,
              ),
            );
          },
          style: ElevatedButton.styleFrom(
            backgroundColor: SpatialDesignSystem.primaryColor,
            shape: RoundedRectangleBorder(
              borderRadius: BorderRadius.circular(8),
            ),
          ),
          child: Text(
            'Confirm',
            style: SpatialDesignSystem.bodyMedium.copyWith(
              color: Colors.white,
            ),
          ),
        ),
      ],
    ),
  );
}

// Dialog for showing maintenance details
void _showMaintenanceDetailDialog(
    BuildContext context, MaintenanceRequest request) {
  final isDark = Theme.of(context).brightness == Brightness.dark;

  showDialog(
    context: context,
    builder: (dialogContext) => Dialog(
      backgroundColor: Colors.transparent,
      elevation: 0,
      insetPadding: EdgeInsets.zero, // Remove default padding
      child: SizedBox(
        height: MediaQuery.of(context).size.height,
        width: MediaQuery.of(context).size.width,
        child: GlassCard(
          padding: const EdgeInsets.all(24), // Tăng padding cho full screen
          gradient: LinearGradient(
            colors: [
              SpatialDesignSystem.primaryColor.withOpacity(0.08),
              SpatialDesignSystem.accentColor.withOpacity(0.04),
            ],
            begin: Alignment.topLeft,
            end: Alignment.bottomRight,
          ),
          child: SingleChildScrollView(
            child: Column(
              mainAxisSize: MainAxisSize.min,
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                // Header with icon and title
                Row(
                  children: [
                    Container(
                      padding: const EdgeInsets.all(12),
                      decoration: BoxDecoration(
                        color: SpatialDesignSystem.primaryColor.withOpacity(0.15),
                        borderRadius: BorderRadius.circular(12),
                      ),
                      child: Icon(
                        Icons.build_circle_rounded,
                        size: 24,
                        color: SpatialDesignSystem.primaryColor,
                      ),
                    ),
                    const SizedBox(width: 12),
                    Expanded(
                      child: Text(
                        'Maintenance Details',
                        style: SpatialDesignSystem.headingSmall.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                          fontWeight: FontWeight.w700,
                        ),
                      ),
                    ),
                  ],
                ),
                
                const SizedBox(height: 20),
                const Divider(),
                const SizedBox(height: 16),

                // Information cards in new layout - 2 columns
                Row(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    // Column 1: 3 ngày - Created, Service Date, Next Due
                    Expanded(
                      child: Column(
                        children: [
                          _buildDetailCard(
                            context: context,
                            title: 'Created',
                            value: _formatDate(request.createdAt),
                            icon: Icons.schedule_rounded,
                            color: SpatialDesignSystem.accentColor,
                            isDark: isDark,
                          ),
                          const SizedBox(height: 12),
                          _buildDetailCard(
                            context: context,
                            title: 'Service',
                            value: request.maintenanceDate != null
                                ? _formatDate(request.maintenanceDate!)
                                : 'Not scheduled',
                            icon: Icons.event_available_rounded,
                            color: SpatialDesignSystem.warningColor,
                            isDark: isDark,
                          ),
                          const SizedBox(height: 12),
                          _buildDetailCard(
                            context: context,
                            title: 'Next Due',
                            value: request.nextDueDate != null
                                ? _formatDate(request.nextDueDate!)
                                : 'Not set',
                            icon: Icons.next_plan_rounded,
                            color: Colors.deepPurple,
                            isDark: isDark,
                          ),
                        ],
                      ),
                    ),
                    const SizedBox(width: 16),
                    
                    // Column 2: Vehicle, Type, Cost
                    Expanded(
                      child: Column(
                        children: [
                          _buildDetailCard(
                            context: context,
                            title: 'Vehicle',
                            value: request.vehicle?.licensePlate ?? 'N/A',
                            icon: Icons.local_shipping_rounded,
                            color: Colors.teal,
                            isDark: isDark,
                          ),
                          const SizedBox(height: 12),
                          _buildDetailCard(
                            context: context,
                            title: 'Type',
                            value: request.maintenanceType,
                            icon: Icons.category_rounded,
                            color: SpatialDesignSystem.primaryColor,
                            isDark: isDark,
                          ),
                          const SizedBox(height: 12),
                          _buildDetailCard(
                            context: context,
                            title: 'Cost',
                            value: request.cost != null 
                                ? '${request.cost!.toStringAsFixed(0)} VND'
                                : 'Not specified',
                            icon: Icons.payments_rounded,
                            color: SpatialDesignSystem.successColor,
                            isDark: isDark,
                          ),
                        ],
                      ),
                    ),
                  ],
                ),
                
                const SizedBox(height: 16),
                
                // Description card (full width)
                Container(
                  width: double.infinity,
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: isDark
                        ? Colors.black.withOpacity(0.2)
                        : Colors.white.withOpacity(0.6),
                    borderRadius: BorderRadius.circular(12),
                    border: Border.all(
                      color: isDark
                          ? Colors.white.withOpacity(0.1)
                          : Colors.black.withOpacity(0.05),
                    ),
                  ),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Row(
                        children: [
                          Container(
                            padding: const EdgeInsets.all(8),
                            decoration: BoxDecoration(
                              color: Colors.indigo.withOpacity(0.1),
                              borderRadius: BorderRadius.circular(8),
                            ),
                            child: Icon(
                              Icons.description_rounded,
                              size: 20,
                              color: Colors.indigo,
                            ),
                          ),
                          const SizedBox(width: 10),
                          Text(
                            'Description',
                            style: SpatialDesignSystem.captionText.copyWith(
                              color: isDark
                                  ? SpatialDesignSystem.textDarkSecondaryColor
                                  : SpatialDesignSystem.textSecondaryColor,
                              fontWeight: FontWeight.w700,
                              fontSize: 16, // Cùng size với detail card title
                            ),
                          ),
                        ],
                      ),
                      const SizedBox(height: 12),
                      Text(
                        request.description,
                        style: SpatialDesignSystem.bodyMedium.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                          height: 1.3,
                          fontSize: 14, // Cùng size với detail card content
                        ),
                      ),
                    ],
                  ),
                ),

                // Notes card (garage info) if available
                if (request.notes != null && request.notes!.isNotEmpty) ...[
                  const SizedBox(height: 16),
                  Container(
                    width: double.infinity,
                    padding: const EdgeInsets.all(16),
                    decoration: BoxDecoration(
                      color: isDark
                          ? const Color(0xFF1E293B).withOpacity(0.5)
                          : const Color(0xFFF8FAFC).withOpacity(0.8),
                      borderRadius: BorderRadius.circular(12),
                      border: Border.all(
                        color: isDark
                            ? Colors.white.withOpacity(0.1)
                            : Colors.black.withOpacity(0.05),
                      ),
                    ),
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Row(
                          children: [
                            Container(
                              padding: const EdgeInsets.all(8),
                              decoration: BoxDecoration(
                                color: Colors.amber.withOpacity(0.1),
                                borderRadius: BorderRadius.circular(8),
                              ),
                              child: Icon(
                                Icons.build_rounded,
                                size: 20,
                                color: Colors.amber.shade700,
                              ),
                            ),
                            const SizedBox(width: 10),
                            Text(
                              'Garage & Mechanic',
                              style: SpatialDesignSystem.captionText.copyWith(
                                color: isDark
                                    ? SpatialDesignSystem.textDarkSecondaryColor
                                    : SpatialDesignSystem.textSecondaryColor,
                                fontWeight: FontWeight.w700,
                                fontSize: 16, // Cùng size với detail card title
                              ),
                            ),
                          ],
                        ),
                        const SizedBox(height: 12),
                        Text(
                          request.notes!,
                          style: SpatialDesignSystem.bodyMedium.copyWith(
                            color: isDark
                                ? SpatialDesignSystem.textDarkPrimaryColor
                                : SpatialDesignSystem.textPrimaryColor,
                            height: 1.3,
                            fontSize: 14, // Cùng size với detail card content
                          ),
                        ),
                      ],
                    ),
                  ),
                ],

                // Action buttons với nút Close cải thiện
                const SizedBox(height: 24),
                Row(
                  mainAxisAlignment: MainAxisAlignment.end,
                  children: [
                    OutlinedButton.icon(
                      onPressed: () => Navigator.of(dialogContext).pop(),
                      icon: const Icon(Icons.close_rounded, size: 18),
                      label: const Text('Close'),
                      style: OutlinedButton.styleFrom(
                        backgroundColor: Colors.transparent,
                        foregroundColor: Colors.white,
                        side: const BorderSide(color: Colors.white, width: 1.5),
                        padding: const EdgeInsets.symmetric(horizontal: 20, vertical: 12),
                        shape: RoundedRectangleBorder(
                          borderRadius: BorderRadius.circular(10),
                        ),
                      ),
                    ),
                  ],
                ),
              ],
            ),
          ),
        ),
      ),
    ),
  );
}

// Helper method to build detail cards
Widget _buildDetailCard({
  required BuildContext context,
  required String title,
  required String value,
  required IconData icon,
  required Color color,
  required bool isDark,
}) {
  return Container(
    width: double.infinity,
    padding: const EdgeInsets.all(12), // Giảm padding một chút
    decoration: BoxDecoration(
      color: isDark
          ? Colors.black.withOpacity(0.2)
          : Colors.white.withOpacity(0.6),
      borderRadius: BorderRadius.circular(12),
      border: Border.all(
        color: isDark
            ? Colors.white.withOpacity(0.1)
            : Colors.black.withOpacity(0.05),
      ),
    ),
    child: Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Row(
          children: [
            Container(
              padding: const EdgeInsets.all(8), // Tăng padding icon
              decoration: BoxDecoration(
                color: color.withOpacity(0.1),
                borderRadius: BorderRadius.circular(8),
              ),
              child: Icon(
                icon,
                size: 20, // Tăng size icon
                color: color,
              ),
            ),
            const SizedBox(width: 10), // Tăng spacing
            Expanded(
              child: Text(
                title,
                style: SpatialDesignSystem.captionText.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                  fontWeight: FontWeight.w700,
                  fontSize: 16, // Tăng font size cho title
                ),
                maxLines: 1, // Chỉ cho phép 1 dòng
                overflow: TextOverflow.ellipsis,
              ),
            ),
          ],
        ),
        const SizedBox(height: 12), // Tăng spacing
        Text(
          value,
          style: SpatialDesignSystem.bodySmall.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
            fontWeight: FontWeight.w500,
            height: 1.3,
            fontSize: 14, // Tăng font size cho content
          ),
          maxLines: 2,
          overflow: TextOverflow.ellipsis,
        ),
      ],
    ),
  );
}

// Helper method to format Date only (short format)
String _formatDate(DateTime dateTime) {
  return '${dateTime.day}/${dateTime.month}/${dateTime.year}';
}

// Dialog for driver to pick up vehicle after maintenance (Step 7)
void _showPickUpVehicleDialog(
    BuildContext context, MaintenanceRequest request) {
  final notesController = TextEditingController();
  final isDark = Theme.of(context).brightness == Brightness.dark;

  showDialog(
    context: context,
    builder: (dialogContext) => Dialog(
      backgroundColor: Colors.transparent,
      elevation: 0,
      insetPadding: const EdgeInsets.all(20),
      child: ConstrainedBox(
        constraints: BoxConstraints(
          maxHeight: MediaQuery.of(context).size.height * 0.7,
          maxWidth: MediaQuery.of(context).size.width * 0.9,
        ),
        child: GlassCard(
          padding: const EdgeInsets.all(24),
          gradient: LinearGradient(
            colors: [
              SpatialDesignSystem.successColor.withOpacity(0.1),
              SpatialDesignSystem.primaryColor.withOpacity(0.05),
            ],
            begin: Alignment.topLeft,
            end: Alignment.bottomRight,
          ),
          child: SingleChildScrollView(
            child: Column(
              mainAxisSize: MainAxisSize.min,
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                // Header with icon and title
                Row(
                  children: [
                    Container(
                      padding: const EdgeInsets.all(12),
                      decoration: BoxDecoration(
                        color: SpatialDesignSystem.successColor.withOpacity(0.15),
                        borderRadius: BorderRadius.circular(12),
                      ),
                      child: Icon(
                        Icons.car_rental_rounded,
                        size: 24,
                        color: SpatialDesignSystem.successColor,
                      ),
                    ),
                    const SizedBox(width: 12),
                    Expanded(
                      child: Text(
                        'Pick Up Vehicle',
                        style: SpatialDesignSystem.subtitleLarge.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                          fontWeight: FontWeight.w700,
                        ),
                      ),
                    ),
                  ],
                ),
                
                const SizedBox(height: 20),
                const Divider(),
                const SizedBox(height: 16),

                // Vehicle info card
                Container(
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: isDark
                        ? Colors.black.withOpacity(0.2)
                        : Colors.white.withOpacity(0.6),
                    borderRadius: BorderRadius.circular(12),
                    border: Border.all(
                      color: isDark
                          ? Colors.white.withOpacity(0.1)
                          : Colors.black.withOpacity(0.05),
                    ),
                  ),
                  child: Row(
                    children: [
                      Container(
                        padding: const EdgeInsets.all(8),
                        decoration: BoxDecoration(
                          color: SpatialDesignSystem.primaryColor.withOpacity(0.1),
                          borderRadius: BorderRadius.circular(8),
                        ),
                        child: Icon(
                          Icons.directions_car,
                          size: 20,
                          color: SpatialDesignSystem.primaryColor,
                        ),
                      ),
                      const SizedBox(width: 12),
                      Expanded(
                        child: Column(
                          crossAxisAlignment: CrossAxisAlignment.start,
                          children: [
                            Text(
                              'Vehicle License Plate',
                              style: SpatialDesignSystem.captionText.copyWith(
                                color: isDark
                                    ? SpatialDesignSystem.textDarkSecondaryColor
                                    : SpatialDesignSystem.textSecondaryColor,
                                fontWeight: FontWeight.w600,
                              ),
                            ),
                            const SizedBox(height: 4),
                            Text(
                              request.vehicle?.licensePlate ?? 'N/A',
                              style: SpatialDesignSystem.bodyMedium.copyWith(
                                color: isDark
                                    ? SpatialDesignSystem.textDarkPrimaryColor
                                    : SpatialDesignSystem.textPrimaryColor,
                                fontWeight: FontWeight.w600,
                              ),
                            ),
                          ],
                        ),
                      ),
                    ],
                  ),
                ),

                const SizedBox(height: 20),

                // Confirmation message
                Container(
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: SpatialDesignSystem.successColor.withOpacity(0.1),
                    borderRadius: BorderRadius.circular(12),
                    border: Border.all(
                      color: SpatialDesignSystem.successColor.withOpacity(0.3),
                    ),
                  ),
                  child: Row(
                    children: [
                      Icon(
                        Icons.check_circle_outline,
                        color: SpatialDesignSystem.successColor,
                        size: 20,
                      ),
                      const SizedBox(width: 10),
                      Expanded(
                        child: Text(
                          'Confirm picking up vehicle after maintenance is completed.',
                          style: SpatialDesignSystem.bodyMedium.copyWith(
                            color: isDark
                                ? SpatialDesignSystem.textDarkPrimaryColor
                                : SpatialDesignSystem.textPrimaryColor,
                          ),
                        ),
                      ),
                    ],
                  ),
                ),

                const SizedBox(height: 20),

                // Notes input field
                Text(
                  'Pickup Notes (optional)',
                  style: SpatialDesignSystem.bodyMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                    fontWeight: FontWeight.w600,
                  ),
                ),
                const SizedBox(height: 8),
                Container(
                  decoration: BoxDecoration(
                    borderRadius: BorderRadius.circular(12),
                    boxShadow: [
                      BoxShadow(
                        color: Colors.black.withOpacity(0.05),
                        blurRadius: 10,
                        offset: const Offset(0, 2),
                      ),
                    ],
                  ),
                  child: TextField(
                    controller: notesController,
                    decoration: InputDecoration(
                      filled: true,
                      fillColor: isDark
                          ? SpatialDesignSystem.darkSurfaceColorSecondary
                          : Colors.white.withOpacity(0.8),
                      border: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(12),
                        borderSide: BorderSide.none,
                      ),
                      enabledBorder: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(12),
                        borderSide: BorderSide(
                          color: isDark
                              ? Colors.white.withOpacity(0.1)
                              : Colors.black.withOpacity(0.1),
                        ),
                      ),
                      focusedBorder: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(12),
                        borderSide: BorderSide(
                          color: SpatialDesignSystem.primaryColor,
                          width: 2,
                        ),
                      ),
                      hintText: 'Add notes when picking up the vehicle...',
                      hintStyle: TextStyle(
                        color: isDark
                            ? Colors.white.withOpacity(0.5)
                            : Colors.black.withOpacity(0.5),
                      ),
                      contentPadding: const EdgeInsets.all(16),
                    ),
                    style: SpatialDesignSystem.bodyMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                    maxLines: 3,
                    minLines: 2,
                  ),
                ),

                const SizedBox(height: 24),

                // Action buttons
                Row(
                  children: [
                    Expanded(
                      child: OutlinedButton(
                        onPressed: () => Navigator.of(dialogContext).pop(),
                        style: OutlinedButton.styleFrom(
                          padding: const EdgeInsets.symmetric(vertical: 16),
                          side: BorderSide(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                          ),
                          shape: RoundedRectangleBorder(
                            borderRadius: BorderRadius.circular(12),
                          ),
                        ),
                        child: Text(
                          'Cancel',
                          style: SpatialDesignSystem.bodyMedium.copyWith(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                            fontWeight: FontWeight.w600,
                          ),
                        ),
                      ),
                    ),
                    const SizedBox(width: 12),
                    Expanded(
                      flex: 2,
                      child: ElevatedButton.icon(
                        onPressed: () {
                          // Update status to IN_USE (18)
                          context.read<MaintenanceBloc>().add(
                                UpdateMaintenanceRequest(
                                  driverId: 0, // Not used in our updated service
                                  maintenanceId: request.id,
                                  updateDto: CreateMaintenanceRequestDto(
                                    vehicleId: request.vehicle?.id ?? 0,
                                    description: request.description,
                                    maintenanceType: request.maintenanceType,
                                    statusId: 18, // IN_USE status
                                    notes: notesController.text.trim().isNotEmpty
                                        ? notesController.text.trim()
                                        : null,
                                  ),
                                ),
                              );

                          Navigator.of(dialogContext).pop();
                          ScaffoldMessenger.of(context).showSnackBar(
                            SnackBar(
                              content: const Text('Vehicle picked up successfully'),
                              backgroundColor: SpatialDesignSystem.successColor,
                            ),
                          );
                        },
                        icon: const Icon(Icons.check_circle, size: 20),
                        label: const Text('Confirm Pickup'),
                        style: ElevatedButton.styleFrom(
                          backgroundColor: SpatialDesignSystem.successColor,
                          foregroundColor: Colors.white,
                          padding: const EdgeInsets.symmetric(vertical: 16),
                          elevation: 2,
                          shadowColor: SpatialDesignSystem.successColor.withOpacity(0.3),
                          shape: RoundedRectangleBorder(
                            borderRadius: BorderRadius.circular(12),
                          ),
                        ),
                      ),
                    ),
                  ],
                ),
              ],
            ),
          ),
        ),
      ),
    ),
  );
}

// Enhanced Dialog for canceling a maintenance request - Spatial UI Dark Theme
void _showCancelMaintenanceDialog(
    BuildContext context, MaintenanceRequest request) {
  final reasonController = TextEditingController();
  final isDark = Theme.of(context).brightness == Brightness.dark;

  showDialog(
    context: context,
    builder: (dialogContext) => Dialog(
      backgroundColor: Colors.transparent,
      elevation: 0,
      child: ConstrainedBox(
        constraints: BoxConstraints(
          maxHeight: MediaQuery.of(context).size.height * 0.7,
          maxWidth: MediaQuery.of(context).size.width * 0.9,
        ),
        child: GlassCard(
          padding: const EdgeInsets.all(24),
          gradient: LinearGradient(
            colors: [
              Colors.red.withOpacity(0.1),
              SpatialDesignSystem.errorColor.withOpacity(0.05),
            ],
            begin: Alignment.topLeft,
            end: Alignment.bottomRight,
          ),
          child: SingleChildScrollView(
            child: Column(
              mainAxisSize: MainAxisSize.min,
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                // Header with icon and title
                Row(
                  children: [
                    Container(
                      padding: const EdgeInsets.all(12),
                      decoration: BoxDecoration(
                        color: SpatialDesignSystem.errorColor.withOpacity(0.15),
                        borderRadius: BorderRadius.circular(12),
                      ),
                      child: Icon(
                        Icons.cancel_outlined,
                        size: 24,
                        color: SpatialDesignSystem.errorColor,
                      ),
                    ),
                    const SizedBox(width: 12),
                    Expanded(
                      child: Text(
                        'Cancel Maintenance',
                        style: SpatialDesignSystem.subtitleLarge.copyWith(
                          color: isDark
                              ? SpatialDesignSystem.textDarkPrimaryColor
                              : SpatialDesignSystem.textPrimaryColor,
                          fontWeight: FontWeight.w700,
                        ),
                      ),
                    ),
                  ],
                ),
                
                const SizedBox(height: 20),
                const Divider(),
                const SizedBox(height: 16),

                // Vehicle info card
                Container(
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: isDark
                        ? Colors.black.withOpacity(0.2)
                        : Colors.white.withOpacity(0.6),
                    borderRadius: BorderRadius.circular(12),
                    border: Border.all(
                      color: isDark
                          ? Colors.white.withOpacity(0.1)
                          : Colors.black.withOpacity(0.05),
                    ),
                  ),
                  child: Row(
                    children: [
                      Container(
                        padding: const EdgeInsets.all(8),
                        decoration: BoxDecoration(
                          color: SpatialDesignSystem.primaryColor.withOpacity(0.1),
                          borderRadius: BorderRadius.circular(8),
                        ),
                        child: Icon(
                          Icons.directions_car_rounded,
                          size: 20,
                          color: SpatialDesignSystem.primaryColor,
                        ),
                      ),
                      const SizedBox(width: 12),
                      Expanded(
                        child: Column(
                          crossAxisAlignment: CrossAxisAlignment.start,
                          children: [
                            Text(
                              'Vehicle License Plate',
                              style: SpatialDesignSystem.captionText.copyWith(
                                color: isDark
                                    ? SpatialDesignSystem.textDarkSecondaryColor
                                    : SpatialDesignSystem.textSecondaryColor,
                              ),
                            ),
                            const SizedBox(height: 4),
                            Text(
                              request.vehicle?.licensePlate ?? 'N/A',
                              style: SpatialDesignSystem.bodyLarge.copyWith(
                                color: isDark
                                    ? SpatialDesignSystem.textDarkPrimaryColor
                                    : SpatialDesignSystem.textPrimaryColor,
                                fontWeight: FontWeight.w600,
                              ),
                            ),
                          ],
                        ),
                      ),
                    ],
                  ),
                ),

                const SizedBox(height: 20),

                // Warning message
                Container(
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: Colors.orange.withOpacity(0.1),
                    borderRadius: BorderRadius.circular(12),
                    border: Border.all(
                      color: Colors.orange.withOpacity(0.3),
                    ),
                  ),
                  child: Row(
                    children: [
                      Icon(
                        Icons.warning_rounded,
                        color: Colors.orange.shade600,
                        size: 20,
                      ),
                      const SizedBox(width: 10),
                      Expanded(
                        child: Text(
                          'Be careful! This action cannot be undone.',
                          style: SpatialDesignSystem.bodyMedium.copyWith(
                            color: Colors.orange.shade700,
                            fontWeight: FontWeight.w500,
                          ),
                        ),
                      ),
                    ],
                  ),
                ),

                const SizedBox(height: 20),

                // Reason input field
                Text(
                  'Reason for Cancellation *',
                  style: SpatialDesignSystem.bodyMedium.copyWith(
                    color: isDark
                        ? SpatialDesignSystem.textDarkPrimaryColor
                        : SpatialDesignSystem.textPrimaryColor,
                    fontWeight: FontWeight.w600,
                  ),
                ),
                const SizedBox(height: 8),
                Container(
                  decoration: BoxDecoration(
                    borderRadius: BorderRadius.circular(12),
                    boxShadow: [
                      BoxShadow(
                        color: Colors.black.withOpacity(0.05),
                        blurRadius: 8,
                        offset: const Offset(0, 2),
                      ),
                    ],
                  ),
                  child: TextField(
                    controller: reasonController,
                    decoration: InputDecoration(
                      filled: true,
                      fillColor: isDark
                          ? Colors.black.withOpacity(0.3)
                          : Colors.white.withOpacity(0.8),
                      border: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(12),
                        borderSide: BorderSide(
                          color: isDark
                              ? Colors.white.withOpacity(0.1)
                              : Colors.black.withOpacity(0.1),
                        ),
                      ),
                      enabledBorder: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(12),
                        borderSide: BorderSide(
                          color: isDark
                              ? Colors.white.withOpacity(0.1)
                              : Colors.black.withOpacity(0.1),
                        ),
                      ),
                      focusedBorder: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(12),
                        borderSide: BorderSide(
                          color: SpatialDesignSystem.primaryColor,
                          width: 2,
                        ),
                      ),
                      hintText: 'Enter detailed reason for cancelling this maintenance request...',
                      hintStyle: TextStyle(
                        color: isDark
                            ? Colors.white.withOpacity(0.5)
                            : Colors.black.withOpacity(0.5),
                      ),
                      contentPadding: const EdgeInsets.all(16),
                    ),
                    style: SpatialDesignSystem.bodyMedium.copyWith(
                      color: isDark
                          ? SpatialDesignSystem.textDarkPrimaryColor
                          : SpatialDesignSystem.textPrimaryColor,
                    ),
                    maxLines: 3,
                    minLines: 2,
                  ),
                ),

                const SizedBox(height: 24),

                // Action buttons
                Row(
                  children: [
                    Expanded(
                      child: OutlinedButton(
                        onPressed: () => Navigator.of(dialogContext).pop(),
                        style: OutlinedButton.styleFrom(
                          padding: const EdgeInsets.symmetric(vertical: 16),
                          side: BorderSide(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                          ),
                          shape: RoundedRectangleBorder(
                            borderRadius: BorderRadius.circular(12),
                          ),
                        ),
                        child: Text(
                          'Close',
                          style: SpatialDesignSystem.bodyMedium.copyWith(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                            fontWeight: FontWeight.w600,
                          ),
                        ),
                      ),
                    ),
                    const SizedBox(width: 16),
                    Expanded(
                      child: ElevatedButton(
                        onPressed: () {
                          if (reasonController.text.trim().isEmpty) {
                            ScaffoldMessenger.of(context).showSnackBar(
                              SnackBar(
                                content: const Text('Please enter reason for cancellation'),
                                backgroundColor: SpatialDesignSystem.errorColor,
                              ),
                            );
                            return;
                          }

                          // Cancel the request
                          context.read<MaintenanceBloc>().add(
                                DeleteMaintenanceRequest(
                                  driverId: 0, // Not used in our updated service
                                  maintenanceId: request.id,
                                ),
                              );

                          Navigator.of(dialogContext).pop();
                        },
                        style: ElevatedButton.styleFrom(
                          backgroundColor: SpatialDesignSystem.errorColor,
                          padding: const EdgeInsets.symmetric(vertical: 16),
                          elevation: 0,
                          shape: RoundedRectangleBorder(
                            borderRadius: BorderRadius.circular(12),
                          ),
                        ),
                        child: Row(
                          mainAxisAlignment: MainAxisAlignment.center,
                          children: [
                            const Icon(
                              Icons.cancel_outlined,
                              size: 18,
                              color: Colors.white,
                            ),
                            const SizedBox(width: 8),
                            Text(
                              'Confirm',
                              style: SpatialDesignSystem.bodyMedium.copyWith(
                                color: Colors.white,
                                fontWeight: FontWeight.w600,
                              ),
                            ),
                          ],
                        ),
                      ),
                    ),
                  ],
                ),
              ],
            ),
          ),
        ),
      ),
    ),
  );
}

// Tab for pending maintenance requests (Status 51)
class PendingMaintenanceTab extends StatelessWidget {
  const PendingMaintenanceTab({super.key});

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: BlocBuilder<MaintenanceBloc, MaintenanceState>(
        builder: (context, state) {
          if (state is MaintenanceLoading && state is! MaintenanceLoaded) {
            return const Center(child: CircularProgressIndicator());
          } else if (state is MaintenanceLoaded) {
            final pendingRequests = state.requests
                .where((request) => request.status?.id == 51)
                .toList();

            if (pendingRequests.isEmpty) {
              return Center(
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    const Icon(Icons.pending_actions,
                        size: 80, color: Colors.grey),
                    const SizedBox(height: 16),
                    Text(
                      'No pending requests',
                      style: SpatialDesignSystem.subtitleLarge,
                    ),
                    // const SizedBox(height: 8),
                    // Text(
                    //   'No maintenance requests are currently pending approval',
                    //   style: SpatialDesignSystem.bodyMedium,
                    //   textAlign: TextAlign.center,
                    // ),
                  ],
                ),
              );
            }

            return Column(
              children: [
                for (int i = 0; i < pendingRequests.length; i++)
                  Padding(
                    padding: const EdgeInsets.only(bottom: 16),
                    child: _buildMaintenanceCard(context, pendingRequests[i]),
                  ),
              ],
            );
          } else if (state is MaintenanceError) {
            return Center(child: Text(state.message));
          } else {
            return const Center(child: Text('No pending requests available'));
          }
        },
      ),
    );
  }
}

// Tab for maintenance requests under maintenance (Status 19)
class UnderMaintenanceTab extends StatelessWidget {
  const UnderMaintenanceTab({super.key});

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: BlocBuilder<MaintenanceBloc, MaintenanceState>(
        builder: (context, state) {
          if (state is MaintenanceLoading && state is! MaintenanceLoaded) {
            return const Center(child: CircularProgressIndicator());
          } else if (state is MaintenanceLoaded) {
            final underMaintenanceRequests = state.requests
                .where((request) => request.status?.id == 19)
                .toList();

            if (underMaintenanceRequests.isEmpty) {
              return Center(
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    const Icon(Icons.build, size: 80, color: Colors.grey),
                    const SizedBox(height: 16),
                    Text(
                      'No vehicles under maintenance',
                      style: SpatialDesignSystem.subtitleLarge,
                    ),
                    // const SizedBox(height: 8),
                    // Text(
                    //   'No vehicles are currently being maintained',
                    //   style: SpatialDesignSystem.bodyMedium,
                    //   textAlign: TextAlign.center,
                    // ),
                  ],
                ),
              );
            }

            return Column(
              children: [
                for (int i = 0; i < underMaintenanceRequests.length; i++)
                  Padding(
                    padding: const EdgeInsets.only(bottom: 16),
                    child: _buildMaintenanceCard(
                        context, underMaintenanceRequests[i]),
                  ),
              ],
            );
          } else if (state is MaintenanceError) {
            return Center(child: Text(state.message));
          } else {
            return const Center(
                child: Text('No maintenance activities available'));
          }
        },
      ),
    );
  }
}

// Tab for completed maintenance requests (Status 17)
class CompletedMaintenanceTab extends StatelessWidget {
  const CompletedMaintenanceTab({super.key});

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16),
      child: BlocBuilder<MaintenanceBloc, MaintenanceState>(
        builder: (context, state) {
          if (state is MaintenanceLoading && state is! MaintenanceLoaded) {
            return const Center(child: CircularProgressIndicator());
          } else if (state is MaintenanceLoaded) {
            final completedRequests = state.requests
                .where((request) => request.status?.id == 17)
                .toList();

            if (completedRequests.isEmpty) {
              return Center(
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    const Icon(Icons.check_circle,
                        size: 80, color: Colors.grey),
                    const SizedBox(height: 16),
                    Text(
                      'No completed maintenance',
                      style: SpatialDesignSystem.subtitleLarge,
                    ),
                    // const SizedBox(height: 8),
                    // Text(
                    //   'No maintenance requests have been completed yet',
                    //   style: SpatialDesignSystem.bodyMedium,
                    //   textAlign: TextAlign.center,
                    // ),
                  ],
                ),
              );
            }

            return Column(
              children: [
                for (int i = 0; i < completedRequests.length; i++)
                  Padding(
                    padding: const EdgeInsets.only(bottom: 16),
                    child: _buildMaintenanceCard(context, completedRequests[i]),
                  ),
              ],
            );
          } else if (state is MaintenanceError) {
            return Center(child: Text(state.message));
          } else {
            return const Center(
                child: Text('No completed maintenance available'));
          }
        },
      ),
    );
  }
}

// Shared function to build maintenance cards - Compact Spatial UI design
Widget _buildMaintenanceCard(BuildContext context, MaintenanceRequest request) {
  final isDark = Theme.of(context).brightness == Brightness.dark;

  return GlassCard(
    padding: const EdgeInsets.all(0), // Remove padding để control layout tốt hơn
    gradient: LinearGradient(
      colors: [
        SpatialDesignSystem.primaryColor.withOpacity(0.03),
        SpatialDesignSystem.accentColor.withOpacity(0.01),
      ],
      begin: Alignment.topLeft,
      end: Alignment.bottomRight,
    ),
    child: Column(
      crossAxisAlignment: CrossAxisAlignment.stretch,
      children: [
        // Header với gradient đẹp mắt hơn chứa mã và thời gian
        Container(
          padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 12),
          decoration: BoxDecoration(
            gradient: LinearGradient(
              colors: isDark
                  ? [
                      const Color(0xFF1A1B2E).withOpacity(0.9),
                      const Color(0xFF16213E).withOpacity(0.7),
                    ]
                  : [
                      const Color(0xFF6366F1).withOpacity(0.1),
                      const Color(0xFF8B5CF6).withOpacity(0.05),
                    ],
              begin: Alignment.topLeft,
              end: Alignment.bottomRight,
            ),
            borderRadius: const BorderRadius.only(
              topLeft: Radius.circular(12),
              topRight: Radius.circular(12),
            ),
            border: Border(
              bottom: BorderSide(
                color: isDark
                    ? Colors.white.withOpacity(0.1)
                    : Colors.black.withOpacity(0.05),
                width: 1,
              ),
            ),
          ),
          child: Row(
            children: [
              // Vehicle info với icon đẹp hơn
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 10, vertical: 6),
                decoration: BoxDecoration(
                  color: isDark
                      ? SpatialDesignSystem.primaryColor.withOpacity(0.2)
                      : SpatialDesignSystem.primaryColor.withOpacity(0.1),
                  borderRadius: BorderRadius.circular(8),
                  border: Border.all(
                    color: SpatialDesignSystem.primaryColor.withOpacity(0.3),
                    width: 1,
                  ),
                ),
                child: Row(
                  mainAxisSize: MainAxisSize.min,
                  children: [
                    Icon(
                      Icons.local_shipping_rounded,
                      size: 16,
                      color: SpatialDesignSystem.primaryColor,
                    ),
                    const SizedBox(width: 6),
                    Text(
                      request.vehicle?.licensePlate ?? 'N/A',
                      style: SpatialDesignSystem.bodyMedium.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkPrimaryColor
                            : SpatialDesignSystem.textPrimaryColor,
                        fontWeight: FontWeight.w600,
                      ),
                    ),
                  ],
                ),
              ),
              const Spacer(),
              // Time info với style mới
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 4),
                decoration: BoxDecoration(
                  color: isDark
                      ? Colors.white.withOpacity(0.08)
                      : Colors.black.withOpacity(0.04),
                  borderRadius: BorderRadius.circular(6),
                ),
                child: Row(
                  mainAxisSize: MainAxisSize.min,
                  children: [
                    Icon(
                      Icons.schedule_rounded,
                      size: 14,
                      color: isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor,
                    ),
                    const SizedBox(width: 4),
                    Text(
                      _formatTime(request.createdAt),
                      style: SpatialDesignSystem.captionText.copyWith(
                        color: isDark
                            ? SpatialDesignSystem.textDarkSecondaryColor
                            : SpatialDesignSystem.textSecondaryColor,
                        fontWeight: FontWeight.w500,
                      ),
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
        
        // Content area
        Padding(
          padding: const EdgeInsets.all(16),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              // Status và Emergency badge
              Row(
                children: [
                  _buildStatusBadge(request.status, isDark),
                  const Spacer(),
                  if (request.description.toLowerCase().contains('emergency') ||
                      request.description.toLowerCase().contains('khẩn cấp')) ...[
                    Container(
                      padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 4),
                      decoration: BoxDecoration(
                        gradient: LinearGradient(
                          colors: [
                            Colors.red.withOpacity(0.15),
                            Colors.orange.withOpacity(0.1),
                          ],
                        ),
                        borderRadius: BorderRadius.circular(12),
                        border: Border.all(
                          color: Colors.red.withOpacity(0.4),
                          width: 1,
                        ),
                      ),
                      child: Row(
                        mainAxisSize: MainAxisSize.min,
                        children: [
                          Icon(
                            Icons.priority_high_rounded,
                            size: 14,
                            color: Colors.red.shade600,
                          ),
                          const SizedBox(width: 4),
                          Text(
                            'Emergency',
                            style: SpatialDesignSystem.captionText.copyWith(
                              color: Colors.red.shade600,
                              fontWeight: FontWeight.w600,
                              fontSize: 11,
                            ),
                          ),
                        ],
                      ),
                    ),
                  ],
                ],
              ),
              const SizedBox(height: 14),
              
              // Title với style mới
              Text(
                request.description,
                style: SpatialDesignSystem.subtitleMedium.copyWith(
                  color: isDark
                      ? SpatialDesignSystem.textDarkPrimaryColor
                      : SpatialDesignSystem.textPrimaryColor,
                  fontWeight: FontWeight.w600,
                  height: 1.3,
                ),
                maxLines: 2,
                overflow: TextOverflow.ellipsis,
              ),
              
              // Notes section với design mới
              if (request.notes != null && request.notes!.isNotEmpty) ...[
                const SizedBox(height: 12),
                Container(
                  padding: const EdgeInsets.all(12),
                  decoration: BoxDecoration(
                    color: isDark
                        ? const Color(0xFF1E293B).withOpacity(0.5)
                        : const Color(0xFFF8FAFC).withOpacity(0.8),
                    borderRadius: BorderRadius.circular(10),
                    border: Border.all(
                      color: isDark
                          ? Colors.white.withOpacity(0.08)
                          : Colors.black.withOpacity(0.06),
                    ),
                  ),
                  child: Row(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Container(
                        padding: const EdgeInsets.all(4),
                        decoration: BoxDecoration(
                          color: isDark
                              ? SpatialDesignSystem.accentColor.withOpacity(0.2)
                              : SpatialDesignSystem.accentColor.withOpacity(0.1),
                          borderRadius: BorderRadius.circular(6),
                        ),
                        child: Icon(
                          Icons.sticky_note_2_rounded,
                          size: 14,
                          color: SpatialDesignSystem.accentColor,
                        ),
                      ),
                      const SizedBox(width: 10),
                      Expanded(
                        child: Text(
                          request.notes!,
                          style: SpatialDesignSystem.bodySmall.copyWith(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                            height: 1.4,
                          ),
                          maxLines: 2,
                          overflow: TextOverflow.ellipsis,
                        ),
                      ),
                    ],
                  ),
                ),
              ],
              
              // Action buttons
              const SizedBox(height: 16),
              _buildActionButtons(context, request),
            ],
          ),
        ),
      ],
    ),
  );
}

Widget _buildStatusBadge(StatusInfo? status, bool isDark) {
  if (status == null) {
    return Container(
      padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
      decoration: BoxDecoration(
        color: (isDark
                ? SpatialDesignSystem.textDarkSecondaryColor
                : SpatialDesignSystem.textSecondaryColor)
            .withOpacity(0.1),
        borderRadius: BorderRadius.circular(16),
        border: Border.all(
            color: (isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor)
                .withOpacity(0.3)),
      ),
      child: Row(
        mainAxisSize: MainAxisSize.min,
        children: [
          const Icon(Icons.help_outline, size: 14, color: Colors.grey),
          const SizedBox(width: 4),
          Text(
            'Unknown',
            style: SpatialDesignSystem.captionText.copyWith(
              color: Colors.grey,
              fontWeight: FontWeight.w600,
            ),
          ),
        ],
      ),
    );
  }

  Color color;
  IconData icon;

  // Map status based on ID
  switch (status.id) {
    case 51: // MAINTENANCE_PENDING - pending
      color = SpatialDesignSystem.warningColor;
      icon = Icons.pending;
      break;
    case 18: // IN_USE - accepted
      color = Colors.blue;
      icon = Icons.check_circle_outline;
      break;
    case 19: // MAINTENANCE - under maintenance
      color = SpatialDesignSystem.primaryColor;
      icon = Icons.build;
      break;
    case 17: // AVAILABLE - maintenance completed
      color = SpatialDesignSystem.successColor;
      icon = Icons.check_circle;
      break;
    default:
      color = SpatialDesignSystem.textSecondaryColor;
      icon = Icons.help_outline;
      break;
  }

  return Container(
    padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
    decoration: BoxDecoration(
      color: color.withOpacity(0.1),
      borderRadius: BorderRadius.circular(16),
      border: Border.all(color: color.withOpacity(0.3)),
    ),
    child: Row(
      mainAxisSize: MainAxisSize.min,
      children: [
        Icon(icon, size: 14, color: color),
        const SizedBox(width: 4),
        Text(
          _getStatusDisplayName(status),
          style: SpatialDesignSystem.captionText.copyWith(
            color: color,
            fontWeight: FontWeight.w600,
          ),
        ),
      ],
    ),
  );
}

String _getStatusDisplayName(StatusInfo status) {
  switch (status.id) {
    case 51: // MAINTENANCE_PENDING
      return 'Pending';
    case 19: // MAINTENANCE
      return 'Under Maintenance';
    case 17: // AVAILABLE
      return 'Maintenance Completed';
    case 18: // IN_USE
      return 'Accepted';
    default:
      return status.name; // Fallback to API name
  }
}

String _formatTime(DateTime dateTime) {
  final now = DateTime.now();
  final difference = now.difference(dateTime);

  if (difference.inDays > 0) {
    return '${difference.inDays} days ago';
  } else if (difference.inHours > 0) {
    return '${difference.inHours} hours ago';
  } else if (difference.inMinutes > 0) {
    return '${difference.inMinutes} minutes ago';
  } else {
    return 'Just now';
  }
}

// Widget to show action buttons based on maintenance request status
Widget _buildActionButtons(BuildContext context, MaintenanceRequest request) {
  final status = request.status?.id;

  // No status or no need for actions
  if (status == null) {
    return const SizedBox.shrink();
  }

  // Check if the request has been approved by checking for specific content in notes
  bool isApproved = false;
  if (request.notes != null) {
    // Check if notes contains garage location and contact info, indicating approval
    isApproved = request.notes!.contains("Garage") &&
        (request.notes!.contains("Thợ") || request.notes!.contains("0"));
  }

  // Scheduled/Accepted - Status 18 (IN_USE)
  // Show "Take vehicle to garage" button for scheduled maintenance
  if (status == 18 && isApproved) {
    return Row(
      mainAxisAlignment: MainAxisAlignment.end,
      children: [
        OutlinedButton.icon(
          onPressed: () => _showTakeToGarageDialog(context, request),
          icon: const Icon(Icons.garage_outlined),
          label: const Text('Take vehicle to garage'),
          style: OutlinedButton.styleFrom(
            foregroundColor: SpatialDesignSystem.primaryColor,
            side: BorderSide(color: SpatialDesignSystem.primaryColor),
            padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
          ),
        ),
      ],
    );
  }

  // Under Maintenance - Status 19 (MAINTENANCE)
  // Show "View Details" button since driver is waiting
  else if (status == 19) {
    return Row(
      mainAxisAlignment: MainAxisAlignment.end,
      children: [
        OutlinedButton.icon(
          onPressed: () => _showMaintenanceDetailDialog(context, request),
          icon: const Icon(Icons.info_outline),
          label: const Text('View details'),
          style: OutlinedButton.styleFrom(
            foregroundColor: Colors.blue,
            side: BorderSide(color: Colors.blue),
            padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
          ),
        ),
      ],
    );
  }

  // Completed, ready for pickup - Status 17 (AVAILABLE)
  // Show "Pick up vehicle" button
  else if (status == 17) {
    return Row(
      mainAxisAlignment: MainAxisAlignment.end,
      children: [
        ElevatedButton.icon(
          onPressed: () => _showPickUpVehicleDialog(context, request),
          icon: const Icon(Icons.check),
          label: const Text('Pick up vehicle'),
          style: ElevatedButton.styleFrom(
            backgroundColor: SpatialDesignSystem.successColor,
            foregroundColor: Colors.white,
            padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
          ),
        ),
      ],
    );
  }

  // Pending - Status 51 (MAINTENANCE_PENDING)
  // Show "Cancel" button
  else if (status == 51) {
    return Row(
      mainAxisAlignment: MainAxisAlignment.end,
      children: [
        OutlinedButton.icon(
          onPressed: () => _showCancelMaintenanceDialog(context, request),
          icon: const Icon(Icons.cancel_outlined),
          label: const Text('Cancel request'),
          style: OutlinedButton.styleFrom(
            foregroundColor: SpatialDesignSystem.errorColor,
            side: BorderSide(color: SpatialDesignSystem.errorColor),
            padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
          ),
        ),
      ],
    );
  }

  return const SizedBox.shrink();
}
