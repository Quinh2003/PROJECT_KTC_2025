import 'package:flutter_bloc/flutter_bloc.dart';
import '../../../services/maintenance_services.dart';
import '../../../domain/models/maintenance/maintenance_request.dart';

// Data Transfer Object for creating maintenance requests
class CreateMaintenanceRequestDto {
  final int vehicleId;
  final String description;
  final String maintenanceType;
  final int statusId;
  final double? cost;
  final String? maintenanceDate;
  final String? nextDueDate;
  final String? notes;

  const CreateMaintenanceRequestDto({
    required this.vehicleId,
    required this.description,
    required this.maintenanceType,
    required this.statusId,
    this.cost,
    this.maintenanceDate,
    this.nextDueDate,
    this.notes,
  });
}

// States
abstract class MaintenanceState {
  const MaintenanceState();
}

class MaintenanceInitial extends MaintenanceState {
  const MaintenanceInitial();
}

class MaintenanceLoading extends MaintenanceState {
  const MaintenanceLoading();
}

class MaintenanceLoaded extends MaintenanceState {
  final List<MaintenanceRequest> requests;
  final int totalPages;
  final int currentPage;

  const MaintenanceLoaded({
    required this.requests,
    required this.totalPages,
    required this.currentPage,
  });

  MaintenanceLoaded copyWith({
    List<MaintenanceRequest>? requests,
    int? totalPages,
    int? currentPage,
  }) {
    return MaintenanceLoaded(
      requests: requests ?? this.requests,
      totalPages: totalPages ?? this.totalPages,
      currentPage: currentPage ?? this.currentPage,
    );
  }
}

class MaintenanceError extends MaintenanceState {
  final String message;
  final String? details;

  const MaintenanceError({
    required this.message,
    this.details,
  });
}

class MaintenanceCreating extends MaintenanceState {
  const MaintenanceCreating();
}

class MaintenanceCreated extends MaintenanceState {
  final MaintenanceRequest request;

  const MaintenanceCreated({required this.request});
}

// Events
abstract class MaintenanceEvent {
  const MaintenanceEvent();
}

class LoadMaintenanceRequests extends MaintenanceEvent {
  final int page;
  final int size;
  final int? statusIdFilter;
  final bool? isEmergencyFilter;

  const LoadMaintenanceRequests({
    this.page = 1,
    this.size = 10,
    this.statusIdFilter,
    this.isEmergencyFilter,
  });
}

class RefreshMaintenanceRequests extends MaintenanceEvent {
  const RefreshMaintenanceRequests();
}

class CreateMaintenanceRequest extends MaintenanceEvent {
  final int driverId;
  final CreateMaintenanceRequestDto createDto;

  const CreateMaintenanceRequest({
    required this.driverId,
    required this.createDto,
  });
}

class UpdateMaintenanceRequest extends MaintenanceEvent {
  final int driverId;
  final int maintenanceId;
  final CreateMaintenanceRequestDto updateDto;

  const UpdateMaintenanceRequest({
    required this.driverId,
    required this.maintenanceId,
    required this.updateDto,
  });
}

class DeleteMaintenanceRequest extends MaintenanceEvent {
  final int driverId;
  final int maintenanceId;

  const DeleteMaintenanceRequest({
    required this.driverId,
    required this.maintenanceId,
  });
}

// BLoC
class MaintenanceBloc extends Bloc<MaintenanceEvent, MaintenanceState> {
  final MaintenanceServices _apiService;

  MaintenanceBloc({
    required MaintenanceServices apiService,
  }) : _apiService = apiService, super(const MaintenanceInitial()) {
    print('🏗️ MaintenanceBloc: Constructor called');
    
    on<LoadMaintenanceRequests>(_onLoadMaintenanceRequests);
    print('🏗️ MaintenanceBloc: LoadMaintenanceRequests handler registered');
    
    on<RefreshMaintenanceRequests>(_onRefreshMaintenanceRequests);
    print('🏗️ MaintenanceBloc: RefreshMaintenanceRequests handler registered');
    
    on<CreateMaintenanceRequest>(_onCreateMaintenanceRequest);
    on<UpdateMaintenanceRequest>(_onUpdateMaintenanceRequest);
    on<DeleteMaintenanceRequest>(_onDeleteMaintenanceRequest);
    
    print('🏗️ MaintenanceBloc: All event handlers registered');
  }

  Future<void> _onLoadMaintenanceRequests(
    LoadMaintenanceRequests event,
    Emitter<MaintenanceState> emit,
  ) async {
    emit(const MaintenanceLoading());
    try {
      // useCache = false để đảm bảo luôn gọi API mới
      final requests = await _apiService.getDriverMaintenanceRequests(useCache: false);
      
      if (requests.isEmpty) {
        emit(const MaintenanceLoaded(requests: [], totalPages: 1, currentPage: 1));
      } else {
        emit(MaintenanceLoaded(requests: requests, totalPages: 1, currentPage: 1));
      }
    } catch (e) {
      emit(MaintenanceError(message: 'Không thể tải danh sách bảo trì: ${e.toString()}', details: e.toString()));
    }
  }

  Future<void> _onRefreshMaintenanceRequests(
    RefreshMaintenanceRequests event,
    Emitter<MaintenanceState> emit,
  ) async {
    print('🔄 MaintenanceBloc: _onRefreshMaintenanceRequests method CALLED!');
    print('🔄 MaintenanceBloc: Event type: ${event.runtimeType}');
    print('🔄 MaintenanceBloc: Current state: ${state.runtimeType}');
    print('🔄 MaintenanceBloc: Refresh button pressed - starting refresh...');
    try {
      // Emit loading state first để UI biết đang refresh
      emit(const MaintenanceLoading());
      print('🔄 MaintenanceBloc: Loading state emitted, calling API...');

      final requests = await _apiService.getDriverMaintenanceRequests(useCache: false);
      print('🔄 MaintenanceBloc: API call successful, received ${requests.length} requests');

      emit(MaintenanceLoaded(
        requests: requests,
        totalPages: 1,
        currentPage: 1,
      ));
      print('🔄 MaintenanceBloc: MaintenanceLoaded state emitted');
    } catch (e) {
      print('❌ MaintenanceBloc: Error during refresh: $e');
      emit(MaintenanceError(
        message: 'Failed to refresh maintenance requests',
        details: e.toString(),
      ));
    }
  }

  Future<void> _onCreateMaintenanceRequest(
    CreateMaintenanceRequest event,
    Emitter<MaintenanceState> emit,
  ) async {
    try {
      emit(const MaintenanceCreating());

      final dto = event.createDto;
      
      final newRequest = await _apiService.createMaintenanceRequest(
        vehicleId: dto.vehicleId,
        description: dto.description,
        maintenanceType: dto.maintenanceType,
        notes: dto.notes,
      );

      emit(MaintenanceCreated(request: newRequest));

      // Refresh the list
      add(const RefreshMaintenanceRequests());
    } catch (e) {
      emit(MaintenanceError(
        message: 'Failed to create maintenance request',
        details: e.toString(),
      ));
    }
  }

  Future<void> _onUpdateMaintenanceRequest(
    UpdateMaintenanceRequest event,
    Emitter<MaintenanceState> emit,
  ) async {
    try { 
      // For picking up vehicle or canceling maintenance
      if (event.updateDto.statusId == 18 || event.updateDto.statusId == 17) { // IN_USE or AVAILABLE status
        await _apiService.pickUpVehicle(
          maintenanceId: event.maintenanceId,
          notes: event.updateDto.notes,
        );
      }

      // Refresh the list
      add(const RefreshMaintenanceRequests());
    } catch (e) {
      emit(MaintenanceError(
        message: 'Failed to update maintenance request',
        details: e.toString(),
      ));
    }
  }

  Future<void> _onDeleteMaintenanceRequest(
    DeleteMaintenanceRequest event,
    Emitter<MaintenanceState> emit,
  ) async {
    try {
      // Instead of deleting, update the status to 18 (IN_USE) to cancel maintenance
      await _apiService.pickUpVehicle(
        maintenanceId: event.maintenanceId,
        notes: 'Yêu cầu bảo trì đã bị hủy bởi tài xế',
      );

      // Refresh the list
      add(const RefreshMaintenanceRequests());
    } catch (e) {
      emit(MaintenanceError(
        message: 'Failed to cancel maintenance request',
        details: e.toString(),
      ));
    }
  }
}