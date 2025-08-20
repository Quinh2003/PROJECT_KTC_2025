// Example: Migration từ old BLoC sang Firebase
// Đây là ví dụ để show cách update existing BLoCs để sử dụng Firebase services

import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:equatable/equatable.dart';

// Firebase Services
import '../../injection/dependency_injection.dart';
import '../../services/firebase_adapters.dart';

// ============ AUTH BLOC EXAMPLE ============

// States
abstract class AuthState extends Equatable {
  const AuthState();
  
  @override
  List<Object> get props => [];
}

class AuthInitial extends AuthState {}
class AuthLoading extends AuthState {}
class AuthAuthenticated extends AuthState {
  final Map<String, dynamic> user;
  
  const AuthAuthenticated(this.user);
  
  @override
  List<Object> get props => [user];
}
class AuthUnauthenticated extends AuthState {}
class AuthError extends AuthState {
  final String message;
  
  const AuthError(this.message);
  
  @override
  List<Object> get props => [message];
}

// Events
abstract class AuthEvent extends Equatable {
  const AuthEvent();
  
  @override
  List<Object> get props => [];
}

class AuthLoginRequested extends AuthEvent {
  final String email;
  final String password;
  
  const AuthLoginRequested({required this.email, required this.password});
  
  @override
  List<Object> get props => [email, password];
}

class AuthRegisterRequested extends AuthEvent {
  final String email;
  final String password;
  final String name;
  final String phone;
  
  const AuthRegisterRequested({
    required this.email,
    required this.password,
    required this.name,
    required this.phone,
  });
  
  @override
  List<Object> get props => [email, password, name, phone];
}

class AuthLogoutRequested extends AuthEvent {}

class AuthStatusChecked extends AuthEvent {}

// BLoC
class AuthBloc extends Bloc<AuthEvent, AuthState> {
  AuthBloc() : super(AuthInitial()) {
    on<AuthLoginRequested>(_onLoginRequested);
    on<AuthRegisterRequested>(_onRegisterRequested);
    on<AuthLogoutRequested>(_onLogoutRequested);
    on<AuthStatusChecked>(_onStatusChecked);
  }

  // Sử dụng Firebase adapter để maintain compatibility
  final _authAdapter = authAdapter;

  Future<void> _onLoginRequested(
    AuthLoginRequested event,
    Emitter<AuthState> emit,
  ) async {
    emit(AuthLoading());
    
    try {
      final result = await _authAdapter.login(event.email, event.password);
      
      if (result['success'] == true) {
        emit(AuthAuthenticated(result['user']));
      } else {
        emit(AuthError(result['error'] ?? 'Login failed'));
      }
    } catch (e) {
      emit(AuthError('Login failed: $e'));
    }
  }

  Future<void> _onRegisterRequested(
    AuthRegisterRequested event,
    Emitter<AuthState> emit,
  ) async {
    emit(AuthLoading());
    
    try {
      final result = await _authAdapter.register(
        email: event.email,
        password: event.password,
        name: event.name,
        phone: event.phone,
      );
      
      if (result['success'] == true) {
        emit(AuthAuthenticated(result['user']));
      } else {
        emit(AuthError(result['error'] ?? 'Registration failed'));
      }
    } catch (e) {
      emit(AuthError('Registration failed: $e'));
    }
  }

  Future<void> _onLogoutRequested(
    AuthLogoutRequested event,
    Emitter<AuthState> emit,
  ) async {
    try {
      await _authAdapter.logout();
      emit(AuthUnauthenticated());
    } catch (e) {
      emit(AuthError('Logout failed: $e'));
    }
  }

  Future<void> _onStatusChecked(
    AuthStatusChecked event,
    Emitter<AuthState> emit,
  ) async {
    if (_authAdapter.isLoggedIn && _authAdapter.currentUser != null) {
      emit(AuthAuthenticated(_authAdapter.currentUser!));
    } else {
      emit(AuthUnauthenticated());
    }
  }
}

// ============ ORDER BLOC EXAMPLE ============

// States
abstract class OrderState extends Equatable {
  const OrderState();
  
  @override
  List<Object> get props => [];
}

class OrderInitial extends OrderState {}
class OrderLoading extends OrderState {}
class OrderLoaded extends OrderState {
  final List<Map<String, dynamic>> orders;
  
  const OrderLoaded(this.orders);
  
  @override
  List<Object> get props => [orders];
}
class OrderError extends OrderState {
  final String message;
  
  const OrderError(this.message);
  
  @override
  List<Object> get props => [message];
}

// Events
abstract class OrderEvent extends Equatable {
  const OrderEvent();
  
  @override
  List<Object> get props => [];
}

class OrdersLoadRequested extends OrderEvent {
  final String userId;
  
  const OrdersLoadRequested(this.userId);
  
  @override
  List<Object> get props => [userId];
}

class OrderStatusUpdateRequested extends OrderEvent {
  final String orderId;
  final String status;
  
  const OrderStatusUpdateRequested({
    required this.orderId,
    required this.status,
  });
  
  @override
  List<Object> get props => [orderId, status];
}

// BLoC
class OrderBloc extends Bloc<OrderEvent, OrderState> {
  OrderBloc() : super(OrderInitial()) {
    on<OrdersLoadRequested>(_onOrdersLoadRequested);
    on<OrderStatusUpdateRequested>(_onOrderStatusUpdateRequested);
  }

  // Sử dụng Firebase adapter
  final _dataAdapter = dataAdapter;

  Future<void> _onOrdersLoadRequested(
    OrdersLoadRequested event,
    Emitter<OrderState> emit,
  ) async {
    emit(OrderLoading());
    
    try {
      final orders = await _dataAdapter.getUserOrders(event.userId);
      emit(OrderLoaded(orders));
    } catch (e) {
      emit(OrderError('Failed to load orders: $e'));
    }
  }

  Future<void> _onOrderStatusUpdateRequested(
    OrderStatusUpdateRequested event,
    Emitter<OrderState> emit,
  ) async {
    try {
      final success = await _dataAdapter.updateOrderStatus(
        event.orderId,
        event.status,
      );
      
      if (!success) {
        emit(OrderError('Failed to update order status'));
      }
      // Optionally reload orders or update state
    } catch (e) {
      emit(OrderError('Failed to update order status: $e'));
    }
  }
}

/*
MIGRATION GUIDE:

1. Old BLoC (sử dụng AuthServices):
```dart
class OldAuthBloc extends Bloc<AuthEvent, AuthState> {
  final AuthServices _authServices = getIt<AuthServices>();
  
  // Old implementation using REST API
}
```

2. New BLoC (sử dụng Firebase):
```dart
class NewAuthBloc extends Bloc<AuthEvent, AuthState> {
  final FirebaseAuthService _firebaseAuth = getIt<FirebaseAuthService>();
  
  // OR sử dụng adapter để maintain compatibility:
  final _authAdapter = authAdapter;
  
  // New implementation using Firebase
}
```

3. Update existing BLoCs:
- Thay thế service injection: `AuthServices` → `FirebaseAuthService` 
- Hoặc sử dụng adapter: `authAdapter`, `dataAdapter`, `notificationAdapter`
- Update method calls để match Firebase service interface
- Test thoroughly để ensure functionality không bị break

4. BLoCs cần update:
- AuthBloc: Use FirebaseAuthService hoặc authAdapter
- OrderBloc: Use FirestoreService hoặc dataAdapter  
- NotificationBloc: Use FirebaseMessagingService hoặc notificationAdapter
- ProfileBloc: Use FirestoreService để manage user data
- TrackingBloc: Use FirebaseRealtimeService cho real-time location

5. Benefits of Firebase migration:
- Real-time data sync với Firestore
- Scalable authentication với Firebase Auth
- Push notifications với FCM
- File upload với Firebase Storage
- Real-time location tracking với Realtime Database
*/
