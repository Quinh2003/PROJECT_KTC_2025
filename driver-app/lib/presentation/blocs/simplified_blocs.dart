// simplified_blocs.dart
// Đơn giản hóa các BLoC để dễ dàng sử dụng services trực tiếp

import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:equatable/equatable.dart';

import '../../services/api_service.dart';
import '../../services/socket_service.dart';
import '../../domain/models/response/auth_response.dart';

// ============ AUTH BLOC ============

// Auth Events
abstract class AuthEvent extends Equatable {
  const AuthEvent();
  
  @override
  List<Object?> get props => [];
}

class LoginEvent extends AuthEvent {
  final String email;
  final String password;
  
  const LoginEvent({
    required this.email,
    required this.password,
  });
  
  @override
  List<Object?> get props => [email, password];
}

class LogoutEvent extends AuthEvent {}

class CheckAuthStatusEvent extends AuthEvent {}

class GetCurrentUserEvent extends AuthEvent {}

// Auth States
abstract class AuthState extends Equatable {
  const AuthState();
  
  @override
  List<Object?> get props => [];
}

class AuthInitial extends AuthState {}

class AuthLoading extends AuthState {}

class AuthAuthenticated extends AuthState {
  final User user;
  
  const AuthAuthenticated(this.user);
  
  @override
  List<Object?> get props => [user];
}

class AuthUnauthenticated extends AuthState {}

class AuthError extends AuthState {
  final String message;
  
  const AuthError(this.message);
  
  @override
  List<Object?> get props => [message];
}

// Auth BLoC
class SimplifiedAuthBloc extends Bloc<AuthEvent, AuthState> {
  final AuthService authService;
  
  SimplifiedAuthBloc({
    required this.authService,
  }) : super(AuthInitial()) {
    on<LoginEvent>(_onLogin);
    on<LogoutEvent>(_onLogout);
    on<CheckAuthStatusEvent>(_onCheckAuthStatus);
    on<GetCurrentUserEvent>(_onGetCurrentUser);
  }
  
  Future<void> _onLogin(
    LoginEvent event, 
    Emitter<AuthState> emit
  ) async {
    emit(AuthLoading());
    
    try {
      final response = await authService.login(
        event.email,
        event.password,
      );
      
      emit(AuthAuthenticated(response.user));
    } catch (e) {
      emit(AuthError(e.toString()));
    }
  }
  
  Future<void> _onLogout(
    LogoutEvent event, 
    Emitter<AuthState> emit
  ) async {
    emit(AuthLoading());
    
    try {
      await authService.logout();
      emit(AuthUnauthenticated());
    } catch (e) {
      emit(AuthError(e.toString()));
    }
  }
  
  Future<void> _onCheckAuthStatus(
    CheckAuthStatusEvent event, 
    Emitter<AuthState> emit
  ) async {
    emit(AuthLoading());
    
    try {
      final isLoggedIn = await authService.isLoggedIn();
      
      if (isLoggedIn) {
        final user = await authService.getCurrentUser();
        if (user != null) {
          emit(AuthAuthenticated(user));
        } else {
          emit(AuthUnauthenticated());
        }
      } else {
        emit(AuthUnauthenticated());
      }
    } catch (e) {
      emit(AuthError(e.toString()));
    }
  }
  
  Future<void> _onGetCurrentUser(
    GetCurrentUserEvent event, 
    Emitter<AuthState> emit
  ) async {
    emit(AuthLoading());
    
    try {
      final user = await authService.getCurrentUser();
      
      if (user != null) {
        emit(AuthAuthenticated(user));
      } else {
        emit(AuthUnauthenticated());
      }
    } catch (e) {
      emit(AuthError(e.toString()));
    }
  }
}

// ============ TRACKING BLOC ============

// Các BLoC khác có thể được đơn giản hóa tương tự
