import 'dart:async';
import 'package:bloc/bloc.dart';
import '../../../domain/usecases/usecases.dart';
import 'auth_event.dart';
import 'auth_state.dart';
import 'user_model.dart';

class AuthBloc extends Bloc<AuthEvent, AuthState> {
  final LoginUseCase _loginUseCase;
  final LogoutUseCase _logoutUseCase;
  final CheckLoginStatusUseCase _checkLoginStatusUseCase;
  final GetCurrentUserUseCase _getCurrentUserUseCase;

  AuthBloc({
    required LoginUseCase loginUseCase,
    required LogoutUseCase logoutUseCase,
    required CheckLoginStatusUseCase checkLoginStatusUseCase,
    required GetCurrentUserUseCase getCurrentUserUseCase,
  }) : _loginUseCase = loginUseCase,
       _logoutUseCase = logoutUseCase,
       _checkLoginStatusUseCase = checkLoginStatusUseCase,
       _getCurrentUserUseCase = getCurrentUserUseCase,
       super(AuthInitialState()) {
    
    on<LoginEvent>(_onLogin);
    on<CheckLoginEvent>(_onCheckLogin);
    on<LogOutEvent>(_onLogOut);
    on<RefreshTokenEvent>(_onRefreshToken);
    on<UpdateUserEvent>(_onUpdateUser);
  }

  Future<void> _onLogin(LoginEvent event, Emitter<AuthState> emit) async {
    try {
      emit(AuthLoadingState());

      final authResponse = await _loginUseCase.call(
        LoginParams(email: event.email, password: event.password)
      );

      // Thêm delay để hiển thị loading
      await Future.delayed(const Duration(milliseconds: 500));

      if (authResponse.success) {
        // Convert từ auth_models.User sang repository User
        final user = UserModel(
          id: authResponse.user.uid,
          email: authResponse.user.email,
          name: authResponse.user.name,
          phone: authResponse.user.phone,
          avatar: authResponse.user.image.isNotEmpty ? authResponse.user.image : null,
          rating: 4.8, // Default rating
          totalDeliveries: 0, // Default value
        );
        
        emit(AuthenticatedState(
          user: user,
          token: authResponse.token,
        ));
      } else {
        emit(AuthErrorState(message: 'Đăng nhập thất bại'));
      }
    } catch (e) {
      emit(AuthErrorState(message: 'Lỗi kết nối: ${e.toString()}'));
    }
  }

  Future<void> _onCheckLogin(CheckLoginEvent event, Emitter<AuthState> emit) async {
    try {
      emit(AuthLoadingState());

      final isLoggedIn = await _checkLoginStatusUseCase.call(NoParams());

      if (isLoggedIn) {
        final user = await _getCurrentUserUseCase.call(NoParams());
        if (user != null) {
          // Create UserModel from User
          final userModel = UserModel(
            id: user.uid,
            email: user.email,
            name: user.name,
            phone: user.phone,
            avatar: user.image.isNotEmpty ? user.image : null,
            rating: 4.8, // Default rating
            totalDeliveries: 0, // Default value
          );
          
          emit(AuthenticatedState(
            user: userModel,
            token: 'mock_token_${userModel.id}',
          ));
        } else {
          emit(UnauthenticatedState());
        }
      } else {
        emit(UnauthenticatedState());
      }
    } catch (e) {
      emit(UnauthenticatedState());
    }
  }

  Future<void> _onLogOut(LogOutEvent event, Emitter<AuthState> emit) async {
    try {
      await _logoutUseCase.call(NoParams());
      emit(UnauthenticatedState());
    } catch (e) {
      emit(AuthErrorState(message: 'Lỗi đăng xuất: ${e.toString()}'));
    }
  }

  Future<void> _onRefreshToken(RefreshTokenEvent event, Emitter<AuthState> emit) async {
    try {
      // Mock refresh token logic
      if (state is AuthenticatedState) {
        final currentState = state as AuthenticatedState;
        emit(AuthenticatedState(
          user: currentState.user,
          token: 'refreshed_token_${currentState.user.id}',
        ));
      }
    } catch (e) {
      emit(AuthErrorState(message: 'Lỗi làm mới token: ${e.toString()}'));
    }
  }

  Future<void> _onUpdateUser(UpdateUserEvent event, Emitter<AuthState> emit) async {
    try {
      if (state is AuthenticatedState) {
        final currentState = state as AuthenticatedState;
        // Create updated user with new data
        final updatedUser = UserModel(
          id: currentState.user.id,
          email: event.userData['email'] ?? currentState.user.email,
          name: event.userData['name'] ?? currentState.user.name,
          phone: event.userData['phone'] ?? currentState.user.phone,
          avatar: event.userData['avatar'] ?? currentState.user.avatar,
          vehicle: event.userData['vehicle'] ?? currentState.user.vehicle,
          rating: (event.userData['rating'] ?? currentState.user.rating).toDouble(),
          totalDeliveries: event.userData['totalDeliveries'] ?? currentState.user.totalDeliveries,
        );
        
        emit(AuthenticatedState(
          user: updatedUser,
          token: currentState.token,
        ));
      }
    } catch (e) {
      emit(AuthErrorState(message: 'Lỗi cập nhật thông tin: ${e.toString()}'));
    }
  }
}


