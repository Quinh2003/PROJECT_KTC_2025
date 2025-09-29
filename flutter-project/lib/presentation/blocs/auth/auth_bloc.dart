import 'dart:async';
import 'package:bloc/bloc.dart';
import '../../../services/auth_services.dart';
import '../../../domain/models/auth/auth_models.dart';
import 'auth_event.dart';
import 'auth_state.dart';
import 'user_model.dart';

class AuthBloc extends Bloc<AuthEvent, AuthState> {
  final AuthServices _authServices;

  AuthBloc({required AuthServices authServices})
      : _authServices = authServices,
        super(AuthInitialState()) {
    on<LoginEvent>(_onLogin);
    on<CheckLoginEvent>(_onCheckLogin);
    on<LogOutEvent>(_onLogOut);
    on<RefreshTokenEvent>(_onRefreshToken);
    on<UpdateUserEvent>(_onUpdateUser);
  }

  Future<void> _onLogin(LoginEvent event, Emitter<AuthState> emit) async {
    print('AuthBloc: Starting login process...'); // Debug log
    
    // Kiểm tra nếu đang ở trạng thái loading, bỏ qua để tránh emit trùng
    if (state is AuthLoadingState) {
      print('AuthBloc: Already in loading state, ignoring request');
      return;
    }
    
    try {
      emit(AuthLoadingState());
      print('AuthBloc: Emitted loading state'); // Debug log

      // Gọi trực tiếp service đăng nhập
      print('AuthBloc: Calling auth service...'); // Debug log
      final loginResponse = await _authServices.login(event.email, event.password);
      print('AuthBloc: Login response received: ${loginResponse.username}'); // Debug log
      
      // Chuyển đổi từ LoginResponse của API sang UserModel
      final userModel = _convertToUserModel(loginResponse);
      print('AuthBloc: User model created: ${userModel.name}'); // Debug log
      
      // Chỉ emit khi thành công, không delay
      emit(AuthenticatedState(
        user: userModel,
        token: loginResponse.accessToken,
      ));
      print('AuthBloc: Emitted authenticated state'); // Debug log
    } catch (e) {
      print('AuthBloc: Login error: $e'); // Debug log
      emit(AuthErrorState(message: 'Đăng nhập thất bại: ${e.toString()}'));
    }
  }

  Future<void> _onCheckLogin(CheckLoginEvent event, Emitter<AuthState> emit) async {
    print('AuthBloc: Checking login status...'); // Debug log
    try {
      // Không emit loading state cho check login để tránh xung đột
      
      // Kiểm tra trạng thái đăng nhập trực tiếp từ service
      final isAuthenticated = await _authServices.checkAuthStatus();
      print('AuthBloc: Check auth status result: $isAuthenticated'); // Debug log

      if (isAuthenticated) {
        try {
          // Làm mới token
          final loginResponse = await _authServices.refreshToken();
          
          // Chuyển đổi sang UserModel
          final userModel = _convertToUserModel(loginResponse);
          
          emit(AuthenticatedState(
            user: userModel,
            token: loginResponse.accessToken,
          ));
          print('AuthBloc: User already authenticated'); // Debug log
        } catch (e) {
          print('AuthBloc: Refresh token failed: $e'); // Debug log
          emit(UnauthenticatedState());
        }
      } else {
        print('AuthBloc: User not authenticated'); // Debug log
        emit(UnauthenticatedState());
      }
    } catch (e) {
      print('AuthBloc: Check login error: $e'); // Debug log
      emit(UnauthenticatedState());
    }
  }

  Future<void> _onLogOut(LogOutEvent event, Emitter<AuthState> emit) async {
    try {
      // Gọi trực tiếp service đăng xuất
      await _authServices.logout();
      emit(UnauthenticatedState());
    } catch (e) {
      emit(AuthErrorState(message: 'Lỗi đăng xuất: ${e.toString()}'));
    }
  }

  Future<void> _onRefreshToken(RefreshTokenEvent event, Emitter<AuthState> emit) async {
    if (state is AuthenticatedState) {
      try {
        // Gọi trực tiếp service làm mới token
        final loginResponse = await _authServices.refreshToken();
        
        // Chuyển đổi sang UserModel
        final userModel = _convertToUserModel(loginResponse);
        
        emit(AuthenticatedState(
          user: userModel,
          token: loginResponse.accessToken,
        ));
      } catch (e) {
        emit(AuthErrorState(message: 'Lỗi làm mới token: ${e.toString()}'));
      }
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
  
  // Phương thức helper để chuyển đổi LoginResponse thành UserModel
  UserModel _convertToUserModel(LoginResponse loginResponse) {
    return UserModel(
      id: loginResponse.userId.toString(),
      name: loginResponse.username,
      email: loginResponse.username,  // Giả sử username là email
      phone: '',  // Điền thông tin nếu có
      avatar: '',  // Điền thông tin nếu có
      vehicle: '',  // Điền thông tin nếu có
      rating: 0.0,  // Giá trị mặc định
      totalDeliveries: 0,  // Giá trị mặc định
    );
  }
}


