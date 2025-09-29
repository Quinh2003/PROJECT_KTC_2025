import 'package:bloc/bloc.dart';
import '../auth_event.dart';
import '../auth_state.dart';
import '../user_model.dart';
import '../../../../services/auth_services.dart';
import '../../../../domain/models/auth/auth_models.dart';

/// Bridge class để kết nối AuthServicesV2 với AuthBloc hiện tại
class AuthApiAdapter {
  final AuthServices _authServices;

  AuthApiAdapter({required AuthServices authServices}) : _authServices = authServices;

  /// Đăng nhập với API mới
  Future<void> handleLogin(LoginEvent event, Emitter<AuthState> emit) async {
    try {
      emit(AuthLoadingState());

      // Chuyển đổi email thành username
      final loginResponse = await _authServices.login(event.email, event.password);

      // Thêm delay để hiển thị loading
      await Future.delayed(const Duration(milliseconds: 500));

      // Chuyển đổi từ LoginResponse của API mới sang UserModel
      final userModel = _convertToUserModel(loginResponse);
      
      emit(AuthenticatedState(
        user: userModel,
        token: loginResponse.accessToken,
      ));
    } catch (e) {
      emit(AuthErrorState(message: 'Đăng nhập thất bại: ${e.toString()}'));
    }
  }

  /// Kiểm tra trạng thái đăng nhập
  Future<void> handleCheckLoginStatus(CheckLoginEvent event, Emitter<AuthState> emit) async {
    try {
      emit(AuthLoadingState());

      final isAuthenticated = await _authServices.checkAuthStatus();

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
        } catch (e) {
          emit(UnauthenticatedState());
        }
      } else {
        emit(UnauthenticatedState());
      }
    } catch (e) {
      emit(UnauthenticatedState());
    }
  }

  /// Đăng xuất
  Future<void> handleLogout(LogOutEvent event, Emitter<AuthState> emit) async {
    try {
      await _authServices.logout();
      emit(UnauthenticatedState());
    } catch (e) {
      emit(AuthErrorState(message: 'Lỗi đăng xuất: ${e.toString()}'));
    }
  }

  /// Làm mới token
  Future<void> handleRefreshToken(RefreshTokenEvent event, Emitter<AuthState> emit, AuthState currentState) async {
    if (currentState is AuthenticatedState) {
      try {
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

  /// Chuyển đổi từ LoginResponse của API mới sang UserModel
  UserModel _convertToUserModel(LoginResponse loginResponse) {
    return UserModel(
      id: loginResponse.userId.toString(),
      email: loginResponse.username, // Dùng username khi email không có
      name: loginResponse.username, // Tên hiển thị tạm thời là username
      phone: '', // API mới không trả về phone, cần lấy từ profile
      avatar: null, // API mới không trả về avatar, cần lấy từ profile
      rating: 0.0, // Cần lấy từ DriverProfile sau khi đăng nhập
      totalDeliveries: 0, // Cần lấy từ DriverAnalytics sau khi đăng nhập
    );
  }
}
