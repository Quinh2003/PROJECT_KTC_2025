import 'package:equatable/equatable.dart';
import 'user_model.dart';

abstract class AuthState extends Equatable {
  @override
  List<Object?> get props => [];
}

class AuthInitialState extends AuthState {}

class AuthLoadingState extends AuthState {}

class AuthenticatedState extends AuthState {
  final UserModel user;
  final String token;

  AuthenticatedState({required this.user, required this.token});

  @override
  List<Object?> get props => [user, token];
}

class UnauthenticatedState extends AuthState {}

class AuthErrorState extends AuthState {
  final String message;

  AuthErrorState({required this.message});

  @override
  List<Object?> get props => [message];
}

class AuthSuccessState extends AuthState {
  final String message;

  AuthSuccessState({required this.message});

  @override
  List<Object?> get props => [message];
}


