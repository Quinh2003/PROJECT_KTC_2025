import 'package:equatable/equatable.dart';

abstract class AuthEvent extends Equatable {
  @override
  List<Object?> get props => [];
}

class LoginEvent extends AuthEvent {
  final String email;
  final String password;

  LoginEvent({required this.email, required this.password});

  @override
  List<Object?> get props => [email, password];
}

class CheckLoginEvent extends AuthEvent {}

class LogOutEvent extends AuthEvent {}

class RefreshTokenEvent extends AuthEvent {}

class UpdateUserEvent extends AuthEvent {
  final Map<String, dynamic> userData;

  UpdateUserEvent({required this.userData});

  @override
  List<Object?> get props => [userData];
}


