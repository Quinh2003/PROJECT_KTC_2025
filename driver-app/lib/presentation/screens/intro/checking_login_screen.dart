import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:ktc_logistics_driver/domain/models/response/auth_response.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';
import 'package:ktc_logistics_driver/presentation/screens/home/select_role_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/login/login_screen.dart';
import 'package:ktc_logistics_driver/presentation/themes/colors_frave.dart';

class CheckingLoginScreen extends StatefulWidget {
  
  @override
  _CheckingLoginScreenState createState() => _CheckingLoginScreenState();
}


class _CheckingLoginScreenState extends State<CheckingLoginScreen> with TickerProviderStateMixin {

  late AnimationController _animationController;
  
  late Animation<double> _scaleAnimation;

  @override
  void initState() {
    super.initState();

    _animationController = AnimationController(vsync: this, duration: Duration(milliseconds: 500));

    _scaleAnimation = Tween<double>(begin: 1.0, end: 0.8).animate(_animationController)..addStatusListener((status) {
      if( status == AnimationStatus.completed ){
        _animationController.reverse();
      } else if ( status == AnimationStatus.dismissed ){
        _animationController.forward();
      }
    });

    _animationController.forward();
  }

  @override
  void dispose() {
    _animationController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {

    final userBloc = BlocProvider.of<UserBloc>(context);

    return BlocListener<AuthBloc, AuthState>(
      listener: (context, state) async {
        
        if( state is AuthLoadingState ){

          Navigator.pushReplacement(context, routeFrave(page: CheckingLoginScreen()));
        
        } else if ( state is UnauthenticatedState ){

          Navigator.pushAndRemoveUntil(context, routeFrave(page: LoginScreen()), (route) => false);    
         
        } else if ( state is AuthenticatedState ){

          // Convert UserModel to User for UserBloc
          final user = User(
            uid: state.user.id,
            name: state.user.name,
            email: state.user.email,
            phone: state.user.phone,
            image: state.user.avatar ?? '',
            role: '1', // Default role - should be provided by UserModel
            isActive: true,
            permissions: [],
          );

          userBloc.add( OnGetUserEvent(user) );

          // For now, default to role 1 (admin) since UserModel doesn't have role
          Navigator.pushAndRemoveUntil(context, routeFrave(page: SelectRoleScreen()), (route) => false);

        }
      },
      child: Scaffold(
        backgroundColor: ColorsFrave.primaryColor,
        body: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: [
            Align(
              alignment: Alignment.center,
              child: AnimatedBuilder(
                animation: _animationController,
                builder: (context, child) 
                  => Transform.scale(
                    scale: _scaleAnimation.value,
                    child: SizedBox(
                      height: 200,
                      width: 200,
                      child: Image.asset('assets/Logo/logo-white.png'),
                    ),
                  ),
              ),
            )
          ],
        ),
      ),
    );
  }
}
