import 'dart:async';
import 'package:meta/meta.dart';
import 'package:bloc/bloc.dart';
import 'package:google_maps_flutter/google_maps_flutter.dart';
import '../../../domain/models/auth/auth_response.dart';
import '../../../services/user_services.dart';

part 'user_event.dart';
part 'user_state.dart';

class UserBloc extends Bloc<UserEvent, UserState> {
  final UserServices userServices;
  Timer? _debouncer;

  UserBloc({required this.userServices}) : super(UserState()){

    on<OnGetUserEvent>(_onGetUser);
    on<OnFetchUserEvent>(_onFetchUser);
    on<OnSelectPictureEvent>(_onSelectPicture);
    on<OnClearPicturePathEvent>(_onClearPicturePath);
    on<OnChangeImageProfileEvent>(_onChangePictureProfile);
    on<OnEditUserEvent>(_onEditProfileUserDebounced); // Use debounced version
    on<OnChangePasswordEvent>(_onChangePassword);
    on<OnRegisterClientEvent>(_onRegisterClient);
    on<OnRegisterDeliveryEvent>(_onRegisterDelivery);
    on<OnUpdateDeliveryToClientEvent>(_onUpdateDeliveryToClient);
    on<OnDeleteStreetAddressEvent>(_onDeleteStreetAddress);
    on<OnSelectAddressButtonEvent>(_onSelectAddressButton);
    on<OnAddNewAddressEvent>(_onAddNewStreetAddress);
  }

  @override
  Future<void> close() {
    _debouncer?.cancel();
    return super.close();
  }

  Future<void> _onGetUser(OnGetUserEvent event, Emitter<UserState> emit) async {
    // Only emit if user actually changed
    if (state.user != event.user) {
      emit(state.copyWith(user: event.user));
    }
  }

  Future<void> _onFetchUser(OnFetchUserEvent event, Emitter<UserState> emit) async {
    // Prevent multiple simultaneous fetch requests
    if (state is LoadingUserState) return;
    
    try {
      emit(LoadingUserState());
      
      final user = await userServices.getUserById();
      
      // Only emit if user data actually changed
      if (state.user != user) {
        emit(SuccessUserState(user: user));
        emit(state.copyWith(user: user));
      } else {
        emit(state.copyWith()); // Just remove loading state
      }
    } catch (e) {
      emit(FailureUserState(e.toString()));
    }
  }

  /// Debounced version of edit profile to prevent rapid submissions
  Future<void> _onEditProfileUserDebounced(OnEditUserEvent event, Emitter<UserState> emit) async {
    _debouncer?.cancel();
    _debouncer = Timer(const Duration(milliseconds: 300), () {
      _onEditProfileUser(event, emit);
    });
  }

  Future<void> _onSelectPicture( OnSelectPictureEvent event, Emitter<UserState> emit) async {

    emit( state.copyWith( pictureProfilePath: event.pictureProfilePath ) );

  }

  Future<void> _onClearPicturePath(OnClearPicturePathEvent event, Emitter<UserState> emit) async {

    emit( state.copyWith( pictureProfilePath: '' ));

  }

  Future<void> _onChangePictureProfile( OnChangeImageProfileEvent event, Emitter<UserState> emit ) async {

    try {

      emit( LoadingUserState() );

      final data = await userServices.changeImageProfile(event.image);

      if( data.resp ){

        final user = await userServices.getUserById();

        emit( SuccessUserState(user: user) );

        // Update state with user data
        emit( state.copyWith( user: user ));

      }else{
        emit( FailureUserState(data.msg) );
      }
      
    } catch (e) {
      emit( FailureUserState(e.toString()) );
    }

  }

  Future<void> _onEditProfileUser( OnEditUserEvent event, Emitter<UserState> emit ) async {

    try {

      emit( LoadingUserState() );

      final data = await userServices.editProfile(event.name, event.lastname, event.phone);

      if( data.resp ){

        final user = await userServices.getUserById();

        emit( SuccessUserState(user: user) );

        // Update state with user data
        emit( state.copyWith( user: user ));

      }else{
        emit( FailureUserState(data.msg) );
      }
      
    } catch (e) {
      emit( FailureUserState(e.toString()) );
    }

  }

  Future<void> _onChangePassword( OnChangePasswordEvent event, Emitter<UserState> emit ) async {

    try {

      emit( LoadingUserState() );

      final data = await userServices.changePassword(event.currentPassword, event.newPassword);

      if( data.resp ){

        final user = await userServices.getUserById();

        emit( SuccessUserState(user: user) );

        // Update state with user data
        emit( state.copyWith( user: user ));

      }else{
        emit( FailureUserState(data.msg) );
      }
      
    } catch (e) {
      emit( FailureUserState(e.toString()) );
    }

  }
  
  Future<void> _onRegisterClient( OnRegisterClientEvent event, Emitter<UserState> emit ) async {

    try {

      emit( LoadingUserState() );

      // TODO: Need to get address and reference from event or elsewhere
      // For now using placeholder values
      final data = await userServices.registerClient(
        event.name, 
        event.lastname, 
        event.phone, 
        event.email, 
        event.password, 
        '', // address placeholder
        '', // reference placeholder
        event.image
      );

      if( data.resp ) {
        emit( SuccessUserState() );
      } else {
        emit( FailureUserState(data.msg) );
      }
      
    } catch (e) {
      emit( FailureUserState(e.toString()));
    }

  }

  Future<void> _onRegisterDelivery( OnRegisterDeliveryEvent event, Emitter<UserState> emit) async {

    try {

      emit( LoadingUserState() );

      final data = await userServices.registerDelivery(
        event.name, 
        event.lastname, 
        event.phone, 
        event.email, 
        event.password, 
        event.image
      );

      if( data.resp ) {
        
        final user = await userServices.getUserById();

        emit( SuccessUserState(user: user) );

        // Update state with user data
        emit( state.copyWith( user: user ));

      } else {
        emit( FailureUserState(data.msg));
      }
      
    } catch (e) {
      emit( FailureUserState(e.toString()) );
    }

  }

  Future<void> _onUpdateDeliveryToClient( OnUpdateDeliveryToClientEvent event, Emitter<UserState> emit) async {

    try {

      emit( LoadingUserState() );

      final data = await userServices.updateDeliveryToClient(event.idPerson);

      if( data.resp ){

        final user = await userServices.getUserById();

        emit( SuccessUserState(user: user) );

        // Update state with user data
        emit( state.copyWith(user: user) );

      }else{
        emit( FailureUserState(data.msg) );
      }
      
    } catch (e) {
      emit( FailureUserState(e.toString()));
    }

  }

  Future<void> _onDeleteStreetAddress( OnDeleteStreetAddressEvent event, Emitter<UserState> emit) async {

    try {

      emit( LoadingUserState() );

      final data = await userServices.deleteStreetAddress( event.uid.toString() );

      if( data.resp ){

        final user = await userServices.getUserById();

        emit( SuccessUserState() );

        // Fix: Cast User to User? for copyWith compatibility
        emit( state.copyWith( user: user as User? ));

      }else {
        emit( FailureUserState(data.msg) );
      }


    } catch (e) {
      emit( FailureUserState(e.toString()) );
    }

  }

  Future<void> _onSelectAddressButton( OnSelectAddressButtonEvent event, Emitter<UserState> emit) async {

    emit( state.copyWith( uidAddress: event.uidAddress, addressName: event.addressName ) );

  }

  Future<void> _onAddNewStreetAddress( OnAddNewAddressEvent event, Emitter<UserState> emit ) async {

    try {

      emit( LoadingUserState() );

      // TODO: Fix method name and parameters based on actual UserServices API
      // Commenting out problematic code for now
      /*
      final data = await userServices.addNewAddressLocation(event.street, event.reference, event.location.latitude.toString(), event.location.longitude.toString());
      
      if( data.resp ){

        final user = await userServices.getUserById();

        final userdb = await userServices.getAddressOne();

        add(OnSelectAddressButtonEvent(userdb.address.id, userdb.address.reference));

        emit( SuccessUserState() );

        emit( state.copyWith( user: user ) );

      }else{
        emit( FailureUserState(data.msg) );
      }
      */

      // Temporary implementation
      emit( FailureUserState('Address functionality not implemented yet') );

    } catch (e) {
      emit( FailureUserState(e.toString()) );
    }

  }


}


