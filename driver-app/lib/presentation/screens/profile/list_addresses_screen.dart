import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/domain/models/address/addresses_response.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';
import 'package:ktc_logistics_driver/presentation/helpers/helpers.dart';
import 'package:ktc_logistics_driver/presentation/screens/client/profile_client_screen.dart';
import 'package:ktc_logistics_driver/presentation/themes/colors_frave.dart';

class ListAddressesScreen extends StatefulWidget {
  const ListAddressesScreen({super.key});

  @override
  _ListAddressesScreenState createState() => _ListAddressesScreenState();
}

class _ListAddressesScreenState extends State<ListAddressesScreen> with WidgetsBindingObserver {


  @override
  void initState() {
      WidgetsBinding.instance.addObserver(this);
     super.initState();
  }

  @override
  void dispose() {
    WidgetsBinding.instance.removeObserver(this);
    super.dispose();
  }

  @override
  void didChangeAppLifecycleState(AppLifecycleState state) async {
    if( state == AppLifecycleState.resumed ){
      if( await Permission.location.isGranted ){
        Navigator.push(context, routeFrave(page: AddStreetAddressScreen()));
      }
    }
  }


  void accessLocation( PermissionStatus status ) {

    switch ( status ){
      
      case PermissionStatus.granted:
        Navigator.push(context, routeFrave(page: AddStreetAddressScreen()));  
        break;
      case PermissionStatus.limited:
        break;
      case PermissionStatus.denied:
      case PermissionStatus.restricted:
      case PermissionStatus.permanentlyDenied:
      case PermissionStatus.provisional:
        openAppSettings();
    }
  }


  @override
  Widget build(BuildContext context) {
    
    return BlocListener<UserBloc, UserState>(
      listener: (context, state) {
        
        if ( state is LoadingUserState ){

          modalLoading(context);

        }else if ( state is SuccessUserState ){

          Navigator.pop(context);

        }else if( state is FailureUserState ){

          Navigator.pop(context);
          errorMessageSnack(context, state.error);
        }
      },
      child: Scaffold(
        backgroundColor: Colors.white,
        appBar: AppBar(
          backgroundColor: Colors.white,
          title: const TextCustom(text: 'List Addresses', fontSize: 19),
          centerTitle: true,
          elevation: 0,
          leadingWidth: 80,
          leading: TextButton(
            onPressed: () => Navigator.pushReplacement(context, routeFrave(page: ProfileClientScreen())), 
            child: const TextCustom(text: 'Cancel', color: ColorsFrave.primaryColor, fontSize: 17 )
          ),
          actions: [
            TextButton(
              onPressed: () async => accessLocation( await Permission.location.request() ), 
              child: const TextCustom(text: 'Add', color: ColorsFrave.primaryColor, fontSize: 17 )
            ),
          ],
        ),
        body: BlocBuilder<UserBloc, UserState>(
          builder: (context, state) {
            // TODO: Replace with proper address loading from UserBloc
            // For now, return empty list to avoid userServices error
            return _ListAddresses(listAddress: []);
          }
        ),
      ),
    );
  }
}

class _ListAddresses extends StatelessWidget {
  
  final List<ListAddress> listAddress;

  const _ListAddresses({super.key, required this.listAddress});

  @override
  Widget build(BuildContext context) {

    final userBloc = BlocProvider.of<UserBloc>(context);

    return ( listAddress.isNotEmpty ) 
    ? ListView.builder(
        padding: const EdgeInsets.symmetric(horizontal: 20.0, vertical: 10.0),
        itemCount: listAddress.length,
        itemBuilder: (_, i) 
          => Dismissible(
                key: Key(listAddress[i].id.toString()),
                direction: DismissDirection.endToStart,
                background: Container(),
                onDismissed: (direction) => userBloc.add( OnDeleteStreetAddressEvent(listAddress[i].id)),
                secondaryBackground: Container(
                  alignment: Alignment.centerRight,
                  padding: const EdgeInsets.only(right: 20.0),
                  margin: const EdgeInsets.only(bottom: 20.0),
                  decoration: BoxDecoration(
                    color: Colors.red,
                    borderRadius: BorderRadius.only(topRight: Radius.circular(10.0), bottomRight: Radius.circular(10.0))
                  ),
                  child: const Icon(Icons.delete_sweep_rounded, color: Colors.white, size: 38),
                ),
                child: Container(
                  height: 70,
                  width: MediaQuery.of(context).size.width,
                  margin: const EdgeInsets.only(bottom: 20.0),
                  decoration: BoxDecoration(
                    color: Colors.grey[50],
                    borderRadius: BorderRadius.circular(10.0)
                  ),
                  child: ListTile(
                    leading: BlocBuilder<UserBloc, UserState>(
                      builder: (_, state) 
                        => ( state.uidAddress == listAddress[i].id ) ? Icon(Icons.radio_button_checked_rounded, color: ColorsFrave.primaryColor) : Icon(Icons.radio_button_off_rounded)
                    ),
                    title: TextCustom(text: listAddress[i].street, fontSize: 20, fontWeight: FontWeight.w500 ),
                    subtitle: TextCustom(text: listAddress[i].reference, fontSize: 16, color: ColorsFrave.secundaryColor ),
                    trailing: Icon(Icons.swap_horiz_rounded, color: Colors.red[300] ),
                    onTap: () => userBloc.add( OnSelectAddressButtonEvent( listAddress[i].id, listAddress[i].reference)),
                  ),
                ),
              )
        )
    : _WithoutListAddress();
  }
}



class _WithoutListAddress extends StatelessWidget {

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      width: MediaQuery.of(context).size.width,
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SvgPicture.asset('assets/svg/my-location.svg', height: 400 ),
          const TextCustom(text: 'Without Address', fontSize: 25, fontWeight: FontWeight.w500, color: ColorsFrave.secundaryColor ),
          const SizedBox(height: 80),
        ],
      ),
    );
  }
}



