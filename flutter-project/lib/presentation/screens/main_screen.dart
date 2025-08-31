import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/screens/dashboard/dashboard_screen_spatial.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/deliveries_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/profile/edit_profile_screen.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';

class MainScreen extends StatefulWidget {
  final int initialIndex;
  
  const MainScreen({
    Key? key,
    this.initialIndex = 0,
  }) : super(key: key);

  @override
  State<MainScreen> createState() => _MainScreenState();
}

class _MainScreenState extends State<MainScreen> {
  late int _currentIndex;
  
  @override
  void initState() {
    super.initState();
    _currentIndex = widget.initialIndex;
  }
  
  final List<Widget> _screens = [
    const DashboardScreenSpatial(showBottomNav: false),
    const DeliveriesScreen(),
    const EditProfileScreen(),
  ];
  
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: IndexedStack(
        index: _currentIndex,
        children: _screens,
      ),
      bottomNavigationBar: _buildBottomNav(context),
    );
  }
  
  Widget _buildBottomNav(BuildContext context) {
    return BottomNavigationBar(
      currentIndex: _currentIndex,
      onTap: (index) {
        setState(() {
          _currentIndex = index;
        });
      },
      type: BottomNavigationBarType.fixed,
      backgroundColor: Theme.of(context).colorScheme.surface,
      selectedItemColor: SpatialDesignSystem.primaryColor,
      unselectedItemColor: SpatialDesignSystem.textSecondaryColor,
      items: const [
        BottomNavigationBarItem(
          icon: Icon(Icons.dashboard),
          label: "Dashboard",
        ),
        BottomNavigationBarItem(
          icon: Icon(Icons.local_shipping),
          label: "Deliveries",
        ),
        BottomNavigationBarItem(
          icon: Icon(Icons.account_circle),
          label: "Profile",
        ),
      ],
    );
  }
}
