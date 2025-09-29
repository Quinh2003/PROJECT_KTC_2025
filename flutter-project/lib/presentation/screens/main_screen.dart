import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/presentation/screens/dashboard/dashboard_screen_spatial.dart';
import 'package:ktc_logistics_driver/presentation/screens/delivery/deliveries_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/maintenance/maintenance_screen.dart';
import 'package:ktc_logistics_driver/presentation/screens/profile/edit_profile_screen.dart';
import 'package:ktc_logistics_driver/presentation/design/spatial_ui.dart';

class MainScreen extends StatefulWidget {
  final int initialIndex;
  
  const MainScreen({
    super.key,
    this.initialIndex = 0,
  });

  @override
  State<MainScreen> createState() => _MainScreenState();
}

class _MainScreenState extends State<MainScreen> {
  late int _currentIndex;
  
  @override
  void initState() {
    super.initState();
    _currentIndex = widget.initialIndex;
    print('MainScreen: Initialized with index $_currentIndex'); // Debug log
  }
  
  @override
  Widget build(BuildContext context) {
    print('MainScreen: Building with index $_currentIndex'); // Debug log
    
    // Sử dụng Scaffold với bottom navigation bar
    return Scaffold(
      body: _buildCurrentScreen(),
      bottomNavigationBar: _buildBottomNav(context),
    );
  }
  
  Widget _buildCurrentScreen() {
    print('MainScreen: Building screen index $_currentIndex'); // Debug log
    switch (_currentIndex) {
      case 0:
        return const DashboardScreenSpatial();
      case 1:
        return const DeliveriesScreen();
      case 2:
        return const MaintenanceScreen();
      case 3:
        return const EditProfileScreen();
      default:
        return const DashboardScreenSpatial();
    }
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
          icon: Icon(Icons.build_circle),
          label: "Maintenance",
        ),
        BottomNavigationBarItem(
          icon: Icon(Icons.account_circle),
          label: "Profile",
        ),
      ],
    );
  }
}
