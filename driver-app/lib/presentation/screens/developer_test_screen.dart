import 'package:flutter/material.dart';
import 'order/order_detail_screen.dart';
import 'map/route_map_screen.dart';

/// Developer Test Screen - Simplified version
/// Focus on Spatial UI demo and debug features
/// Remove this file in production build
class DeveloperTestScreen extends StatefulWidget {
  const DeveloperTestScreen({super.key});

  @override
  State<DeveloperTestScreen> createState() => _DeveloperTestScreenState();
}

class _DeveloperTestScreenState extends State<DeveloperTestScreen> with SingleTickerProviderStateMixin {
  late TabController _tabController;
  
  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: 2, vsync: this); // Chỉ còn 2 tabs
  }
  
  @override
  void dispose() {
    _tabController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('🧪 Developer Test Hub'),
        backgroundColor: Colors.deepPurple,
        foregroundColor: Colors.white,
        bottom: TabBar(
          controller: _tabController,
          indicatorColor: Colors.white,
          labelColor: Colors.white,
          unselectedLabelColor: Colors.white70,
          tabs: const [
            Tab(icon: Icon(Icons.view_in_ar), text: 'Spatial UI'),
            Tab(icon: Icon(Icons.bug_report), text: 'Debug'),
          ],
        ),
      ),
      body: TabBarView(
        controller: _tabController,
        children: [
          _buildSpatialUITab(),
          _buildDebugTab(),
        ],
      ),
    );
  }

  Widget _buildSpatialUITab() {
    return Padding(
      padding: const EdgeInsets.all(16.0),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: [
          const Card(
            child: Padding(
              padding: EdgeInsets.all(16.0),
              child: Column(
                children: [
                  Icon(Icons.view_in_ar, size: 48, color: Colors.blue),
                  SizedBox(height: 8),
                  Text(
                    '🎨 Spatial UI Components',
                    style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
                  ),
                  Text(
                    'Test new 3D-inspired UI screens',
                    style: TextStyle(color: Colors.grey),
                  ),
                ],
              ),
            ),
          ),
          const SizedBox(height: 16),
          Expanded(
            child: Column(
              children: [
                _buildSpatialUICard(
                  'Order Detail Screen',
                  'Modern order details with spatial design',
                  Icons.receipt_long,
                  () => Navigator.push(
                    context,
                    MaterialPageRoute(
                      builder: (context) => const OrderDetailScreen(
                        orderId: 'KTC-2025-08-14-123',
                      ),
                    ),
                  ),
                ),
                const SizedBox(height: 12),
                _buildSpatialUICard(
                  'Route Map Screen',
                  '3D-style route visualization',
                  Icons.map,
                  () => Navigator.push(
                    context,
                    MaterialPageRoute(
                      builder: (context) => const RouteMapScreen(
                        routeId: 'RT-2025-08-14-01',
                      ),
                    ),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildDebugTab() {
    return Padding(
      padding: const EdgeInsets.all(16.0),
      child: Column(
        children: [
          const Card(
            child: Padding(
              padding: EdgeInsets.all(16.0),
              child: Column(
                children: [
                  Icon(Icons.bug_report, size: 48, color: Colors.orange),
                  SizedBox(height: 8),
                  Text(
                    '🐛 Debug Information',
                    style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
                  ),
                  Text(
                    'Development tools and system info',
                    style: TextStyle(color: Colors.grey),
                  ),
                ],
              ),
            ),
          ),
          const SizedBox(height: 16),
          Expanded(
            child: Column(
              children: [
                _buildDebugInfoCard('Flutter Version', '3.32.8'),
                _buildDebugInfoCard('Dart Version', '3.5.0'),
                _buildDebugInfoCard('Package Name', 'com.ktc.logistics_driver'),
                _buildDebugInfoCard('Build Mode', 'Debug'),
                _buildDebugInfoCard('Architecture', 'Clean Architecture + BLoC'),
                _buildDebugInfoCard('Backend', 'Firebase + HTTP APIs'),
                const SizedBox(height: 16),
                Card(
                  color: Colors.green.shade50,
                  child: ListTile(
                    leading: const Icon(Icons.check_circle, color: Colors.green),
                    title: const Text('Refactor Complete'),
                    subtitle: const Text('Following Frave07 project structure'),
                  ),
                ),
                const SizedBox(height: 8),
                Card(
                  color: Colors.red.shade50,
                  child: ListTile(
                    leading: const Icon(Icons.warning, color: Colors.red),
                    title: const Text('Production Build'),
                    subtitle: const Text('Remove this screen before release'),
                    trailing: ElevatedButton(
                      style: ElevatedButton.styleFrom(backgroundColor: Colors.red),
                      onPressed: () => Navigator.pop(context),
                      child: const Text('Close', style: TextStyle(color: Colors.white)),
                    ),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildSpatialUICard(String title, String subtitle, IconData icon, VoidCallback onTap) {
    return Card(
      elevation: 4,
      child: ListTile(
        leading: CircleAvatar(
          backgroundColor: Colors.blue.shade100,
          child: Icon(icon, color: Colors.blue),
        ),
        title: Text(title, style: const TextStyle(fontWeight: FontWeight.bold)),
        subtitle: Text(subtitle),
        trailing: const Icon(Icons.arrow_forward_ios),
        onTap: onTap,
      ),
    );
  }

  Widget _buildDebugInfoCard(String label, String value) {
    return Card(
      child: ListTile(
        title: Text(label),
        trailing: Text(value, style: const TextStyle(fontWeight: FontWeight.bold)),
      ),
    );
  }
}
