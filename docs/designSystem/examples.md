# Usage Examples

This document provides practical examples of the Spatial UI design system in real-world screens. These examples demonstrate how components and patterns work together to create cohesive, user-friendly interfaces.

## Driver Dashboard

![Driver Dashboard](https://via.placeholder.com/500x900/4264FB/FFFFFF?text=Driver+Dashboard)

The driver dashboard is the home screen for delivery drivers, providing an overview of current tasks and quick access to key functions.

### Implementation

```dart
class DriverDashboard extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return SpatialComponents.backgroundContainer(
      useDarkMode: true,
      child: Scaffold(
        backgroundColor: Colors.transparent,
        appBar: AppBar(
          title: Text('KTC Driver'),
          backgroundColor: Colors.transparent,
          elevation: 0,
          actions: [
            IconButton(
              icon: Icon(Icons.notifications),
              onPressed: () {
                // Show notifications
              },
            ),
            IconButton(
              icon: Icon(Icons.account_circle),
              onPressed: () {
                // Show profile
              },
            ),
          ],
        ),
        body: SafeArea(
          child: ListView(
            padding: EdgeInsets.all(SpatialTheme.spaceMD),
            children: [
              // Driver stats summary
              SpatialComponents.glassContainer(
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.spaceAround,
                  children: [
                    _buildStatItem(
                      title: 'Today\'s Deliveries',
                      value: '12',
                      icon: Icons.local_shipping,
                    ),
                    _buildStatItem(
                      title: 'Completed',
                      value: '8',
                      icon: Icons.check_circle,
                    ),
                    _buildStatItem(
                      title: 'Earnings',
                      value: '₫840K',
                      icon: Icons.monetization_on,
                    ),
                  ],
                ),
                padding: EdgeInsets.symmetric(
                  vertical: SpatialTheme.spaceMD,
                  horizontal: SpatialTheme.spaceSM,
                ),
              ),
              SizedBox(height: SpatialTheme.spaceMD),
              
              // Current delivery
              if (hasActiveDelivery) _buildCurrentDelivery(),
              
              SizedBox(height: SpatialTheme.spaceMD),
              
              // Section title
              Text(
                'Upcoming Deliveries',
                style: SpatialTheme.textTheme.headlineSmall?.copyWith(
                  color: Colors.white,
                ),
              ),
              SizedBox(height: SpatialTheme.spaceSM),
              
              // Upcoming deliveries list
              _buildUpcomingDeliveries(),
            ],
          ),
        ),
        bottomNavigationBar: _buildBottomNavigation(),
        floatingActionButton: SpatialComponents.spatialFAB(
          icon: Icons.map,
          onPressed: () {
            // Open navigation map
          },
        ),
      ),
    );
  }
  
  Widget _buildStatItem({required String title, required String value, required IconData icon}) {
    return Column(
      children: [
        Icon(
          icon,
          color: Colors.white.withOpacity(0.9),
          size: 28,
        ),
        SizedBox(height: SpatialTheme.spaceXS),
        Text(
          value,
          style: TextStyle(
            fontSize: 20,
            fontWeight: FontWeight.bold,
            color: Colors.white,
          ),
        ),
        SizedBox(height: 2),
        Text(
          title,
          style: TextStyle(
            fontSize: 12,
            color: Colors.white.withOpacity(0.7),
          ),
        ),
      ],
    );
  }
  
  Widget _buildCurrentDelivery() {
    return SpatialComponents.spatialCard(
      useDarkMode: true,
      glowing: true,
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              SpatialComponents.statusBadge(
                text: 'In Progress',
                backgroundColor: SpatialTheme.primaryCyan,
                icon: Icons.directions_car,
              ),
              Spacer(),
              Text(
                'ETA: 15 min',
                style: TextStyle(
                  color: Colors.white.withOpacity(0.7),
                ),
              ),
            ],
          ),
          SizedBox(height: SpatialTheme.spaceSM),
          Text(
            'Order #VN4829',
            style: SpatialTheme.textTheme.titleLarge?.copyWith(
              color: Colors.white,
            ),
          ),
          SizedBox(height: SpatialTheme.spaceXS),
          Text(
            'Nguyen Van A - 0912345678',
            style: TextStyle(
              color: Colors.white.withOpacity(0.9),
            ),
          ),
          SizedBox(height: SpatialTheme.spaceXS),
          Text(
            '123 Le Loi St, District 1, HCMC',
            style: TextStyle(
              color: Colors.white.withOpacity(0.7),
            ),
          ),
          SizedBox(height: SpatialTheme.spaceMD),
          Row(
            children: [
              Expanded(
                child: SpatialComponents.gradientButton(
                  text: 'Navigate',
                  icon: Icons.navigation,
                  onPressed: () {
                    // Open navigation
                  },
                  gradient: SpatialTheme.accentGradient,
                ),
              ),
              SizedBox(width: SpatialTheme.spaceSM),
              SpatialComponents.iconButton(
                icon: Icons.phone,
                onPressed: () {
                  // Call customer
                },
                color: Colors.white,
                elevated: true,
              ),
              SizedBox(width: SpatialTheme.spaceSM),
              SpatialComponents.iconButton(
                icon: Icons.message,
                onPressed: () {
                  // Message customer
                },
                color: Colors.white,
                elevated: true,
              ),
            ],
          ),
        ],
      ),
    );
  }
  
  Widget _buildUpcomingDeliveries() {
    return Column(
      children: List.generate(
        3,
        (index) => Padding(
          padding: EdgeInsets.only(bottom: SpatialTheme.spaceMD),
          child: SpatialComponents.glassContainer(
            child: Row(
              children: [
                Container(
                  width: 50,
                  height: 50,
                  decoration: BoxDecoration(
                    shape: BoxShape.circle,
                    color: SpatialTheme.primaryBlue.withOpacity(0.2),
                  ),
                  child: Center(
                    child: Icon(
                      Icons.inventory_2,
                      color: Colors.white,
                    ),
                  ),
                ),
                SizedBox(width: SpatialTheme.spaceSM),
                Expanded(
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(
                        'Order #VN48${30 + index}',
                        style: TextStyle(
                          fontWeight: FontWeight.bold,
                          color: Colors.white,
                        ),
                      ),
                      SizedBox(height: 4),
                      Text(
                        '456 Nguyen Hue Blvd, D1',
                        style: TextStyle(
                          color: Colors.white.withOpacity(0.7),
                          fontSize: 13,
                        ),
                      ),
                    ],
                  ),
                ),
                Column(
                  crossAxisAlignment: CrossAxisAlignment.end,
                  children: [
                    SpatialComponents.statusBadge(
                      text: 'Scheduled',
                      backgroundColor: SpatialTheme.primaryPurple,
                    ),
                    SizedBox(height: 4),
                    Text(
                      '${1 + index}:30 PM',
                      style: TextStyle(
                        color: Colors.white.withOpacity(0.7),
                        fontSize: 13,
                      ),
                    ),
                  ],
                ),
              ],
            ),
            padding: EdgeInsets.all(SpatialTheme.spaceSM),
          ),
        ),
      ),
    );
  }
  
  Widget _buildBottomNavigation() {
    return SpatialComponents.glassContainer(
      borderRadius: BorderRadius.only(
        topLeft: Radius.circular(SpatialTheme.radiusLG),
        topRight: Radius.circular(SpatialTheme.radiusLG),
      ),
      child: BottomNavigationBar(
        backgroundColor: Colors.transparent,
        elevation: 0,
        selectedItemColor: Colors.white,
        unselectedItemColor: Colors.white.withOpacity(0.5),
        items: [
          BottomNavigationBarItem(
            icon: Icon(Icons.home),
            label: 'Home',
          ),
          BottomNavigationBarItem(
            icon: Icon(Icons.list_alt),
            label: 'Orders',
          ),
          BottomNavigationBarItem(
            icon: Icon(Icons.history),
            label: 'History',
          ),
          BottomNavigationBarItem(
            icon: Icon(Icons.person),
            label: 'Profile',
          ),
        ],
        currentIndex: 0,
        onTap: (index) {
          // Handle navigation
        },
      ),
    );
  }
}
```

### Key Design Considerations

- **Hierarchy**: Important information (current delivery) is emphasized with glowing effects
- **Status indicators**: Clear visual status badges show delivery states
- **Accessibility**: High contrast text on dark backgrounds for readability
- **Quick actions**: FAB for the most common action (navigation)
- **Context**: Sufficient information without overwhelming the driver
- **Consistency**: Unified visual language through the Spatial UI components

## Order Details Screen

![Order Details](https://via.placeholder.com/500x900/4264FB/FFFFFF?text=Order+Details)

Detailed view of a delivery order with package information, customer details, and delivery status.

### Implementation

```dart
class OrderDetailsScreen extends StatefulWidget {
  final String orderId;
  
  const OrderDetailsScreen({required this.orderId});
  
  @override
  _OrderDetailsScreenState createState() => _OrderDetailsScreenState();
}

class _OrderDetailsScreenState extends State<OrderDetailsScreen> with SingleTickerProviderStateMixin {
  late TabController _tabController;
  
  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: 3, vsync: this);
  }
  
  @override
  void dispose() {
    _tabController.dispose();
    super.dispose();
  }
  
  @override
  Widget build(BuildContext context) {
    return SpatialComponents.backgroundContainer(
      useDarkMode: true,
      child: Scaffold(
        backgroundColor: Colors.transparent,
        appBar: AppBar(
          title: Text('Order #${widget.orderId}'),
          backgroundColor: Colors.transparent,
          elevation: 0,
          actions: [
            IconButton(
              icon: Icon(Icons.more_vert),
              onPressed: () {
                // Show options menu
              },
            ),
          ],
        ),
        body: SafeArea(
          child: Column(
            children: [
              // Status indicator
              SpatialComponents.glassContainer(
                child: Row(
                  children: [
                    Container(
                      width: 10,
                      height: 10,
                      decoration: BoxDecoration(
                        color: DeliveryStatusColors.inTransit,
                        shape: BoxShape.circle,
                      ),
                    ),
                    SizedBox(width: SpatialTheme.spaceSM),
                    Text(
                      'In Transit',
                      style: TextStyle(
                        color: Colors.white,
                        fontWeight: FontWeight.bold,
                      ),
                    ),
                    SizedBox(width: SpatialTheme.spaceMD),
                    Expanded(
                      child: LinearProgressIndicator(
                        value: 0.6,
                        backgroundColor: Colors.white.withOpacity(0.2),
                        valueColor: AlwaysStoppedAnimation<Color>(
                          DeliveryStatusColors.inTransit,
                        ),
                      ),
                    ),
                    SizedBox(width: SpatialTheme.spaceSM),
                    Text(
                      '60%',
                      style: TextStyle(
                        color: Colors.white.withOpacity(0.8),
                      ),
                    ),
                  ],
                ),
                margin: EdgeInsets.all(SpatialTheme.spaceMD),
                padding: EdgeInsets.symmetric(
                  horizontal: SpatialTheme.spaceMD,
                  vertical: SpatialTheme.spaceSM,
                ),
              ),
              
              // Tab bar
              SpatialComponents.glassContainer(
                child: TabBar(
                  controller: _tabController,
                  tabs: [
                    Tab(text: 'Details'),
                    Tab(text: 'Route'),
                    Tab(text: 'Timeline'),
                  ],
                  indicator: BoxDecoration(
                    gradient: SpatialTheme.primaryGradient,
                    borderRadius: BorderRadius.circular(SpatialTheme.radiusSM),
                  ),
                  labelColor: Colors.white,
                  unselectedLabelColor: Colors.white.withOpacity(0.6),
                ),
                margin: EdgeInsets.symmetric(horizontal: SpatialTheme.spaceMD),
                padding: EdgeInsets.all(SpatialTheme.spaceXS),
                borderRadius: SpatialTheme.borderRadiusMedium,
              ),
              
              // Tab content
              Expanded(
                child: TabBarView(
                  controller: _tabController,
                  children: [
                    _buildDetailsTab(),
                    _buildRouteTab(),
                    _buildTimelineTab(),
                  ],
                ),
              ),
            ],
          ),
        ),
        bottomNavigationBar: _buildBottomActions(),
      ),
    );
  }
  
  Widget _buildDetailsTab() {
    return ListView(
      padding: EdgeInsets.all(SpatialTheme.spaceMD),
      children: [
        // Customer information
        SpatialComponents.spatialCard(
          useDarkMode: true,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'Customer',
                style: SpatialTheme.textTheme.titleLarge?.copyWith(
                  color: Colors.white,
                ),
              ),
              SizedBox(height: SpatialTheme.spaceSM),
              Row(
                children: [
                  CircleAvatar(
                    backgroundColor: SpatialTheme.primaryPurple.withOpacity(0.2),
                    child: Icon(
                      Icons.person,
                      color: Colors.white,
                    ),
                  ),
                  SizedBox(width: SpatialTheme.spaceSM),
                  Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(
                        'Nguyen Van A',
                        style: TextStyle(
                          color: Colors.white,
                          fontWeight: FontWeight.w500,
                        ),
                      ),
                      Text(
                        '0912345678',
                        style: TextStyle(
                          color: Colors.white.withOpacity(0.7),
                        ),
                      ),
                    ],
                  ),
                  Spacer(),
                  SpatialComponents.iconButton(
                    icon: Icons.phone,
                    onPressed: () {
                      // Call customer
                    },
                    color: Colors.white,
                  ),
                ],
              ),
            ],
          ),
        ),
        
        SizedBox(height: SpatialTheme.spaceMD),
        
        // Delivery address
        SpatialComponents.spatialCard(
          useDarkMode: true,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'Delivery Address',
                style: SpatialTheme.textTheme.titleLarge?.copyWith(
                  color: Colors.white,
                ),
              ),
              SizedBox(height: SpatialTheme.spaceSM),
              Row(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Icon(
                    Icons.location_on,
                    color: SpatialTheme.primaryCyan,
                  ),
                  SizedBox(width: SpatialTheme.spaceSM),
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          '123 Le Loi St, District 1, HCMC',
                          style: TextStyle(
                            color: Colors.white,
                          ),
                        ),
                        SizedBox(height: 4),
                        Text(
                          'Apartment 5B, Building C',
                          style: TextStyle(
                            color: Colors.white.withOpacity(0.7),
                            fontSize: 13,
                          ),
                        ),
                        SizedBox(height: 4),
                        Text(
                          'Note: Please call before delivery',
                          style: TextStyle(
                            color: SpatialTheme.warning,
                            fontSize: 13,
                            fontStyle: FontStyle.italic,
                          ),
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ],
          ),
        ),
        
        SizedBox(height: SpatialTheme.spaceMD),
        
        // Package details
        SpatialComponents.spatialCard(
          useDarkMode: true,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'Package Details',
                style: SpatialTheme.textTheme.titleLarge?.copyWith(
                  color: Colors.white,
                ),
              ),
              SizedBox(height: SpatialTheme.spaceMD),
              _buildPackageDetailRow(
                title: 'Items',
                value: '3',
                icon: Icons.inventory_2,
              ),
              Divider(color: Colors.white.withOpacity(0.1)),
              _buildPackageDetailRow(
                title: 'Weight',
                value: '1.5 kg',
                icon: Icons.scale,
              ),
              Divider(color: Colors.white.withOpacity(0.1)),
              _buildPackageDetailRow(
                title: 'Size',
                value: 'Medium',
                icon: Icons.aspect_ratio,
              ),
              Divider(color: Colors.white.withOpacity(0.1)),
              _buildPackageDetailRow(
                title: 'Payment',
                value: 'Prepaid',
                icon: Icons.payment,
              ),
            ],
          ),
        ),
      ],
    );
  }
  
  Widget _buildPackageDetailRow({
    required String title,
    required String value,
    required IconData icon,
  }) {
    return Padding(
      padding: EdgeInsets.symmetric(vertical: SpatialTheme.spaceSM),
      child: Row(
        children: [
          Icon(
            icon,
            color: Colors.white.withOpacity(0.7),
            size: 20,
          ),
          SizedBox(width: SpatialTheme.spaceSM),
          Text(
            title,
            style: TextStyle(
              color: Colors.white.withOpacity(0.7),
            ),
          ),
          Spacer(),
          Text(
            value,
            style: TextStyle(
              color: Colors.white,
              fontWeight: FontWeight.w500,
            ),
          ),
        ],
      ),
    );
  }
  
  Widget _buildRouteTab() {
    // Route map view implementation
    return Center(
      child: Text(
        'Route Map',
        style: TextStyle(color: Colors.white),
      ),
    );
  }
  
  Widget _buildTimelineTab() {
    // Timeline implementation
    return Center(
      child: Text(
        'Delivery Timeline',
        style: TextStyle(color: Colors.white),
      ),
    );
  }
  
  Widget _buildBottomActions() {
    return SpatialComponents.glassContainer(
      padding: EdgeInsets.symmetric(
        horizontal: SpatialTheme.spaceMD,
        vertical: SpatialTheme.spaceSM,
      ),
      borderRadius: BorderRadius.only(
        topLeft: Radius.circular(SpatialTheme.radiusLG),
        topRight: Radius.circular(SpatialTheme.radiusLG),
      ),
      child: Row(
        children: [
          Expanded(
            child: SpatialComponents.gradientButton(
              text: 'Confirm Delivery',
              onPressed: () {
                // Handle delivery confirmation
              },
              icon: Icons.check,
            ),
          ),
          SizedBox(width: SpatialTheme.spaceMD),
          SpatialComponents.iconButton(
            icon: Icons.camera_alt,
            onPressed: () {
              // Take proof of delivery photo
            },
            elevated: true,
            color: Colors.white,
          ),
          SizedBox(width: SpatialTheme.spaceSM),
          SpatialComponents.iconButton(
            icon: Icons.cancel,
            onPressed: () {
              // Report delivery issue
            },
            elevated: true,
            color: SpatialTheme.error,
          ),
        ],
      ),
    );
  }
}
```

### Key Design Considerations

- **Organization**: Tabbed interface to separate different aspects of the order
- **Status visualization**: Clear progress indicator for delivery status
- **Information architecture**: Logically grouped information in cards
- **Action priority**: Primary action (confirm delivery) is emphasized
- **Accessibility**: Critical information is presented first
- **Visual consistency**: Uniform card and container styling

---

© 2025 KTC Logistics | Design System Team
