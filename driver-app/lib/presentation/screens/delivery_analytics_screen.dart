import 'package:flutter/material.dart';
import '../components/delivery_charts.dart';
import '../components/delivery_bubble_chart.dart';
import '../design/spatial_ui.dart';

class DeliveryAnalyticsScreen extends StatefulWidget {
  const DeliveryAnalyticsScreen({super.key});

  @override
  State<DeliveryAnalyticsScreen> createState() => _DeliveryAnalyticsScreenState();
}

class _DeliveryAnalyticsScreenState extends State<DeliveryAnalyticsScreen> {
  bool isDark = false;

  @override
  Widget build(BuildContext context) {
    // Check if the app is in dark mode
    final brightness = Theme.of(context).brightness;
    isDark = brightness == Brightness.dark;

    return Scaffold(
      appBar: AppBar(
        title: const Text('Delivery Analytics'),
        actions: [
          IconButton(
            icon: Icon(isDark ? Icons.light_mode : Icons.dark_mode),
            onPressed: () {
              // This is a simple toggle for demo purposes
              // In a real app, you would use a ThemeProvider or similar
              setState(() {
                isDark = !isDark;
              });
            },
          ),
        ],
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            _buildSectionTitle('Weekly Delivery Summary'),
            const SizedBox(height: 8),
            _buildMetricCards(),
            const SizedBox(height: 24),
            
            _buildSectionTitle('Weekly Performance'),
            const SizedBox(height: 8),
            _buildChartCard(
              'Weekly Delivery Trend',
              'Number of deliveries completed by day',
              DeliveryAreaChart(isDark: isDark),
            ),
            const SizedBox(height: 16),
            
            _buildSectionTitle('Daily Performance'),
            const SizedBox(height: 8),
            _buildChartCard(
              'Daily Deliveries by Day of Week',
              'Number of deliveries completed by day',
              DeliveryBarChart(isDark: isDark),
            ),
            const SizedBox(height: 16),
            
            _buildSectionTitle('Delivery Distribution'),
            const SizedBox(height: 8),
            _buildChartCard(
              'Delivery Types',
              'Distribution of delivery types',
              DeliveryTypePieChart(isDark: isDark),
            ),
            const SizedBox(height: 16),
            
            _buildSectionTitle('Delivery Efficiency'),
            const SizedBox(height: 8),
            _buildChartCard(
              'Distance vs Time',
              'Bubble size represents package size',
              DeliveryScatterChart(isDark: isDark),
            ),
            const SizedBox(height: 16),

            _buildSectionTitle('Regional Performance'),
            const SizedBox(height: 8),
            _buildChartCard(
              'Region Analysis',
              'Delivery volume by region with on-time performance',
              DeliveryBubbleChart(isDark: isDark),
            ),
            const SizedBox(height: 16),
            
            _buildSectionTitle('Delivery Service Types'),
            const SizedBox(height: 8),
            _buildChartCard(
              'Service Type Performance',
              'Performance metrics by delivery service type',
              DeliveryRadarChart(isDark: isDark),
            ),
            const SizedBox(height: 32),
          ],
        ),
      ),
    );
  }

  Widget _buildSectionTitle(String title) {
    return Text(
      title,
      style: TextStyle(
        fontSize: 18,
        fontWeight: FontWeight.bold,
        color: isDark
            ? SpatialDesignSystem.textDarkPrimaryColor
            : SpatialDesignSystem.textPrimaryColor,
      ),
    );
  }

  Widget _buildChartCard(String title, String subtitle, Widget chart) {
    return Card(
      elevation: 2,
      color: isDark
          ? SpatialDesignSystem.darkSurfaceColor
          : SpatialDesignSystem.surfaceColor,
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              title,
              style: TextStyle(
                fontSize: 16,
                fontWeight: FontWeight.bold,
                color: isDark
                    ? SpatialDesignSystem.textDarkPrimaryColor
                    : SpatialDesignSystem.textPrimaryColor,
              ),
            ),
            const SizedBox(height: 4),
            Text(
              subtitle,
              style: TextStyle(
                fontSize: 12,
                color: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
              ),
            ),
            const SizedBox(height: 16),
            chart,
          ],
        ),
      ),
    );
  }

  Widget _buildMetricCards() {
    return GridView.count(
      shrinkWrap: true,
      crossAxisCount: 2,
      crossAxisSpacing: 16,
      mainAxisSpacing: 16,
      childAspectRatio: 2.5,
      physics: const NeverScrollableScrollPhysics(),
      children: [
        _buildMetricCard(
          'Total Deliveries', 
          '158', 
          Icons.local_shipping,
          SpatialDesignSystem.primaryColor,
          '+12% vs last week',
        ),
        _buildMetricCard(
          'On-time Rate', 
          '94%', 
          Icons.timer,
          SpatialDesignSystem.successColor,
          '+2% vs last week',
        ),
        _buildMetricCard(
          'Avg. Distance', 
          '7.8 km', 
          Icons.route,
          SpatialDesignSystem.warningColor,
          '-0.5 km vs last week',
        ),
        _buildMetricCard(
          'Customer Rating', 
          '4.8', 
          Icons.star,
          SpatialDesignSystem.accentColor,
          'Same as last week',
        ),
      ],
    );
  }

  Widget _buildMetricCard(
    String title, 
    String value, 
    IconData icon, 
    Color color,
    String comparison,
  ) {
    return Card(
      elevation: 2,
      color: isDark
          ? SpatialDesignSystem.darkSurfaceColor
          : SpatialDesignSystem.surfaceColor,
      child: Padding(
        padding: const EdgeInsets.all(12.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                Icon(
                  icon,
                  size: 18,
                  color: color,
                ),
                const SizedBox(width: 8),
                Expanded(
                  child: Text(
                    title,
                    style: TextStyle(
                      fontSize: 12,
                      fontWeight: FontWeight.w500,
                      color: isDark
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor,
                    ),
                    overflow: TextOverflow.ellipsis,
                  ),
                ),
              ],
            ),
            const SizedBox(height: 4),
            Text(
              value,
              style: TextStyle(
                fontSize: 20,
                fontWeight: FontWeight.bold,
                color: isDark
                    ? SpatialDesignSystem.textDarkPrimaryColor
                    : SpatialDesignSystem.textPrimaryColor,
              ),
            ),
            const SizedBox(height: 2),
            Text(
              comparison,
              style: TextStyle(
                fontSize: 10,
                color: isDark
                    ? SpatialDesignSystem.textDarkSecondaryColor
                    : SpatialDesignSystem.textSecondaryColor,
              ),
            ),
          ],
        ),
      ),
    );
  }
}
