import 'package:flutter/material.dart';
import '../../design/spatial_ui.dart';

class RouteMapScreen extends StatefulWidget {
  final String routeId;

  const RouteMapScreen({
    Key? key,
    required this.routeId,
  }) : super(key: key);

  @override
  State<RouteMapScreen> createState() => _RouteMapScreenState();
}

class _RouteMapScreenState extends State<RouteMapScreen> {
  final bool _isFollowingUser = true;

  @override
  Widget build(BuildContext context) {
    final isDark = Theme.of(context).brightness == Brightness.dark;
    
    return Scaffold(
      backgroundColor: isDark
          ? SpatialDesignSystem.darkBackgroundColor
          : SpatialDesignSystem.backgroundColor,
      appBar: AppBar(
        title: Text(
          "Lộ Trình #${widget.routeId}",
          style: SpatialDesignSystem.subtitleLarge.copyWith(
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
        ),
        backgroundColor: Colors.transparent,
        elevation: 0,
        leading: IconButton(
          icon: Icon(
            Icons.arrow_back,
            color: isDark
                ? SpatialDesignSystem.textDarkPrimaryColor
                : SpatialDesignSystem.textPrimaryColor,
          ),
          onPressed: () => Navigator.pop(context),
        ),
        actions: [
          IconButton(
            icon: Icon(
              _isFollowingUser ? Icons.location_on : Icons.location_off,
              color: _isFollowingUser ? SpatialDesignSystem.primaryColor : isDark
                ? SpatialDesignSystem.textDarkSecondaryColor
                : SpatialDesignSystem.textSecondaryColor,
            ),
            onPressed: () {
              // Toggle location tracking
            },
          ),
        ],
      ),
      body: Column(
        children: [
          // Thông tin lộ trình
          Padding(
            padding: const EdgeInsets.all(16),
            child: GlassCard(
              padding: const EdgeInsets.all(16),
              child: Row(
                children: [
                  // Thời gian
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          "Thời gian",
                          style: TextStyle(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                          ),
                        ),
                        const SizedBox(height: 4),
                        Text(
                          "25 phút",
                          style: SpatialDesignSystem.subtitleMedium.copyWith(
                            fontWeight: FontWeight.bold,
                          ),
                        ),
                      ],
                    ),
                  ),
                  
                  // Khoảng cách
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          "Khoảng cách",
                          style: TextStyle(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                          ),
                        ),
                        const SizedBox(height: 4),
                        Text(
                          "3.5 km",
                          style: SpatialDesignSystem.subtitleMedium.copyWith(
                            fontWeight: FontWeight.bold,
                          ),
                        ),
                      ],
                    ),
                  ),
                  
                  // Thời gian đến
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          "Đến lúc",
                          style: TextStyle(
                            color: isDark
                                ? SpatialDesignSystem.textDarkSecondaryColor
                                : SpatialDesignSystem.textSecondaryColor,
                          ),
                        ),
                        const SizedBox(height: 4),
                        Text(
                          "14:30",
                          style: SpatialDesignSystem.subtitleMedium.copyWith(
                            fontWeight: FontWeight.bold,
                          ),
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ),
          ),
          
          // Map Placeholder (tạm thời)
          Expanded(
            child: Padding(
              padding: const EdgeInsets.only(left: 16, right: 16, bottom: 16),
              child: GlassCard(
                padding: const EdgeInsets.all(16),
                child: Center(
                  child: Column(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      const Icon(
                        Icons.map,
                        size: 64,
                        color: SpatialDesignSystem.primaryColor,
                      ),
                      const SizedBox(height: 16),
                      Text(
                        "Bản Đồ Lộ Trình",
                        style: SpatialDesignSystem.headingSmall,
                      ),
                      const SizedBox(height: 8),
                      Text(
                        "Đang tải bản đồ...\nMapbox sẽ hiển thị tại đây",
                        style: SpatialDesignSystem.bodyMedium,
                        textAlign: TextAlign.center,
                      ),
                      const SizedBox(height: 24),
                      Row(
                        mainAxisAlignment: MainAxisAlignment.center,
                        children: [
                          Column(
                            children: [
                              Container(
                                padding: const EdgeInsets.all(8),
                                decoration: BoxDecoration(
                                  color: Colors.blue.withOpacity(0.2),
                                  borderRadius: BorderRadius.circular(8),
                                ),
                                child: const Icon(
                                  Icons.location_on,
                                  color: Colors.blue,
                                ),
                              ),
                              const SizedBox(height: 4),
                              const Text("Điểm đi"),
                            ],
                          ),
                          Container(
                            width: 100,
                            height: 2,
                            color: Colors.grey.withOpacity(0.5),
                            margin: const EdgeInsets.symmetric(horizontal: 16),
                          ),
                          Column(
                            children: [
                              Container(
                                padding: const EdgeInsets.all(8),
                                decoration: BoxDecoration(
                                  color: Colors.red.withOpacity(0.2),
                                  borderRadius: BorderRadius.circular(8),
                                ),
                                child: const Icon(
                                  Icons.location_on,
                                  color: Colors.red,
                                ),
                              ),
                              const SizedBox(height: 4),
                              const Text("Điểm đến"),
                            ],
                          ),
                        ],
                      ),
                    ],
                  ),
                ),
              ),
            ),
          ),
          
          // Bottom Action Bar
          SafeArea(
            child: Padding(
              padding: const EdgeInsets.all(16),
              child: SpatialButton(
                text: "Đã Hoàn Thành Giao Hàng",
                onPressed: () {
                  Navigator.pop(context);
                },
                width: double.infinity,
                isGradient: true,
                gradient: SpatialDesignSystem.successGradient,
                iconData: Icons.check_circle,
              ),
            ),
          ),
        ],
      ),
    );
  }
}
