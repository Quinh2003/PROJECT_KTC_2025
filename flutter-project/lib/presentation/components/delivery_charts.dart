import 'package:fl_chart/fl_chart.dart';
import 'package:flutter/material.dart';
import '../design/spatial_ui.dart';

class DeliveryAreaChart extends StatelessWidget {
  final bool isDark;

  const DeliveryAreaChart({super.key, required this.isDark});

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1.7,
      child: LineChart(
        mainData(),
      ),
    );
  }

  Widget bottomTitleWidgets(double value, TitleMeta meta) {
    final style = TextStyle(
      fontSize: 12, 
      fontWeight: FontWeight.w500,
      color: isDark
          ? SpatialDesignSystem.textDarkSecondaryColor
          : SpatialDesignSystem.textSecondaryColor,
    );
    
    Widget text;
    switch (value.toInt()) {
      case 0:
        text = Text('Mon', style: style);
        break;
      case 1:
        text = Text('Tue', style: style);
        break;
      case 2:
        text = Text('Wed', style: style);
        break;
      case 3:
        text = Text('Thu', style: style);
        break;
      case 4:
        text = Text('Fri', style: style);
        break;
      case 5:
        text = Text('Sat', style: style);
        break;
      case 6:
        text = Text('Sun', style: style);
        break;
      default:
        text = Text('', style: style);
        break;
    }

    return SideTitleWidget(
      meta: meta,
      child: text,
    );
  }

  Widget leftTitleWidgets(double value, TitleMeta meta) {
    final style = TextStyle(
      fontSize: 12,
      color: isDark
          ? SpatialDesignSystem.textDarkSecondaryColor
          : SpatialDesignSystem.textSecondaryColor,
      fontWeight: FontWeight.w500,
    );
    
    String text;
    if (value == 0) {
      text = '0';
    } else if (value == 10) {
      text = '10';
    } else if (value == 20) {
      text = '20';
    } else if (value == 30) {
      text = '30';
    } else if (value == 40) {
      text = '40';
    } else {
      return Container();
    }

    return SideTitleWidget(
      meta: meta,
      child: Text(text, style: style),
    );
  }

  LineChartData mainData() {
    final List<Color> gradientColors = [
      SpatialDesignSystem.primaryColor,
      SpatialDesignSystem.primaryColor.withOpacity(0.5),
    ];
    
    return LineChartData(
      gridData: FlGridData(
        show: true,
        drawVerticalLine: true,
        horizontalInterval: 10,
        verticalInterval: 1,
        getDrawingHorizontalLine: (value) {
          return FlLine(
            color: isDark 
                ? Colors.white.withOpacity(0.1)
                : Colors.black.withOpacity(0.1),
            strokeWidth: 0.5,
          );
        },
        getDrawingVerticalLine: (value) {
          return FlLine(
            color: isDark 
                ? Colors.white.withOpacity(0.1)
                : Colors.black.withOpacity(0.1),
            strokeWidth: 0.5,
          );
        },
      ),
      titlesData: FlTitlesData(
        show: true,
        rightTitles: const AxisTitles(
          sideTitles: SideTitles(showTitles: false),
        ),
        topTitles: const AxisTitles(
          sideTitles: SideTitles(showTitles: false),
        ),
        bottomTitles: AxisTitles(
          sideTitles: SideTitles(
            showTitles: true,
            reservedSize: 30,
            interval: 1,
            getTitlesWidget: bottomTitleWidgets,
          ),
        ),
        leftTitles: AxisTitles(
          sideTitles: SideTitles(
            showTitles: true,
            interval: 10,
            getTitlesWidget: leftTitleWidgets,
            reservedSize: 42,
          ),
        ),
      ),
      borderData: FlBorderData(
        show: true,
        border: Border.all(
          color: isDark 
              ? Colors.white.withOpacity(0.2)
              : Colors.black.withOpacity(0.2),
        ),
      ),
      minX: 0,
      maxX: 6,
      minY: 0,
      maxY: 40,
      lineBarsData: [
        LineChartBarData(
          spots: const [
            FlSpot(0, 10),
            FlSpot(1, 18),
            FlSpot(2, 23),
            FlSpot(3, 17),
            FlSpot(4, 30),
            FlSpot(5, 32),
            FlSpot(6, 28),
          ],
          isCurved: true,
          gradient: LinearGradient(
            colors: gradientColors,
          ),
          barWidth: 3,
          isStrokeCapRound: true,
          dotData: const FlDotData(
            show: false,
          ),
          belowBarData: BarAreaData(
            show: true,
            gradient: LinearGradient(
              colors: gradientColors
                  .map((color) => color.withOpacity(0.3))
                  .toList(),
            ),
          ),
        ),
      ],
    );
  }
}

class DeliveryTypePieChart extends StatelessWidget {
  final bool isDark;

  const DeliveryTypePieChart({super.key, required this.isDark});

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1.3,
      child: PieChart(
        PieChartData(
          sectionsSpace: 2,
          centerSpaceRadius: 40,
          sections: [
            PieChartSectionData(
              color: SpatialDesignSystem.primaryColor,
              value: 40,
              title: '40%',
              radius: 60,
              titleStyle: TextStyle(
                fontSize: 14,
                fontWeight: FontWeight.bold,
                color: isDark ? Colors.white : Colors.black,
              ),
              titlePositionPercentageOffset: 0.55,
            ),
            PieChartSectionData(
              color: SpatialDesignSystem.accentColor,
              value: 30,
              title: '30%',
              radius: 65,
              titleStyle: TextStyle(
                fontSize: 14,
                fontWeight: FontWeight.bold,
                color: isDark ? Colors.white : Colors.black,
              ),
              titlePositionPercentageOffset: 0.55,
            ),
            PieChartSectionData(
              color: SpatialDesignSystem.warningColor,
              value: 15,
              title: '15%',
              radius: 50,
              titleStyle: TextStyle(
                fontSize: 14,
                fontWeight: FontWeight.bold,
                color: isDark ? Colors.white : Colors.black,
              ),
              titlePositionPercentageOffset: 0.6,
            ),
            PieChartSectionData(
              color: SpatialDesignSystem.successColor,
              value: 15,
              title: '15%',
              radius: 55,
              titleStyle: TextStyle(
                fontSize: 14,
                fontWeight: FontWeight.bold,
                color: isDark ? Colors.white : Colors.black,
              ),
              titlePositionPercentageOffset: 0.55,
            ),
          ],
        ),
      ),
    );
  }
}

class DeliveryScatterChart extends StatefulWidget {
  final bool isDark;

  const DeliveryScatterChart({super.key, required this.isDark});

  @override
  State<DeliveryScatterChart> createState() => _DeliveryScatterChartState();
}

class _DeliveryScatterChartState extends State<DeliveryScatterChart> {
  int touchedIndex = -1;

  // Data points: (x:distance in km, y:time in minutes, size:package size)
  final List<(double, double, double)> deliveryData = [
    (4.0, 15.0, 4.0),  // 4km, 15min, small package
    (2.0, 10.0, 12.0), // 2km, 10min, medium package
    (8.0, 25.0, 8.0),  // 8km, 25min, medium package
    (12.0, 35.0, 20.0), // 12km, 35min, large package
    (5.0, 18.0, 14.0), // 5km, 18min, medium package
    (9.0, 28.0, 18.0), // 9km, 28min, large package
    (3.0, 12.0, 22.0), // 3km, 12min, large package
    (7.0, 22.0, 10.0), // 7km, 22min, medium package
    (10.0, 32.0, 16.0), // 10km, 32min, large package
    (6.0, 20.0, 6.0),  // 6km, 20min, small package
  ];

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1.7,
      child: ScatterChart(
        ScatterChartData(
          scatterSpots: deliveryData.map((data) {
            final (double distance, double time, double size) = data;
            return ScatterSpot(
              distance,
              time,
              dotPainter: FlDotCirclePainter(
                color: widget.isDark 
                    ? SpatialDesignSystem.primaryColor
                    : SpatialDesignSystem.primaryColor.withOpacity(0.8),
                radius: size / 2,
              ),
            );
          }).toList(),
          minX: 0,
          maxX: 14,
          minY: 0,
          maxY: 40,
          borderData: FlBorderData(
            show: true,
            border: Border.all(
              color: widget.isDark
                  ? Colors.white.withOpacity(0.2)
                  : Colors.black.withOpacity(0.2),
            ),
          ),
          gridData: FlGridData(
            show: true,
            drawHorizontalLine: true,
            horizontalInterval: 5,
            drawVerticalLine: true,
            verticalInterval: 2,
            getDrawingHorizontalLine: (value) {
              return FlLine(
                color: widget.isDark
                    ? Colors.white.withOpacity(0.1)
                    : Colors.black.withOpacity(0.1),
                strokeWidth: 0.5,
              );
            },
            getDrawingVerticalLine: (value) {
              return FlLine(
                color: widget.isDark
                    ? Colors.white.withOpacity(0.1)
                    : Colors.black.withOpacity(0.1),
                strokeWidth: 0.5,
              );
            },
          ),
          titlesData: FlTitlesData(
            show: true,
            rightTitles: const AxisTitles(
              sideTitles: SideTitles(showTitles: false),
            ),
            topTitles: const AxisTitles(
              sideTitles: SideTitles(showTitles: false),
            ),
            bottomTitles: AxisTitles(
              axisNameWidget: Text(
                'Distance (km)',
                style: TextStyle(
                  color: widget.isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              sideTitles: SideTitles(
                showTitles: true,
                interval: 2,
                getTitlesWidget: (value, meta) {
                  return SideTitleWidget(
                    meta: meta,
                    child: Text(
                      value.toInt().toString(),
                      style: TextStyle(
                        fontSize: 10,
                        color: widget.isDark
                            ? SpatialDesignSystem.textDarkSecondaryColor
                            : SpatialDesignSystem.textSecondaryColor,
                      ),
                    ),
                  );
                },
                reservedSize: 28,
              ),
            ),
            leftTitles: AxisTitles(
              axisNameWidget: Text(
                'Time (min)',
                style: TextStyle(
                  color: widget.isDark
                      ? SpatialDesignSystem.textDarkSecondaryColor
                      : SpatialDesignSystem.textSecondaryColor,
                ),
              ),
              sideTitles: SideTitles(
                showTitles: true,
                interval: 5,
                getTitlesWidget: (value, meta) {
                  return SideTitleWidget(
                    meta: meta,
                    child: Text(
                      value.toInt().toString(),
                      style: TextStyle(
                        fontSize: 10,
                        color: widget.isDark
                            ? SpatialDesignSystem.textDarkSecondaryColor
                            : SpatialDesignSystem.textSecondaryColor,
                      ),
                    ),
                  );
                },
                reservedSize: 40,
              ),
            ),
          ),
          scatterTouchData: ScatterTouchData(
            enabled: true,
            handleBuiltInTouches: true,
            touchTooltipData: ScatterTouchTooltipData(
              getTooltipItems: (ScatterSpot touchedBarSpot) {
                return ScatterTooltipItem(
                  'Package:',
                  textStyle: TextStyle(
                    height: 1.2,
                    color: widget.isDark ? Colors.white70 : Colors.black87,
                    fontStyle: FontStyle.italic,
                    fontWeight: FontWeight.bold,
                  ),
                  bottomMargin: 10,
                  children: [
                    TextSpan(
                      text: '\nDistance: ${touchedBarSpot.x.toStringAsFixed(1)} km',
                      style: TextStyle(
                        color: widget.isDark ? Colors.white : Colors.black,
                        fontStyle: FontStyle.normal,
                        fontWeight: FontWeight.normal,
                      ),
                    ),
                    TextSpan(
                      text: '\nTime: ${touchedBarSpot.y.toStringAsFixed(1)} min',
                      style: TextStyle(
                        color: widget.isDark ? Colors.white : Colors.black,
                        fontStyle: FontStyle.normal,
                        fontWeight: FontWeight.normal,
                      ),
                    ),
                    TextSpan(
                      text: '\nSize: ${_getPackageSize(touchedBarSpot)}',
                      style: TextStyle(
                        color: widget.isDark ? Colors.white : Colors.black,
                        fontStyle: FontStyle.normal,
                        fontWeight: FontWeight.normal,
                      ),
                    ),
                  ],
                );
              },
            ),
          ),
        ),
      ),
    );
  }
  
  String _getPackageSize(ScatterSpot spot) {
    // Find the matching data point to determine size
    for (final data in deliveryData) {
      final (double distance, double time, double size) = data;
      if (distance == spot.x && time == spot.y) {
        if (size < 8) return 'Small';
        if (size < 16) return 'Medium';
        return 'Large';
      }
    }
    return 'Unknown';
  }
}

class DeliveryRadarChart extends StatefulWidget {
  final bool isDark;

  const DeliveryRadarChart({super.key, required this.isDark});

  @override
  State<DeliveryRadarChart> createState() => _DeliveryRadarChartState();
}

class _DeliveryRadarChartState extends State<DeliveryRadarChart> {
  int selectedDataSetIndex = -1;

  @override
  Widget build(BuildContext context) {
    final gridColor = widget.isDark
        ? Colors.white.withOpacity(0.3)
        : Colors.black.withOpacity(0.2);
    
    final titleColor = widget.isDark
        ? SpatialDesignSystem.textDarkSecondaryColor
        : SpatialDesignSystem.textSecondaryColor;
    
    final standardColor = SpatialDesignSystem.primaryColor;
    final expressColor = SpatialDesignSystem.accentColor;
    final sameDay = SpatialDesignSystem.warningColor;
    final specialHandling = SpatialDesignSystem.errorColor;

    return AspectRatio(
      aspectRatio: 1.3,
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Delivery Performance',
              style: TextStyle(
                fontSize: 16,
                fontWeight: FontWeight.bold,
                color: widget.isDark
                    ? SpatialDesignSystem.textDarkPrimaryColor
                    : SpatialDesignSystem.textPrimaryColor,
              ),
            ),
            const SizedBox(height: 8),
            
            // Data source legend
            Wrap(
              spacing: 16,
              children: [
                _buildLegendItem('Standard', standardColor),
                _buildLegendItem('Express', expressColor),
                _buildLegendItem('Same Day', sameDay),
                _buildLegendItem('Special', specialHandling),
              ],
            ),
            
            const SizedBox(height: 16),
            
            // Radar chart
            Expanded(
              child: RadarChart(
                RadarChartData(
                  radarTouchData: RadarTouchData(
                    touchCallback: (FlTouchEvent event, response) {
                      if (!event.isInterestedForInteractions) {
                        setState(() {
                          selectedDataSetIndex = -1;
                        });
                        return;
                      }
                      setState(() {
                        selectedDataSetIndex =
                            response?.touchedSpot?.touchedDataSetIndex ?? -1;
                      });
                    },
                  ),
                  dataSets: showingDataSets(
                    standardColor,
                    expressColor,
                    sameDay,
                    specialHandling,
                  ),
                  radarBackgroundColor: Colors.transparent,
                  borderData: FlBorderData(show: false),
                  radarBorderData: BorderSide(color: gridColor, width: 1),
                  titlePositionPercentageOffset: 0.2,
                  titleTextStyle: TextStyle(color: titleColor, fontSize: 12),
                  getTitle: (index, angle) {
                    switch (index) {
                      case 0:
                        return RadarChartTitle(
                          text: 'Speed',
                          angle: angle,
                        );
                      case 1:
                        return RadarChartTitle(
                          text: 'Accuracy',
                          angle: angle,
                        );
                      case 2:
                        return RadarChartTitle(
                          text: 'Cost',
                          angle: angle,
                        );
                      case 3:
                        return RadarChartTitle(
                          text: 'Range',
                          angle: angle,
                        );
                      case 4:
                        return RadarChartTitle(
                          text: 'Satisfaction',
                          angle: angle,
                        );
                      default:
                        return const RadarChartTitle(text: '');
                    }
                  },
                  tickCount: 5,
                  ticksTextStyle: TextStyle(
                    color: Colors.transparent, 
                    fontSize: 10,
                  ),
                  tickBorderData: BorderSide(color: Colors.transparent),
                  gridBorderData: BorderSide(color: gridColor, width: 1),
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }
  
  Widget _buildLegendItem(String label, Color color) {
    return Row(
      mainAxisSize: MainAxisSize.min,
      children: [
        Container(
          width: 12,
          height: 12,
          decoration: BoxDecoration(
            color: color,
            shape: BoxShape.circle,
          ),
        ),
        const SizedBox(width: 4),
        Text(
          label,
          style: TextStyle(
            fontSize: 12,
            color: widget.isDark
                ? SpatialDesignSystem.textDarkSecondaryColor
                : SpatialDesignSystem.textSecondaryColor,
          ),
        ),
      ],
    );
  }

  List<RadarDataSet> showingDataSets(
    Color standardColor,
    Color expressColor,
    Color sameDayColor,
    Color specialColor,
  ) {
    final rawDataSets = [
      // Standard Delivery
      RawDataSet(
        title: 'Standard',
        color: standardColor,
        values: [
          60, // Speed
          85, // Accuracy
          90, // Cost-effectiveness
          70, // Range
          80, // Customer Satisfaction
        ],
      ),
      // Express Delivery
      RawDataSet(
        title: 'Express',
        color: expressColor,
        values: [
          85, // Speed
          80, // Accuracy
          60, // Cost-effectiveness
          65, // Range
          85, // Customer Satisfaction
        ],
      ),
      // Same Day Delivery
      RawDataSet(
        title: 'Same Day',
        color: sameDayColor,
        values: [
          95, // Speed
          75, // Accuracy
          40, // Cost-effectiveness
          50, // Range
          90, // Customer Satisfaction
        ],
      ),
      // Special Handling
      RawDataSet(
        title: 'Special',
        color: specialColor,
        values: [
          70, // Speed
          95, // Accuracy
          30, // Cost-effectiveness
          40, // Range
          95, // Customer Satisfaction
        ],
      ),
    ];

    return rawDataSets.asMap().entries.map((entry) {
      final index = entry.key;
      final rawDataSet = entry.value;

      final isSelected = index == selectedDataSetIndex
          ? true
          : selectedDataSetIndex == -1
              ? true
              : false;

      return RadarDataSet(
        fillColor: isSelected
            ? rawDataSet.color.withOpacity(0.2)
            : rawDataSet.color.withOpacity(0.05),
        borderColor: isSelected
            ? rawDataSet.color
            : rawDataSet.color.withOpacity(0.25),
        entryRadius: isSelected ? 3 : 2,
        dataEntries: rawDataSet.values
            .map((e) => RadarEntry(value: e))
            .toList(),
        borderWidth: isSelected ? 2.3 : 2,
      );
    }).toList();
  }
}

class RawDataSet {
  RawDataSet({
    required this.title,
    required this.color,
    required this.values,
  });

  final String title;
  final Color color;
  final List<double> values;
}

class DeliveryBarChart extends StatelessWidget {
  final bool isDark;

  const DeliveryBarChart({super.key, required this.isDark});

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1.7,
      child: BarChart(
        BarChartData(
          barTouchData: BarTouchData(
            enabled: true,
            touchTooltipData: BarTouchTooltipData(
              tooltipPadding: const EdgeInsets.all(8),
              getTooltipItem: (group, groupIndex, rod, rodIndex) {
                String weekDay;
                switch (group.x) {
                  case 0:
                    weekDay = 'Monday';
                    break;
                  case 1:
                    weekDay = 'Tuesday';
                    break;
                  case 2:
                    weekDay = 'Wednesday';
                    break;
                  case 3:
                    weekDay = 'Thursday';
                    break;
                  case 4:
                    weekDay = 'Friday';
                    break;
                  case 5:
                    weekDay = 'Saturday';
                    break;
                  case 6:
                    weekDay = 'Sunday';
                    break;
                  default:
                    weekDay = '';
                }
                return BarTooltipItem(
                  '$weekDay\n',
                  TextStyle(
                    color: isDark ? Colors.black : Colors.white,
                    fontWeight: FontWeight.bold,
                    fontSize: 14,
                  ),
                  children: <TextSpan>[
                    TextSpan(
                      text: '${rod.toY.round()} deliveries',
                      style: TextStyle(
                        color: isDark ? Colors.black : Colors.white,
                        fontSize: 12,
                        fontWeight: FontWeight.w500,
                      ),
                    ),
                  ],
                );
              },
            ),
          ),
          titlesData: FlTitlesData(
            show: true,
            rightTitles: const AxisTitles(
              sideTitles: SideTitles(showTitles: false),
            ),
            topTitles: const AxisTitles(
              sideTitles: SideTitles(showTitles: false),
            ),
            bottomTitles: AxisTitles(
              sideTitles: SideTitles(
                showTitles: true,
                getTitlesWidget: (double value, TitleMeta meta) {
                  final style = TextStyle(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                    fontWeight: FontWeight.w500,
                    fontSize: 12,
                  );
                  Widget text;
                  switch (value.toInt()) {
                    case 0:
                      text = Text('M', style: style);
                      break;
                    case 1:
                      text = Text('T', style: style);
                      break;
                    case 2:
                      text = Text('W', style: style);
                      break;
                    case 3:
                      text = Text('T', style: style);
                      break;
                    case 4:
                      text = Text('F', style: style);
                      break;
                    case 5:
                      text = Text('S', style: style);
                      break;
                    case 6:
                      text = Text('S', style: style);
                      break;
                    default:
                      text = Text('', style: style);
                      break;
                  }
                  return SideTitleWidget(
                    meta: meta,
                    child: text,
                  );
                },
                reservedSize: 28,
                interval: 1,
              ),
            ),
            leftTitles: AxisTitles(
              sideTitles: SideTitles(
                showTitles: true,
                getTitlesWidget: (double value, TitleMeta meta) {
                  final style = TextStyle(
                    color: isDark
                        ? SpatialDesignSystem.textDarkSecondaryColor
                        : SpatialDesignSystem.textSecondaryColor,
                    fontWeight: FontWeight.w500,
                    fontSize: 12,
                  );
                  
                  String text;
                  if (value == 0) {
                    text = '0';
                  } else if (value == 10) {
                    text = '10';
                  } else if (value == 20) {
                    text = '20';
                  } else if (value == 30) {
                    text = '30';
                  } else {
                    return Container();
                  }
                  
                  return SideTitleWidget(
                    meta: meta,
                    child: Text(text, style: style),
                  );
                },
                reservedSize: 42,
                interval: 10,
              ),
            ),
          ),
          borderData: FlBorderData(
            show: true,
            border: Border.all(
              color: isDark
                  ? Colors.white.withOpacity(0.2)
                  : Colors.black.withOpacity(0.2),
            ),
          ),
          barGroups: [
            BarChartGroupData(
              x: 0,
              barRods: [
                BarChartRodData(
                  toY: 15,
                  gradient: LinearGradient(
                    colors: [
                      SpatialDesignSystem.primaryColor,
                      SpatialDesignSystem.primaryColor.withOpacity(0.6),
                    ],
                    begin: Alignment.bottomCenter,
                    end: Alignment.topCenter,
                  ),
                  width: 16,
                  borderRadius: const BorderRadius.only(
                    topLeft: Radius.circular(4),
                    topRight: Radius.circular(4),
                  ),
                )
              ],
            ),
            BarChartGroupData(
              x: 1,
              barRods: [
                BarChartRodData(
                  toY: 18,
                  gradient: LinearGradient(
                    colors: [
                      SpatialDesignSystem.primaryColor,
                      SpatialDesignSystem.primaryColor.withOpacity(0.6),
                    ],
                    begin: Alignment.bottomCenter,
                    end: Alignment.topCenter,
                  ),
                  width: 16,
                  borderRadius: const BorderRadius.only(
                    topLeft: Radius.circular(4),
                    topRight: Radius.circular(4),
                  ),
                )
              ],
            ),
            BarChartGroupData(
              x: 2,
              barRods: [
                BarChartRodData(
                  toY: 10,
                  gradient: LinearGradient(
                    colors: [
                      SpatialDesignSystem.primaryColor,
                      SpatialDesignSystem.primaryColor.withOpacity(0.6),
                    ],
                    begin: Alignment.bottomCenter,
                    end: Alignment.topCenter,
                  ),
                  width: 16,
                  borderRadius: const BorderRadius.only(
                    topLeft: Radius.circular(4),
                    topRight: Radius.circular(4),
                  ),
                )
              ],
            ),
            BarChartGroupData(
              x: 3,
              barRods: [
                BarChartRodData(
                  toY: 22,
                  gradient: LinearGradient(
                    colors: [
                      SpatialDesignSystem.primaryColor,
                      SpatialDesignSystem.primaryColor.withOpacity(0.6),
                    ],
                    begin: Alignment.bottomCenter,
                    end: Alignment.topCenter,
                  ),
                  width: 16,
                  borderRadius: const BorderRadius.only(
                    topLeft: Radius.circular(4),
                    topRight: Radius.circular(4),
                  ),
                )
              ],
            ),
            BarChartGroupData(
              x: 4,
              barRods: [
                BarChartRodData(
                  toY: 26,
                  gradient: LinearGradient(
                    colors: [
                      SpatialDesignSystem.primaryColor,
                      SpatialDesignSystem.primaryColor.withOpacity(0.6),
                    ],
                    begin: Alignment.bottomCenter,
                    end: Alignment.topCenter,
                  ),
                  width: 16,
                  borderRadius: const BorderRadius.only(
                    topLeft: Radius.circular(4),
                    topRight: Radius.circular(4),
                  ),
                )
              ],
            ),
            BarChartGroupData(
              x: 5,
              barRods: [
                BarChartRodData(
                  toY: 12,
                  gradient: LinearGradient(
                    colors: [
                      SpatialDesignSystem.primaryColor,
                      SpatialDesignSystem.primaryColor.withOpacity(0.6),
                    ],
                    begin: Alignment.bottomCenter,
                    end: Alignment.topCenter,
                  ),
                  width: 16,
                  borderRadius: const BorderRadius.only(
                    topLeft: Radius.circular(4),
                    topRight: Radius.circular(4),
                  ),
                )
              ],
            ),
            BarChartGroupData(
              x: 6,
              barRods: [
                BarChartRodData(
                  toY: 8,
                  gradient: LinearGradient(
                    colors: [
                      SpatialDesignSystem.primaryColor,
                      SpatialDesignSystem.primaryColor.withOpacity(0.6),
                    ],
                    begin: Alignment.bottomCenter,
                    end: Alignment.topCenter,
                  ),
                  width: 16,
                  borderRadius: const BorderRadius.only(
                    topLeft: Radius.circular(4),
                    topRight: Radius.circular(4),
                  ),
                )
              ],
            ),
          ],
          gridData: FlGridData(
            show: true,
            drawVerticalLine: false,
            getDrawingHorizontalLine: (value) {
              return FlLine(
                color: isDark 
                    ? Colors.white.withOpacity(0.1)
                    : Colors.black.withOpacity(0.1),
                strokeWidth: 1,
              );
            },
          ),
        ),
      ),
    );
  }
}
