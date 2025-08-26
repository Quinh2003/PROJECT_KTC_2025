import 'package:fl_chart/fl_chart.dart';
import 'package:flutter/material.dart';
import '../../design/spatial_ui.dart';

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

// Tạm thời ẩn đi ScatterChart vì có vấn đề với API trong fl_chart 1.0.0
/*
class DeliveryScatterChart extends StatelessWidget {
  final bool isDark;

  const DeliveryScatterChart({super.key, required this.isDark});

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1.7,
      child: Container(
        padding: const EdgeInsets.all(16),
        child: const Center(
          child: Text(
            'Scatter Chart không khả dụng trong phiên bản hiện tại',
            textAlign: TextAlign.center,
          ),
        ),
      ),
    );
  }
}
*/

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
