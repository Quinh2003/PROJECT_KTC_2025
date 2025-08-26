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

  LineChartData mainData() {
    final List<Color> gradientColors = [
      SpatialDesignSystem.primaryColor,
      SpatialDesignSystem.primaryColor.withOpacity(0.5),
    ];
    
    return LineChartData(
      gridData: const FlGridData(
        show: true,
        drawHorizontalLine: true,
        drawVerticalLine: true,
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
            getTitlesWidget: (value, meta) {
              const style = TextStyle(
                fontSize: 12,
                fontWeight: FontWeight.w500,
              );
              
              Widget text;
              switch (value.toInt()) {
                case 0:
                  text = const Text('Mon', style: style);
                  break;
                case 1:
                  text = const Text('Tue', style: style);
                  break;
                case 2:
                  text = const Text('Wed', style: style);
                  break;
                case 3:
                  text = const Text('Thu', style: style);
                  break;
                case 4:
                  text = const Text('Fri', style: style);
                  break;
                case 5:
                  text = const Text('Sat', style: style);
                  break;
                case 6:
                  text = const Text('Sun', style: style);
                  break;
                default:
                  text = const Text('', style: style);
                  break;
              }
              
              return SideTitleWidget(
                axisSide: meta.axisSide,
                space: 8,
                child: text,
              );
            },
          ),
        ),
        leftTitles: AxisTitles(
          sideTitles: SideTitles(
            showTitles: true,
            interval: 10,
            reservedSize: 42,
            getTitlesWidget: (value, meta) {
              TextStyle style = TextStyle(
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
                axisSide: meta.axisSide,
                space: 8,
                child: Text(text, style: style),
              );
            },
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

class DeliveryScatterChart extends StatelessWidget {
  final bool isDark;

  const DeliveryScatterChart({super.key, required this.isDark});

  @override
  Widget build(BuildContext context) {
    return AspectRatio(
      aspectRatio: 1.7,
      child: ScatterChart(
        ScatterChartData(
          scatterSpots: [
            ScatterSpot(
              2, 8,
              color: SpatialDesignSystem.primaryColor,
              radius: 8,
            ),
            ScatterSpot(
              4, 5,
              color: SpatialDesignSystem.accentColor,
              radius: 12,
            ),
            ScatterSpot(
              8, 7,
              color: SpatialDesignSystem.warningColor,
              radius: 6,
            ),
            ScatterSpot(
              5, 3,
              color: SpatialDesignSystem.successColor,
              radius: 10,
            ),
            ScatterSpot(
              7, 9,
              color: SpatialDesignSystem.errorColor,
              radius: 5,
            ),
            ScatterSpot(
              1, 6,
              color: SpatialDesignSystem.primaryColor.withOpacity(0.7),
              radius: 7,
            ),
            ScatterSpot(
              3, 2,
              color: SpatialDesignSystem.accentColor.withOpacity(0.7),
              radius: 9,
            ),
            ScatterSpot(
              6, 8,
              color: SpatialDesignSystem.warningColor.withOpacity(0.7),
              radius: 4,
            ),
            ScatterSpot(
              9, 4,
              color: SpatialDesignSystem.successColor.withOpacity(0.7),
              radius: 11,
            ),
          ],
          minX: 0,
          maxX: 10,
          minY: 0,
          maxY: 10,
          borderData: FlBorderData(
            show: true,
            border: Border.all(
              color: isDark 
                  ? Colors.white.withOpacity(0.2)
                  : Colors.black.withOpacity(0.2),
              width: 1,
            ),
          ),
          gridData: const FlGridData(
            show: true,
            drawHorizontalLine: true,
            drawVerticalLine: true,
            horizontalInterval: 2,
            verticalInterval: 2,
          ),
          titlesData: FlTitlesData(
            show: true,
            leftTitles: AxisTitles(
              sideTitles: SideTitles(
                showTitles: true,
                reservedSize: 30,
                interval: 2,
                getTitlesWidget: (double value, TitleMeta meta) {
                  return Text(
                    value.toInt().toString(),
                    style: TextStyle(
                      color: isDark 
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor,
                      fontSize: 10,
                    ),
                  );
                },
              ),
            ),
            rightTitles: const AxisTitles(
              sideTitles: SideTitles(showTitles: false),
            ),
            bottomTitles: AxisTitles(
              sideTitles: SideTitles(
                showTitles: true,
                reservedSize: 30,
                interval: 2,
                getTitlesWidget: (double value, TitleMeta meta) {
                  return Text(
                    value.toInt().toString(),
                    style: TextStyle(
                      color: isDark 
                          ? SpatialDesignSystem.textDarkSecondaryColor
                          : SpatialDesignSystem.textSecondaryColor,
                      fontSize: 10,
                    ),
                  );
                },
              ),
            ),
            topTitles: const AxisTitles(
              sideTitles: SideTitles(showTitles: false),
            ),
          ),
        ),
      ),
    );
  }
}
